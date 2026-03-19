library(tidyverse)
library(hoopR)

data_dir <- '/Users/rose501/Library/CloudStorage/OneDrive-PNNL/Desktop/Projects/Sandbox/March Madness/March-Madness-Bracket-Sim/' #input local path here

# ---- 1. Load Data ----
kp <- read_csv(paste0(data_dir,"Pomeroy_CBB_Ratings_26.csv")) %>% 
  mutate(Team = str_trim(Team))

name_map <- tribble(
  ~Team, ~espn_id,
  "Duke","150","Michigan","130","Arizona","12","Florida","57",
  "Houston","248","Iowa St.","66","Illinois","356","Purdue","2509",
  "Michigan St.","127","Gonzaga","2250","Connecticut","41",
  "Vanderbilt","238","Virginia","258","Nebraska","158",
  "Arkansas","8","Tennessee","2633","St. John's","2599",
  "Alabama","333","Louisville","97","Texas Tech","2641",
  "Kansas","2305","Wisconsin","275","BYU","252",
  "Saint Mary's","2608","Iowa","2294","Ohio St.","194",
  "UCLA","26","Kentucky","96","North Carolina","153",
  "Utah St.","328","Miami FL","2390","Georgia","61",
  "Villanova","222","N.C. State","152","Santa Clara","2541",
  "Clemson","228","Texas","251","Texas A&M","245",
  "Saint Louis","139","SMU","2567","TCU","2628",
  "VCU","2670","South Florida","58","UCF","2116",
  "Missouri","142","Northern Iowa","2460","Cal Baptist","2856",
  "North Dakota St.","2449","McNeese","2377","Troy","2653",
  "Penn","219","Idaho","70","Tennessee St.","2634",
  "Wright St.","2750","Hofstra","2275","Akron","2006",
  "Queens","3077","High Point","2272","Hawaii","62",
  "LIU","2344","Miami OH","193","UMBC","2692",
  "Siena","2561","Howard","47","Lehigh","2329",
  "Prairie View A&M","2504","Furman","231","Kennesaw St.","2320"
)

# ---- 2. ESPN data ----
standings <- espn_mbb_standings() %>%
  mutate(espn_id = str_trim(as.character(team_id))) %>%
  select(espn_id, vsaprankedteams_winpercent, road_winpercent) %>%
  distinct(espn_id, .keep_all = TRUE)

box <- load_mbb_team_box(seasons = 2025) %>%
  mutate(espn_id = str_trim(as.character(team_id)))

seasonal <- box %>%
  inner_join(name_map, by = "espn_id") %>%
  group_by(Team) %>%
  summarise(
    turnover_pct = sum(turnovers, na.rm=T) /
      sum(field_goals_attempted + 0.44*free_throws_attempted + turnovers, na.rm=T) * 100,
    ft_pct = sum(free_throws_made, na.rm=T) / sum(free_throws_attempted, na.rm=T) * 100,
    three_pct = sum(three_point_field_goals_made, na.rm=T) /
      sum(three_point_field_goals_attempted, na.rm=T) * 100,
    avg_off_reb = mean(offensive_rebounds, na.rm=T),
    avg_def_reb = mean(defensive_rebounds, na.rm=T),
    .groups = "drop"
  )

last10 <- box %>%
  inner_join(name_map, by = "espn_id") %>%
  group_by(Team) %>% arrange(desc(game_date)) %>% slice_head(n = 10) %>%
  summarise(last10_wins = sum(team_score > opponent_team_score, na.rm=T),
            last10_pct = last10_wins / 10 * 100, .groups = "drop")

# ---- 3. Merge ----
model_data <- kp %>%
  left_join(name_map, by = "Team") %>%
  left_join(seasonal, by = "Team") %>%
  left_join(last10, by = "Team") %>%
  left_join(standings, by = "espn_id") %>%
  mutate(
    road_winpct    = replace_na(road_winpercent, 0.4) * 100,
    ranked_winpct  = replace_na(vsaprankedteams_winpercent, 0.2) * 100,
    upset_potential = ranked_winpct + replace_na(last10_wins, 5),
    across(c(turnover_pct, ft_pct, three_pct, last10_pct, avg_off_reb, avg_def_reb),
           ~replace_na(.x, median(.x, na.rm = TRUE)))
  )

# ---- Add health penalty to model_data  ----
model_data <- model_data %>%
  mutate(
    # Negative = hurt, scale 0 to -10
    health = case_when(
      Team == "Duke"       ~ -5,   # major injury (key player)
   #   Team == "Michigan"   ~ -2,   # major injury (key player)
      Team == "Alabama"    ~ -6,   # major player suspended
      Team == "Connecticut"~ -4,   # moderate concern
      Team == "SMU"        ~ -4,
      Team == "Texas Tech" ~ -7,   #major player injury
      Team == "Clemson"    ~ -3,
      Team == "BYU"        ~ -3,
      Team == "Siena"      ~ -3,
      Team == "North Carolina" ~ -4,
      TRUE ~ 0
    )
  )

# Regional strength
region_strength <- model_data %>%
  filter(tournament_seed <= 8) %>%
  group_by(Region) %>%
  summarise(region_power = mean(Net_Rtng, na.rm=T), .groups="drop") %>%
  mutate(region_adj = -(region_power - mean(region_power)) * 0.5)
model_data <- model_data %>% left_join(region_strength, by = "Region")

# ---- 4. First Four ----
resolve_first_four <- function(df) {
  ff     <- df %>% filter(first_four %in% TRUE)
  non_ff <- df %>% filter(!first_four %in% TRUE)
  # Use actual first four winners
  ff_winners <- ff %>% filter(first_four_win %in% TRUE)
  bind_rows(non_ff, ff_winners)
}

# ---- 5. HEAD-TO-HEAD win probability (stats-driven) ----
# ---- Define weights globally ----
W <- list(
  rtg = 0.35, off = 1.1, def = 0.85, to = 2.7,
  ft = 1.0, three = 1.4, reb = 1.2, upset = 0.08,
  momentum = 0.12, road = 0.08,
  health = 1.2   
)

# ---- Use in h2h_win_prob ----
h2h_win_prob <- function(t1, t2, round_num = 1) {
  off_diff      <- (t1$Adj_OffE - t2$Adj_DefE) - (t2$Adj_OffE - t1$Adj_DefE)
  to_diff       <- -(t1$turnover_pct - t2$turnover_pct)
  ft_diff       <- t1$ft_pct - t2$ft_pct
  three_diff    <- t1$three_pct - t2$three_pct
  reb_diff      <- (t1$avg_off_reb + t1$avg_def_reb) - (t2$avg_off_reb + t2$avg_def_reb)
  upset_diff    <- t1$upset_potential - t2$upset_potential
  momentum_diff <- t1$last10_pct - t2$last10_pct
  road_diff     <- t1$road_winpct - t2$road_winpct
  rtg_diff      <- (t1$Net_Rtng - t1$Luck_rting*25 + t1$region_adj) -
    (t2$Net_Rtng - t2$Luck_rting*25 + t2$region_adj)
  health_diff   <- t1$health - t2$health
    def_diff      <- t2$Adj_DefE - t1$Adj_DefE  # positive = T1 has better defense
  
  composite <-
    rtg_diff      * W$rtg +
    off_diff      * W$off +
    to_diff       * W$to  +
    ft_diff       * W$ft  +
    three_diff    * W$three +
    reb_diff      * W$reb +
    upset_diff    * W$upset +
    momentum_diff * W$momentum +
    road_diff     * W$road +
    health_diff   * W$health +
    def_diff      * W$def       
  
  sigma <- 14 - (round_num - 1) * 0.5
  1 / (1 + 10^(-composite / sigma))
}

# ---- 6. Play round using h2h comparison ----
play_round <- function(df, all_data, round_num = 1) {
  df %>%
    mutate(pair = ceiling(row_number() / 2), .by = Region) %>%
    reframe(T1 = Team[1], S1 = Seed[1],
            T2 = Team[2], S2 = Seed[2],
            .by = c(Region, pair)) %>%
    rowwise() %>%
    mutate(
      p = {
        t1 <- all_data %>% filter(Team == T1) %>% slice(1)
        t2 <- all_data %>% filter(Team == T2) %>% slice(1)
        h2h_win_prob(t1, t2, round_num)
      },
      w = runif(1) < p,
      Team = ifelse(w, T1, T2),
      Seed = ifelse(w, S1, S2)
    ) %>%
    ungroup() %>%
    select(Region, Team, Seed)
}

# ---- 7. Full tournament ----
simulate_tournament <- function(bracket) {
  seed_order <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  
  # Per-tournament noise: randomly perturb key stats (teams run hot/cold)
  noisy <- bracket %>% mutate(
    Net_Rtng     = Net_Rtng + rt(n(), df=4) * 5,
    Adj_OffE     = Adj_OffE + rt(n(), df=5) * 3,
    Adj_DefE     = Adj_DefE + rt(n(), df=5) * 3,
    turnover_pct = turnover_pct + rnorm(n(), 0, 2),
    ft_pct       = ft_pct + rnorm(n(), 0, 3),
    three_pct    = three_pct + rnorm(n(), 0, 3),
    upset_potential = upset_potential + rnorm(n(), 0, 5)
  )
  
  current <- noisy %>%
    arrange(Region, match(tournament_seed, seed_order)) %>%
    transmute(Region, Team, Seed = tournament_seed)
  
  results <- list()
  for (i in 1:4) {
    current <- play_round(current, noisy, round_num = i)
    results[[c("R64","R32","S16","E8")[i]]] <- current$Team
  }
  
  # Final Four
  ff <- current %>% arrange(match(Region, c("East","West","South","Midwest")))
  ff_pairs <- tibble(
    T1 = ff$Team[c(1,3)], S1 = ff$Seed[c(1,3)],
    T2 = ff$Team[c(2,4)], S2 = ff$Seed[c(2,4)]
  ) %>% rowwise() %>%
    mutate(
      p = {
        t1 <- noisy %>% filter(Team == T1) %>% slice(1)
        t2 <- noisy %>% filter(Team == T2) %>% slice(1)
        h2h_win_prob(t1, t2, 5)
      },
      w = runif(1) < p,
      Team = ifelse(w, T1, T2), Seed = ifelse(w, S1, S2)
    ) %>% ungroup()
  results[["F4"]] <- ff_pairs$Team
  
  # Championship
  t1 <- noisy %>% filter(Team == ff_pairs$Team[1]) %>% slice(1)
  t2 <- noisy %>% filter(Team == ff_pairs$Team[2]) %>% slice(1)
  p <- h2h_win_prob(t1, t2, 6)
  results[["Champ"]] <- ifelse(runif(1) < p, ff_pairs$Team[1], ff_pairs$Team[2])
  results
}

# ---- 8. Monte Carlo ----
set.seed(2025)
N <- 10000

sims <- map(1:N, \(i) {
  resolve_first_four(model_data) %>% simulate_tournament()
}, .progress = TRUE)

# ============================================================
# DIAGNOSTICS: METRIC INFLUENCE ANALYSIS
# ============================================================

# ---- 1. Correlation: team stats vs advancement probability ----
# How far each team advances on average across sims
adv_score <- map_dfr(c("R64","R32","S16","E8","F4","Champ"), \(rnd) {
  summarize_round(rnd) %>% mutate(Round = rnd)
}) %>%
  mutate(round_val = case_when(
    Round == "R64" ~ 1, Round == "R32" ~ 2, Round == "S16" ~ 3,
    Round == "E8" ~ 4, Round == "F4" ~ 5, Round == "Champ" ~ 6
  )) %>%
  group_by(Team) %>%
  summarise(avg_advance = sum(round_val * Count) / N, .groups = "drop")

metric_cors <- model_data %>%
  inner_join(adv_score, by = "Team") %>%
  summarise(across(
    c(Net_Rtng, Adj_OffE, Adj_DefE, turnover_pct, ft_pct, three_pct,
      upset_potential, last10_pct, road_winpct, ranked_winpct,
      Luck_rting, SOS_Net_rtng, Adj_Tempo, avg_off_reb, avg_def_reb),
    ~cor(.x, avg_advance, use = "complete.obs")
  )) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Correlation") %>%
  arrange(desc(abs(Correlation)))

cat("\n=== METRIC CORRELATION WITH ADVANCEMENT ===\n")
print(metric_cors, n = 20)

# ---- 2. H2H weight contribution analysis ----
# Compute average absolute differential per metric across all R64 matchups
seed_order <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
bracket <- model_data %>%
  filter(!first_four %in% TRUE | is.na(first_four)) %>%
  arrange(Region, match(tournament_seed, seed_order))

matchup_diffs <- bracket %>%
  group_by(Region) %>%
  mutate(pair = ceiling(row_number() / 2)) %>%
  group_by(Region, pair) %>%
  filter(n() == 2) %>%
  summarise(
    matchup = paste(Team[1], "vs", Team[2]),
    seed_gap = paste(tournament_seed[1], "v", tournament_seed[2]),
    off_diff     = abs((Adj_OffE[1] - Adj_DefE[2]) - (Adj_OffE[2] - Adj_DefE[1])),
    to_diff      = abs(turnover_pct[1] - turnover_pct[2]),
    ft_diff      = abs(ft_pct[1] - ft_pct[2]),
    three_diff   = abs(three_pct[1] - three_pct[2]),
    reb_diff     = abs((avg_off_reb[1]+avg_def_reb[1]) - (avg_off_reb[2]+avg_def_reb[2])),
    upset_diff   = abs(upset_potential[1] - upset_potential[2]),
    momentum_diff= abs(last10_pct[1] - last10_pct[2]),
    road_diff    = abs(road_winpct[1] - road_winpct[2]),
    rtg_diff     = abs((Net_Rtng[1] - Luck_rting[1]*25) - (Net_Rtng[2] - Luck_rting[2]*25)),
    health_diff  = abs(health[1] - health[2]),
    def_diff     = abs(Adj_DefE[1] - Adj_DefE[2]),  # NEW
    .groups = "drop"
  )

# Weight * avg differential = effective influence
weights <- tibble(
  Metric = c("off_diff","to_diff","ft_diff","three_diff","reb_diff",
             "upset_diff","momentum_diff","road_diff","rtg_diff","health_diff","def_diff"),
  Weight = c(W$off, W$to, W$ft, W$three, W$reb, W$upset, W$momentum, W$road, W$rtg, W$health, W$def),
  Label  = c("Matchup Offense","Turnovers","Free Throws","3PT Shooting",
             "Rebounding","Upset Potential","Momentum","Road Toughness","Net Rating","Health","Defense")
)

influence <- matchup_diffs %>%
  summarise(across(off_diff:def_diff, mean)) %>%
  pivot_longer(everything(), names_to = "Metric", values_to = "Avg_Diff") %>%
  inner_join(weights, by = "Metric") %>%
  mutate(
    Effective_Influence = round(Avg_Diff * Weight, 2),
    Pct_Influence = round(Effective_Influence / sum(Effective_Influence) * 100, 1)
  ) %>%
  arrange(desc(Pct_Influence)) %>%
  select(Label, Weight, Avg_Diff = Avg_Diff, Influence = Effective_Influence, `Pct%` = Pct_Influence)

cat("\n=== METRIC INFLUENCE ON WIN PROBABILITY ===\n")
cat("(Weight × Avg Differential across R64 matchups)\n\n")
print(influence, n = 10)

# ---- 3. Biggest upset drivers: which metrics favor underdogs? ----
cat("\n=== UNDERDOG EDGE METRICS (seed 9+ vs seed 1-8) ===\n")
underdog_edge <- bracket %>%
  group_by(Region) %>%
  mutate(pair = ceiling(row_number() / 2)) %>%
  group_by(Region, pair) %>%
  filter(n() == 2) %>%
  summarise(
    fav = Team[which.min(tournament_seed)],
    dog = Team[which.max(tournament_seed)],
    fav_seed = min(tournament_seed), dog_seed = max(tournament_seed),
    dog_to_edge     = turnover_pct[which.min(tournament_seed)] - turnover_pct[which.max(tournament_seed)],
    dog_ft_edge     = ft_pct[which.max(tournament_seed)] - ft_pct[which.min(tournament_seed)],
    dog_3pt_edge    = three_pct[which.max(tournament_seed)] - three_pct[which.min(tournament_seed)],
    dog_momentum    = last10_pct[which.max(tournament_seed)] - last10_pct[which.min(tournament_seed)],
    dog_upset_pot   = upset_potential[which.max(tournament_seed)] - upset_potential[which.min(tournament_seed)],
    dog_health_edge = health[which.max(tournament_seed)] - health[which.min(tournament_seed)],
    .groups = "drop"
  ) %>%
  filter(dog_seed >= 9) %>%
  mutate(total_dog_edge = dog_to_edge + dog_ft_edge*W$ft + dog_3pt_edge*W$three + 
           dog_momentum*W$momentum + dog_upset_pot*W$upset + dog_health_edge*W$health) %>%
  arrange(desc(total_dog_edge))

print(underdog_edge, n = 20)

# ---- 4. Sensitivity: re-run 1K sims zeroing out each metric ----
cat("\n=== SENSITIVITY ANALYSIS (1K sims each) ===\n")
cat("Zeroing out each metric and measuring change in avg 1-seed F4 rate\n\n")

# Baseline 1-seed F4 rate from full run
baseline_1seed <- f4_seeds["1"] / sum(f4_seeds) * 100

sensitivity_results <- map_dfr(
  c("turnover_pct", "ft_pct", "three_pct", "upset_potential",
    "last10_pct", "road_winpct", "ranked_winpct"), \(metric) {
      
      nulled_data <- model_data
      # Set metric to global mean (neutralize it)
      nulled_data[[metric]] <- mean(nulled_data[[metric]], na.rm = TRUE)
      
      null_sims <- map(1:1000, \(i) {
        resolve_first_four(nulled_data) %>% simulate_tournament()
      })
      
      null_f4 <- map(null_sims, \(s) {
        kp %>% filter(Team %in% s[["F4"]]) %>% pull(tournament_seed)
      }) %>% unlist() %>% table()
      
      null_1seed <- ifelse("1" %in% names(null_f4), null_f4["1"] / sum(null_f4) * 100, 0)
      
      tibble(
        Metric = metric,
        `1seed_F4%_baseline` = round(as.numeric(baseline_1seed), 1),
        `1seed_F4%_nulled` = round(as.numeric(null_1seed), 1),
        `Change` = round(as.numeric(null_1seed - baseline_1seed), 1)
      )
    })

print(sensitivity_results, n = 10)
cat("\nPositive change = removing metric HELPS 1-seeds (metric was causing upsets)")
cat("\nNegative change = removing metric HURTS 1-seeds (metric was protecting them)\n")

# ---- 9. Summarize ----
summarize_round <- function(rnd) {
  map(sims, \(s) s[[rnd]]) %>% unlist() %>% table() %>% sort(decreasing=TRUE) %>%
    {tibble(Team=names(.), Count=as.integer(.), Pct=round(as.integer(.)/N*100,1))}
}

for (rnd in c("Champ","F4","E8","S16")) {
  cat(sprintf("\n=== %s ===\n", rnd))
  print(summarize_round(rnd) %>%
          left_join(kp %>% distinct(Team, tournament_seed), by="Team") %>% head(25), n=25)
}

cat("\n=== SEEDS 10+ IN SWEET 16 ===\n")
print(summarize_round("S16") %>%
        left_join(kp %>% distinct(Team, tournament_seed), by="Team") %>%
        filter(tournament_seed >= 10) %>% arrange(desc(Pct)), n=20)

cat("\n=== F4 SEED DISTRIBUTION ===\n")
f4_seeds <- map(sims, \(s) {
  kp %>% filter(Team %in% s[["F4"]]) %>% pull(tournament_seed)
}) %>% unlist() %>% table()
model_f4 <- round(f4_seeds / sum(f4_seeds) * 100, 1)
hist_f4 <- c("1"=30,"2"=16,"3"=11,"4"=9,"5"=7,"6"=6,"7"=4,"8"=4,
             "9"=3,"10"=3,"11"=3,"12"=2,"13"=1,"14"=0.5,"15"=0.3,"16"=0.2)
tibble(Seed=1:16,
       Historical=hist_f4[as.character(1:16)],
       Model=as.numeric(model_f4[as.character(1:16)])) %>%
  mutate(Model=replace_na(Model,0)) %>% print(n=16)

# Diagnose the specific matchup stats
cat("\n=== MIDWEST TOP SEED PROFILES ===\n")
model_data %>%
  filter(Region == "Midwest", tournament_seed <= 8) %>%
  arrange(tournament_seed) %>%
  select(Team, tournament_seed, Net_Rtng, Adj_OffE, Adj_DefE, 
         turnover_pct, ft_pct, three_pct, last10_pct, 
         upset_potential, health) %>%
  print(n = 8, width = Inf)

# ---- 10. OPTIMAL BRACKET ----
round_probs <- map_dfr(c("R64","R32","S16","E8","F4","Champ"), \(rnd) {
  summarize_round(rnd) %>%
    left_join(kp %>% distinct(Team, tournament_seed, Region), by="Team") %>%
    mutate(Round = rnd)
})

# ---- Midwest F4 full distribution ----
cat("\n=== MIDWEST FULL F4 PROBABILITIES ===\n")
round_probs %>%
  filter(Round == "F4", Region == "Midwest") %>%
  left_join(kp %>% distinct(Team, tournament_seed, Net_Rtng, Adj_DefE, Adj_OffE), by = "Team") %>%
  left_join(model_data %>% distinct(Team, health), by = "Team") %>%
  arrange(desc(Pct)) %>%
  print(n = 15)

# ============================================================
# OPTIMAL BRACKET (bracket-path-aware)
# ============================================================

build_optimal_bracket <- function(resolved_data, round_probs) {
  seed_order <- c(1,16,8,9,5,12,4,13,6,11,3,14,7,10,2,15)
  
  get_pct <- function(team, rnd) {
    p <- round_probs %>% filter(Team == team, Round == rnd) %>% pull(Pct)
    if (length(p) == 0) 0 else p
  }
  
  current <- resolved_data %>%
    arrange(Region, match(tournament_seed, seed_order)) %>%
    transmute(Region, Team, Seed = tournament_seed)
  
  bracket <- list()
  
  # R64→R32, R32→S16, S16→E8, E8→F4 (4 rounds, not 3)
  lookup_round <- c("R32","S16","E8","F4")
  for (i in 1:4) {
    rnd <- lookup_round[i]
    paired <- current %>%
      mutate(pair = ceiling(row_number() / 2), .by = Region)
    current <- paired %>%
      group_by(Region, pair) %>%
      mutate(adv = map_dbl(Team, ~get_pct(.x, rnd))) %>%
      slice_max(adv, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(Region, Team, Seed)
    bracket[[rnd]] <- current
  }
  
  # FINAL FOUR: now 4 teams (1 per region)
  ff <- current %>% arrange(match(Region, c("East","West","South","Midwest")))
  bracket[["F4"]] <- ff %>%
    mutate(F4_Pct = map_dbl(Team, ~get_pct(.x, "F4")))
  
  # SEMIFINALS: East vs West, South vs Midwest
  semis <- tibble(
    Game = c("Semi 1: East vs West", "Semi 2: South vs Midwest"),
    T1 = ff$Team[c(1,2)], S1 = ff$Seed[c(1,2)], R1 = ff$Region[c(1,2)],
    T2 = ff$Team[c(3,4)], S2 = ff$Seed[c(3,4)], R2 = ff$Region[c(3,4)]
  ) %>% rowwise() %>%
    mutate(
      p1 = get_pct(T1, "Champ"), p2 = get_pct(T2, "Champ"),
      Winner = ifelse(p1 >= p2, T1, T2),
      Winner_Seed = ifelse(p1 >= p2, S1, S2),
      Winner_Region = ifelse(p1 >= p2, R1, R2)
    ) %>% ungroup()
  bracket[["Semis"]] <- semis
  
  # CHAMPIONSHIP
  p1 <- get_pct(semis$Winner[1], "Champ")
  p2 <- get_pct(semis$Winner[2], "Champ")
  bracket[["Championship"]] <- tibble(
    Game = "Championship",
    T1 = semis$Winner[1], S1 = semis$Winner_Seed[1],
    T2 = semis$Winner[2], S2 = semis$Winner_Seed[2],
    Champion = ifelse(p1 >= p2, semis$Winner[1], semis$Winner[2]),
    Champ_Seed = ifelse(p1 >= p2, semis$Winner_Seed[1], semis$Winner_Seed[2]),
    Champ_Pct = max(p1, p2)
  )
  
  bracket
}

# Build and print
resolved <- resolve_first_four(model_data)
optimal <- build_optimal_bracket(resolved, round_probs)

cat("\n========================================\n")
cat("     MODEL OPTIMAL BRACKET\n")
cat("========================================\n")

for (rnd in c("R32","S16","E8")) {
  cat(sprintf("\n--- %s ---\n", rnd))
  optimal[[rnd]] %>%
    left_join(round_probs %>% filter(Round == rnd) %>% select(Team, Pct), by = "Team") %>%
    mutate(Pct = replace_na(Pct, 0)) %>%
    arrange(Region, desc(Pct)) %>%
    print(n = 40)
}

cat("\n--- FINAL FOUR ---\n")
print(optimal[["F4"]] %>% select(Region, Team, Seed, `Adv%` = F4_Pct))

cat("\n--- SEMIFINALS ---\n")
optimal[["Semis"]] %>% select(Game, T1, S1, T2, S2, Winner) %>% print()

cat("\n--- CHAMPIONSHIP ---\n")
optimal[["Championship"]] %>%
  select(T1, S1, T2, S2, Champion, Champ_Seed, `Win%` = Champ_Pct) %>% print()

cat(sprintf("\n CHAMPION: %s (%s-seed) \n",
            optimal[["Championship"]]$Champion,
            optimal[["Championship"]]$Champ_Seed))

library(ggplot2)
library(scales)

# ============================================================
# 1. CHAMPIONSHIP PROBABILITY — Top 20 Teams
# ============================================================
champ_data <- summarize_round("Champ") %>%
  left_join(kp %>% distinct(Team, tournament_seed, Region), by = "Team") %>%
  head(20) %>%
  mutate(Team = fct_reorder(Team, Pct))

ggplot(champ_data, aes(x = Team, y = Pct, fill = factor(tournament_seed))) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Pct, "%")), hjust = -0.15, size = 3.2) +
  coord_flip() +
  scale_fill_viridis_d(option = "turbo", name = "Seed") +
  labs(title = "Championship Win Probability",
       subtitle = paste0("Based on ", format(N, big.mark=","), " Monte Carlo Simulations"),
       x = NULL, y = "Win %") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right")

ggsave("plots/01_championship_probability.png", plot = last_plot(), width = 10, height = 7, dpi = 150)

# ============================================================
# 2. FINAL FOUR SEED DISTRIBUTION vs HISTORICAL
# ============================================================
f4_seeds <- map(sims, \(s) {
  kp %>% filter(Team %in% s[["F4"]]) %>% pull(tournament_seed)
}) %>% unlist() %>% table()
model_f4 <- round(f4_seeds / sum(f4_seeds) * 100, 1)
hist_f4 <- c("1"=30,"2"=16,"3"=11,"4"=9,"5"=7,"6"=6,"7"=4,"8"=4,
             "9"=3,"10"=3,"11"=3,"12"=2,"13"=1,"14"=0.5,"15"=0.3,"16"=0.2)

seed_comp <- tibble(Seed = 1:16,
                    Historical = hist_f4[as.character(1:16)],
                    Model = as.numeric(model_f4[as.character(1:16)])) %>%
  mutate(Model = replace_na(Model, 0)) %>%
  pivot_longer(c(Historical, Model), names_to = "Source", values_to = "Pct")

ggplot(seed_comp, aes(x = factor(Seed), y = Pct, fill = Source)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Historical" = "#4A90D9", "Model" = "#E74C3C")) +
  labs(title = "Final Four Seed Distribution: Model vs Historical",
       x = "Seed", y = "% of Final Four Appearances", fill = NULL) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("plots/02_seed_distribution.png", plot = last_plot(), width = 10, height = 7, dpi = 150)

# ============================================================
# 3. ROUND-BY-ROUND ADVANCEMENT HEATMAP — Top 25 Teams
# ============================================================
heat_data <- map_dfr(c("R32","S16","E8","F4","Champ"), \(rnd) {
  summarize_round(rnd) %>% mutate(Round = rnd)
}) %>%
  left_join(kp %>% distinct(Team, tournament_seed), by = "Team") %>%
  mutate(Round = factor(Round, levels = c("R32","S16","E8","F4","Champ"),
                        labels = c("Round of 32","Sweet 16","Elite 8","Final Four","Champion")))

# Top 25 by championship probability
top_teams <- heat_data %>%
  filter(Round == "Champion") %>%
  slice_max(Pct, n = 25) %>%
  pull(Team)

heat_filtered <- heat_data %>%
  filter(Team %in% top_teams) %>%
  mutate(Team = factor(Team, levels = rev(
    heat_data %>% filter(Round == "Champion", Team %in% top_teams) %>%
      arrange(Pct) %>% pull(Team)
  )))

ggplot(heat_filtered, aes(x = Round, y = Team, fill = Pct)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = ifelse(Pct >= 1, paste0(Pct, "%"),
                               ifelse(Pct > 0, "<1%", ""))),
            size = 2.8, color = "white", fontface = "bold") +
  scale_fill_gradient2(low = "#2C3E50", mid = "#E67E22", high = "#E74C3C",
                       midpoint = 30, name = "Adv %") +
  labs(title = "Round-by-Round Advancement Probability",
       subtitle = "Top 25 Championship Contenders",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid = element_blank())

ggsave("plots/03_advancement_heatmap.png", plot = last_plot(), width = 10, height = 7, dpi = 150)


# ============================================================
# 4. REGIONAL FINAL FOUR RACE — Top 5 per Region
# ============================================================
f4_by_region <- summarize_round("F4") %>%
  left_join(kp %>% distinct(Team, tournament_seed, Region), by = "Team") %>%
  group_by(Region) %>%
  slice_max(Pct, n = 5) %>%
  ungroup() %>%
  mutate(label = paste0(Team, " (", tournament_seed, ")"),
         label = fct_reorder(label, Pct))

ggplot(f4_by_region, aes(x = label, y = Pct, fill = Region)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(Pct, "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  facet_wrap(~Region, scales = "free_y", ncol = 2) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Final Four Contenders by Region",
       subtitle = "Top 5 per region | (seed) shown",
       x = NULL, y = "Probability %") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold", size = 13),
        panel.grid.major.y = element_blank())

ggsave("plots/04_regional_f4_race.png", plot = last_plot(), width = 10, height = 7, dpi = 150)


# ============================================================
# 5. OPTIMAL BRACKET VISUALIZATION
# ============================================================
# Build bracket path data
bracket_viz <- bind_rows(
  optimal[["R32"]] %>% mutate(Round = "R32"),
  optimal[["S16"]] %>% mutate(Round = "S16"),
  optimal[["E8"]]  %>% mutate(Round = "E8"),
  optimal[["F4"]] %>% select(Region, Team, Seed) %>% mutate(Round = "F4"),
  tibble(Region = optimal[["Championship"]]$R1,
         Team = optimal[["Championship"]]$Champion,
         Seed = optimal[["Championship"]]$Champ_Seed,
         Round = "Champ")
) %>%
  left_join(round_probs %>% select(Team, Round, Pct), by = c("Team","Round")) %>%
  mutate(
    Round = factor(Round, levels = c("R32","S16","E8","F4","Champ"),
                   labels = c("Round of 32","Sweet 16","Elite 8","Final Four","Champion")),
    label = paste0(Team, " (", Seed, ")")
  )

# Dot chart showing advancement path
ggplot(bracket_viz, aes(x = Round, y = reorder(label, Seed), 
                        size = Pct, color = Region)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(2, 10), name = "Adv %",
                        breaks = c(10, 30, 50, 70, 90)) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Optimal Bracket Selections",
       subtitle = "Dot size = probability of advancing to that round",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(color = "grey90"))

ggsave("plots/05_optimal_bracket.png", plot = last_plot(), width = 10, height = 7, dpi = 150)


# ============================================================
# 6. UPSET FREQUENCY BY SEED MATCHUP
# ============================================================
r64_upsets <- map_dfr(1:N, \(i) {
  r32 <- sims[[i]][["R32"]]
  kp %>% filter(Team %in% r32) %>%
    select(Team, tournament_seed) %>%
    filter(tournament_seed >= 9)
}) %>%
  count(tournament_seed, name = "total") %>%
  mutate(
    upset_rate = round(total / (N * 4) * 100, 1),  # 4 matchups per seed per tourney
    matchup = paste0(17 - tournament_seed, " vs ", tournament_seed)
  )

ggplot(r64_upsets, aes(x = reorder(matchup, -tournament_seed), y = upset_rate)) +
  geom_col(fill = "#E74C3C", width = 0.6) +
  geom_text(aes(label = paste0(upset_rate, "%")), vjust = -0.5, size = 3.5) +
  labs(title = "Round of 64 Upset Rates",
       subtitle = "% of times lower seed wins across all simulations",
       x = "Matchup", y = "Upset Rate %") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.x = element_blank())

ggsave("plots/06_upset_rates.png", plot = last_plot(), width = 10, height = 7, dpi = 150)


# ============================================================
# 7. METRIC INFLUENCE — Lollipop Chart
# ============================================================
ggplot(influence, aes(x = reorder(Label, `Pct%`), y = `Pct%`)) +
  geom_segment(aes(xend = Label, y = 0, yend = `Pct%`), 
               color = "#2C3E50", linewidth = 1.2) +
  geom_point(aes(size = Weight), color = "#E74C3C", alpha = 0.85) +
  geom_text(aes(label = paste0(`Pct%`, "%")), hjust = -0.4, size = 3.5) +
  coord_flip() +
  scale_size_continuous(range = c(3, 10), name = "Weight") +
  labs(title = "Model Influence Breakdown",
       subtitle = "Share of composite win probability by metric | Dot size = raw weight",
       x = NULL, y = "Influence %") +
  theme_minimal(base_size = 13) +
  theme(panel.grid.major.y = element_blank(),
        legend.position = "right")

ggsave("plots/07_metric_influence.png", plot = last_plot(), width = 10, height = 7, dpi = 150)


# Save all plots to /plots folder
dir.create("plots", showWarnings = FALSE)

ggsave("plots/01_championship_probability.png", plot = last_plot(), width = 10, height = 7, dpi = 150)
# Repeat for each plot, or wrap each ggplot block:

# Example:
p1 <- ggplot(champ_data, ...) + ...
ggsave("plots/01_championship_probability.png", p1, width = 10, height = 7, dpi = 150)

p2 <- ggplot(seed_comp, ...) + ...
ggsave("plots/02_seed_distribution.png", p2, width = 9, height = 6, dpi = 150)