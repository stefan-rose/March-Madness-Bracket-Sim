#script for creation of a March Madness bracket model 
credentials::set_github_pat("general_pat")

# Load required packages
pacman::p_load(tidyverse,hoopR,ncaahoopR,bigballR,mRchmadness)

data_dir <- "" #input local path here

#load Ken Pomeroy ratings for 2025 
kp_df <- read_csv(paste0(data_dir,"Pomeroy_CBB_Ratings_25.csv"))

# Official 2025 Region Structure from NCAA
region_assignments <- tribble(
  ~Seed, ~Region, ~Team,
  # South Region (Atlanta) -----------------------------------------------
  1,  "South", "Auburn",
  2,  "South", "Michigan St.",
  3,  "South", "Iowa St.",
  4,  "South", "Texas A&M",
  5,  "South", "Michigan",
  6,  "South", "Ole Miss",
  7,  "South", "Marquette",
  8,  "South", "Louisville",
  9,  "South", "Creighton",
  10, "South", "New Mexico",
  11, "South", "North Carolina", 
  12, "South", "UC San Diego",
  13, "South", "Yale",
  14, "South", "Lipscomb",
  15, "South", "Bryant",
  16, "South", "Alabama St.", 
  
  # East Region (Newark) -------------------------------------------------
  1,  "East", "Duke",
  2,  "East", "Alabama",
  3,  "East", "Wisconsin",
  4,  "East", "Arizona",
  5,  "East", "Oregon",
  6,  "East", "BYU",
  7,  "East", "Saint Mary's",
  8,  "East", "Mississippi St.",
  9,  "East", "Baylor",
  10, "East", "Vanderbilt",
  11, "East", "VCU", 
  12, "East", "Liberty",
  13, "East", "Akron",
  14, "East", "Montana",
  15, "East", "Robert Morris",
  16, "East", "American", 
  
  # Midwest Region (Indianapolis) -----------------------------------------
  1,  "Midwest", "Houston",
  2,  "Midwest", "Tennessee",
  3,  "Midwest", "Kentucky",
  4,  "Midwest", "Purdue",
  5,  "Midwest", "Clemson",
  6,  "Midwest", "Illinois",
  7,  "Midwest", "UCLA",
  8,  "Midwest", "Gonzaga",
  9,  "Midwest", "Georgia",
  10, "Midwest", "Utah St.",
  11, "Midwest", "Xavier", 
  12, "Midwest", "McNeese",
  13, "Midwest", "High Point",
  14, "Midwest", "Troy",
  15, "Midwest", "Wofford",
  16, "Midwest", "SIUE",
  
  # West Region (San Francisco) -------------------------------------------
  1,  "West", "Florida",
  2,  "West", "St. John's",
  3,  "West", "Texas Tech",
  4,  "West", "Maryland",
  5,  "West", "Memphis",
  6,  "West", "Missouri",
  7,  "West", "Kansas",
  8,  "West", "UConn",
  9,  "West", "Oklahoma",
  10, "West", "Arkansas",
  11, "West", "Drake", 
  12, "West", "Colorado St.",
  13, "West", "Grand Canyon",
  14, "West", "UNC Wilmington",
  15, "West", "Nebraska Omaha",
  16, "West", "Norfolk St."
)

# Merge with team data ---------------------------------------------------
final_mapping <- teams.men %>%
  mutate(
    clean_id = map_chr(id, ~str_split(.x, "/")[[1]][1]),
    Team = map_chr(clean_id, ~filter(teams.men, id == .x)$name)
  ) %>%
  right_join(region_assignments, by = "Team") %>%
  select(Team, Seed, Region, id) %>%
  arrange(Region, Seed) %>% 
  left_join(kp_df, by=c("Team")) %>% 
  mutate(id = case_when(Team =="Michigan St."~"127",
                        Team =="Iowa St."~"66",
                        Team =="St. John's"~"2599",
                        Team =="UC San Diego"~"28",
                        Team == "Norfolk St."~"2450",
                        Team == "Utah St."~"328",
                        Team=="North Carolina"~"153",
                        Team=="Colorado St."~"36",
                        Team=="SIUE"~"2565",
                        Team=="American"~"44",
                        Team=="Mississippi St."~"344",
                        Team=="Nebraska Omaha"~"2437",
                        Team=="Alabama St."~"2011",TRUE~id),
         Conf_tournament_winner=if_else(is.na(Conf_tournament_winner),FALSE,TRUE),
         id=str_trim(id))

# Clean column names and extract metrics

ranked_wins_by_team<-
  hoopR::espn_mbb_standings()%>% mutate(team_id=as.character(team_id)) %>% 
  mutate(team_id=str_trim(team_id))

season_box_stats <- hoopR::load_mbb_team_box() %>% mutate(team_id=as.character(team_id)) %>% 
  mutate(team_id=str_trim(team_id))


filtered_teams <- final_mapping %>% 
  left_join(season_box_stats,by=c("id"="team_id")) 

# Calculate seasonal stats
seasonal_stats <- filtered_teams %>%
  group_by(Team,id) %>%
  summarise(
    turnover_percentage = sum(turnovers) / sum(field_goals_attempted + 0.44 * free_throws_attempted + turnovers) * 100,
    free_throw_percentage = sum(free_throws_made) / sum(free_throws_attempted) * 100,
    three_point_field_goal_pct = sum(three_point_field_goals_made) / sum(three_point_field_goals_attempted) *100,
    avg_Off_rebounds_pg = mean(offensive_rebounds,na.rm = T),
    avg_def_rebounds_pg = mean(defensive_rebounds,na.rm = T),
    total_games = n()
  ) %>%
  # Calculate win percentage in last 10 games
  left_join(
    filtered_teams %>%
      group_by(Team,id) %>%
      arrange(desc(game_date)) %>%
      slice_head(n = 10) %>%
      summarise(
        last_10_wins = sum(team_score > opponent_team_score),
        last_10_win_percentage = last_10_wins / 10 * 100
      )#,
   # by = c("id","Team")
  ) %>% 
  right_join(final_mapping) %>% 
  left_join(
    ranked_wins_by_team,by=c("id"="team_id")
  )

# Create Model Dataset ------------------------------------------------

model_data <- seasonal_stats %>% 
  mutate(
    seed = as.numeric(str_extract(Seed, "\\d+")),
    upset_potential = (vsaprankedteams_winpercent*100 + last_10_wins) #(1 - seed/max(seed)) * 
  ) %>%
  filter(!is.na(seed)) 

model_data <- model_data %>%
  mutate(
    tournament_success = ((17 - Seed) / 16)-0.05,  
    road_winpercent = road_winpercent*100,
    threshold = (Net_Rtng + 100) / 200,
    win = ifelse(tournament_success > threshold, 1, 0)
  )

# Prediction Model -----------------------------------------------------

  win_model <- glm(
    formula = win ~  turnover_percentage + free_throw_percentage + three_point_field_goal_pct +upset_potential+
      last_10_win_percentage + Adj_Tempo + Avg_Opp_Adj_DefE + road_winpercent, #upset_potential
    data = model_data, na.action = na.omit,
    family = binomial(link = "logit")
  )
  
  summary(win_model)
  
  
  # Simulation Function -------------------------------------------------
  
  # Function to simulate first round of matchups
  simulate_matchups <- function(teams_data, model) {
    matchups <- teams_data %>%
      arrange(Region, Seed) %>%
      group_by(Region) %>%
      do({
        region_teams <- .
        # Pairs set up assuming standard seeding rules
        seeds <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
        region_teams <- region_teams %>%
          filter(Seed %in% seeds) %>%
          arrange(match(Seed, seeds))
        
        # Pair the teams, 1 vs 16, 8 vs 9, etc.
        match_pairs <- data.frame(
          Team1 = region_teams$Team[c(TRUE, FALSE)],
          Team2 = region_teams$Team[c(FALSE, TRUE)],
          Seed1 = region_teams$Seed[c(TRUE, FALSE)],
          Seed2 = region_teams$Seed[c(FALSE, TRUE)]
        )
        
        # Predict outcomes
        match_pairs <- match_pairs %>%
          rowwise() %>%
          mutate(
            Prob_Team1_Wins = predict(model, newdata = region_teams %>% filter(Team == Team1), type = "response"),
            Prob_Team2_Wins = predict(model, newdata = region_teams %>% filter(Team == Team2), type = "response"),
            Winner = ifelse(runif(1) < Prob_Team1_Wins, Team1, Team2)
          )
      }) %>%
      select(Region, Team1, Team2, Seed1, Seed2, Winner)
    
    return(matchups)
  }
  
  # Simulate the first round of bracket
  first_round_results <- simulate_matchups(model_data, win_model)
  
  # Print matchups and winners
  print(first_round_results)
  
 
  # Function to simulate the tournament outcome
  simulate_tournament <- function(teams_data, model) {
    teams_progress <- teams_data %>%
      mutate(progress = "Round of 64")
    
    current_round <- teams_data %>%
      arrange(Region, Seed) %>%
      group_by(Region) %>%
      group_modify(~ {
        region_teams <- .x
        seeds <- c(1, 16, 8, 9, 5, 12, 4, 13, 6, 11, 3, 14, 7, 10, 2, 15)
        paired_teams <- region_teams %>%
          filter(Seed %in% seeds) %>%
          arrange(match(Seed, seeds))
        
        data.frame(
          Team1 = paired_teams$Team[c(TRUE, FALSE)],
          Team2 = paired_teams$Team[c(FALSE, TRUE)],
          Seed1 = paired_teams$Seed[c(TRUE, FALSE)],
          Seed2 = paired_teams$Seed[c(FALSE, TRUE)]
        ) %>%
          rowwise() %>%
          mutate(
            Prob_Team1_Wins = predict(model, newdata = paired_teams %>% filter(Team == Team1), type = "response"),
            Prob_Team2_Wins = predict(model, newdata = paired_teams %>% filter(Team == Team2), type = "response"),
            Winner = ifelse(runif(1) < Prob_Team1_Wins, Team1, Team2),
            WinnerSeed = ifelse(Winner == Team1, Seed1, Seed2)
          )
      }) %>%
      ungroup() %>%
      select(Region, Winner, WinnerSeed)
    
    round_names <- c("Round of 32", "Sweet 16", "Elite 8", "Final Four", "Championship")
    round_count <- 0
    
    while (length(unique(current_round$Region)) > 0) {
      round_count <- round_count + 1
      if (round_count > length(round_names)) break
      
      teams_progress <- teams_progress %>%
        mutate(progress = ifelse(
          Team %in% current_round$Winner,
          round_names[min(round_count, length(round_names))],
          progress
        ))
      
      current_round <- current_round %>%
        arrange(Region, WinnerSeed)
      
      current_round <- current_round %>%
        group_by(Region) %>%
        group_modify(~ {
          if (nrow(.x) < 2) return(.x)
          
          paired_teams <- .x %>%
            arrange(WinnerSeed)
          
          data.frame(
            Team1 = paired_teams$Winner[c(TRUE, FALSE)],
            Team2 = paired_teams$Winner[c(FALSE, TRUE)],
            Seed1 = paired_teams$WinnerSeed[c(TRUE, FALSE)],
            Seed2 = paired_teams$WinnerSeed[c(FALSE, TRUE)]
          ) %>%
            rowwise() %>%
            mutate(
              Prob_Team1_Wins = predict(model, newdata = teams_data %>% filter(Team == Team1), type = "response"),
              Prob_Team2_Wins = predict(model, newdata = teams_data %>% filter(Team == Team2), type = "response"),
              Winner = ifelse(runif(1) < Prob_Team1_Wins, Team1, Team2),
              WinnerSeed = ifelse(Winner == Team1, Seed1, Seed2)
            )
        }) %>%
        ungroup() %>%
        select(Region, Winner, WinnerSeed)
    }
    
    final_winners <- current_round$Winner
    teams_progress <- teams_progress %>%
      mutate(progress = ifelse(Team %in% final_winners, "Championship", progress))
    
    return(teams_progress %>% select(Team, Seed, progress))
  }
  
  # Run multiple tournament simulations
  run_multiple_simulations <- function(num_simulations, teams_data, model) {
    results <- bind_rows(lapply(1:num_simulations, function(x) {
      cat("Running simulation:", x, "\n")
      simulate_tournament(teams_data, model) %>%
        mutate(simulation = x)
    }))
    
    return(results)
  }
  
  # running 1000 simulations
  set.seed(2025)
  num_simulations <- 1000
  simulation_results <- run_multiple_simulations(num_simulations, model_data, win_model)
  
  # Process results to show frequency of progress outcomes by team
  team_progress_summary <- simulation_results %>%
    group_by(Team, Seed, progress) %>%
    summarize(
      count = n(),
      .groups = 'drop'
    ) %>%
    arrange(Team, desc(count))
  
  # Print the summarized results
  print(team_progress_summary)
  
  # Create the plot
  ggplot(team_progress_summary, aes(x = reorder(Team, -progress), y = count, fill = progress)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Tournament Progression of Teams Across Simulations",
         x = "Teams",
         y = "Number of Simulations Reaching Each Round",
         fill = "Progression Round") +
    scale_fill_brewer(palette = "Set3") +  # Use a color palette for better distinction
    theme_minimal(base_size = 12) +
    coord_flip()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Tilt team names for readability

  
  team_progress_summary <- team_progress_summary %>%
    mutate(
      progress = ifelse(progress == "Championship", "Final Four", progress),
      progress = factor(progress, levels = c("Round of 64", "Round of 32", "Sweet 16", "Elite 8", "Final Four")),
      Team = factor(Team, levels = sort(unique(Team), decreasing = TRUE))
    )
  
  # Create the scatter plot with alphabetical ordering of teams
  ggplot(team_progress_summary, aes(x = progress, y = Team, size = count, color = progress)) +
    geom_point(alpha = 0.7) +
    scale_size_continuous(range = c(1, 10), guide = guide_legend(title = "Simulations")) +
    labs(title = "Scatter Plot of Tournament Progressions per Team",
         x = "Tournament Round",
         color = "Round",
         y = "Team") +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal() +
    guides(color="none")+
    theme(axis.text.y = element_text(size = 10)) 
  
  # Determine the most frequent progress category for each team
  max_progress_summary <- team_progress_summary %>%
    group_by(Team) %>%
    slice(which.max(count)) %>%
    ungroup()
  
  # Create the lollipop chart
  ggplot(max_progress_summary, aes(x = Team, y = progress, color = progress)) +
    geom_segment(aes(x = Team, xend = Team, y = "Round of 64", yend = progress,size=1.2), lineend = "round") +
    geom_point(aes(size = count)) +
    labs(title = "Lollipop Chart of Most Frequent Tournament Progress for Each Team",
         x = "Team",
         y = "Most Frequent Progression Round",
         size = "Count",
         color = "Progression Round") +
    scale_size_continuous(range = c(1, 5), guide = guide_legend(title = "Simulation Count")) +
    scale_color_brewer(palette = "Dark2") +
    theme_minimal() +
    coord_flip() +
    theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 11),
          axis.text.y = element_text(angle = 0, hjust = 1, size = 9),
          legend.position = "none")

