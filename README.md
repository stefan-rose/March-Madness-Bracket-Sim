# March Madness Bracket Model
This R script simulates the NCAA March Madness tournament brackets using a way too simplistic logistic regression model. It leverages team performance metrics and upsets potential to predict match outcomes, running through realistic tournament structures and regional seed setups.

## Main Features:
#### Data: 
Utilizes Ken Pomeroy's ratings and ESPN standings to build a comprehensive dataset of team statistics, including recent performance, tempo, and adjusted efficiency metrics.
Tournament Structure: Replicates the official 2025 NCAA bracket setup, aligning teams by their seeds and regional assignments, ensuring authenticity in simulation layout.
#### Predictive Model:
Logistic Regression: Predicts game outcomes by estimating the probability of winning.
#### Key Factors:
- Turnover and Shooting Efficiency: Considers how effectively teams manage possessions and scoring opportunities.
- Recent Form and Tempo: Includes last 10 games' win percentage and game pace to judge current momentum.
- Upset Potential: Assesses teams’ capability to overcome stronger-seeded opponents.
#### Simulation and Analysis:
Simulates thousands of tournament paths, outputting detailed team progression and frequency of stage advancement.
## Visualizations:
#### Scatterplot:
Depicts the frequency with which each team progresses to each round across simulations. The points' size represents the number of simulations reaching each round.
![scatter_chart25](https://github.com/user-attachments/assets/3e965c21-2033-4556-a1b0-0bb16e8e90cc)
#### Lollipop Chart:
Highlights the most frequent progression for each team, showing the round they most commonly reach across all simulations. This provides a clear view of probable team performance.!
![lollipop_chart25](https://github.com/user-attachments/assets/e1a85d6a-eb39-48b6-b526-69653f819bd5)
