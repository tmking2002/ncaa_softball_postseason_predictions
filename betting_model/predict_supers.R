library(conflicted)
library(janitor)
library(MASS)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

source("betting_model/create_model.R")

get_info <- function(matchup){
  
  updated <- matchup %>% 
    merge(pitcher_stats_upd %>% select(player, wFIP), by.x = "away_starting_pitcher", by.y = "player") %>% 
    rename(away_starter = wFIP) %>% 
    merge(pitcher_stats_upd %>% select(player, wFIP), by.x = "home_starting_pitcher", by.y = "player") %>% 
    rename(home_starter = wFIP) %>% 
    merge(power_ratings %>% select(team, offensive_rating, defensive_rating), by.x = "away_team", by.y = "team") %>% 
    rename(away_team_off_rating = offensive_rating,
           away_team_def_rating = defensive_rating) %>% 
    merge(power_ratings %>% select(team, offensive_rating, defensive_rating), by.x = "home_team", by.y = "team") %>% 
    rename(home_team_off_rating = offensive_rating,
           home_team_def_rating = defensive_rating)
  
  return(updated)

}

sim_game <- function(matchup_info, num_sims){
  
  away_pred <- predict(model, matchup_info %>% rename(off_rating = away_team_off_rating,
                                                           opp_def_rating = home_team_def_rating,
                                                           opp_starter = home_starter))
  
  home_pred <- predict(model, matchup_info %>% rename(off_rating = home_team_off_rating,
                                                           opp_def_rating = away_team_def_rating,
                                                           opp_starter = away_starter))
  
  scores <- data.frame()
  
  for(i in 1:num_sims){
    
    away_runs <- rpois(1, away_pred)
    home_runs <- rpois(1, home_pred)
    
    margin <- home_runs - away_runs
    
    while(margin == 0){

      away_runs <- rpois(1, away_pred / 7)
      home_runs <- rpois(1, home_pred / 7)
     
      margin <- home_runs - away_runs
       
    }
    
    scores <- rbind(scores, data.frame(home_runs, away_runs, margin) %>% 
                      mutate(total = home_runs + away_runs))
   
    if(i %% 100 == 0) print(i)
     
  }
  
  return(scores)
  
}

get_stats <- function(scores){
  
  win_perc <- round_half_up(c(mean(scores$margin > 0) - .025,
                              mean(scores$margin > 0) + .025), 2)
  
  median_runs <- round_half_up(c(quantile(scores$total, .4),
                                 quantile(scores$total, .6)), 1)
  
  return(list(win_perc, median_runs))
  
}

#### Oklahoma vs. Clemson ####

matchup <- data.frame(away_team = "Clemson", away_starting_pitcher = "Cagle, Valerie",
                      home_team = "Oklahoma", home_starting_pitcher = "Storako, Alex")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

#### Duke vs. Stanford ####

matchup <- data.frame(away_team = "Stanford", away_starting_pitcher = "Canady, NiJaree",
                      home_team = "Duke", home_starting_pitcher = "Curd, Cassidy")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

#### Alabama vs. Northwestern ####

# Fouts

matchup <- data.frame(away_team = "Northwestern", away_starting_pitcher = "Williams, Danielle",
                      home_team = "Alabama", home_starting_pitcher = "Fouts, Montana")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

# Torrence

matchup <- data.frame(away_team = "Northwestern", away_starting_pitcher = "Williams, Danielle",
                      home_team = "Alabama", home_starting_pitcher = "Torrence, Jaala")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

#### Tennessee vs. Texas ####

matchup <- data.frame(away_team = "Texas", away_starting_pitcher = "Morgan, Mac",
                      home_team = "Tennessee", home_starting_pitcher = "Rogers, Ashley")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

#### Florida St. vs. Georgia ####

matchup <- data.frame(home_team = "Florida St.", home_starting_pitcher = "Sandercock, Kathryn",
                      away_team = "Georgia", away_starting_pitcher = "Walters, Shelby")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

#### Oklahoma St. vs. Oregon ####

matchup <- data.frame(away_team = "Oregon", away_starting_pitcher = "Hansen, Stevie",
                      home_team = "Oklahoma St.", home_starting_pitcher = "Maxwell, Kelly")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

#### Washington vs. Louisiana ####

matchup <- data.frame(away_team = "Louisiana", away_starting_pitcher = "Landry, Sam",
                      home_team = "Washington", home_starting_pitcher = "Meylan, Ruby")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

#### Utah vs. San Diego St. ####

matchup <- data.frame(away_team = "San Diego St.", away_starting_pitcher = "Light, Allie",
                      home_team = "Utah", home_starting_pitcher = "Lopez, Mariah")

matchup_info <- get_info(matchup)

scores <- sim_game(matchup_info, 10000)

stats <- get_stats(scores)

stats[[1]]
stats[[2]]

