library(softballR)
library(tidyverse)
library(anytime)
library(xgboost)
library(pROC)

source("get_power_ratings.R")

#### COMPILE TRAIN DATA ####

# Read in data and create 'postseason' variable
scoreboard_2017 <- load_ncaa_softball_scoreboard(2017) %>% mutate(game_date = anydate(game_date),
                                                         season = 2017,
                                                         postseason = ifelse(game_date >= "2017-05-18",T,F))

scoreboard_2018 <- load_ncaa_softball_scoreboard(2018) %>% mutate(game_date = anydate(game_date),
                                                         season = 2018,
                                                         postseason = ifelse(game_date >= "2018-05-18",T,F))

scoreboard_2019 <- load_ncaa_softball_scoreboard(2019) %>% mutate(game_date = anydate(game_date),
                                                         season = 2019,
                                                         postseason = ifelse(game_date >= "2019-05-31",T,F))

scoreboard_2021 <- load_ncaa_softball_scoreboard(2021) %>% mutate(game_date = anydate(game_date),
                                                         season = 2021,
                                                         postseason = ifelse(game_date >= "2021-05-21",T,F))

scoreboard_2022 <- load_ncaa_softball_scoreboard(2022) %>% mutate(game_date = anydate(game_date),
                                                                  season = 2022,
                                                                  postseason = ifelse(game_date >= "2022-05-20",T,F))

regular_season <- rbind(scoreboard_2017, scoreboard_2018, scoreboard_2019, scoreboard_2021, scoreboard_2022) %>% 
  filter(postseason == F)


# Find power ratings for each season
power_ratings <- data.frame()

for(i in c(2017, 2018, 2019, 2021, 2022)){
  
  curr_power_ratings <- get_power_ratings(regular_season %>% filter(season == i))
  
  power_ratings <- rbind(power_ratings, curr_power_ratings %>% mutate(season = i))
  
}


postseason <- rbind(scoreboard_2017, scoreboard_2018, scoreboard_2019, scoreboard_2021, scoreboard_2022) %>% 
  filter(postseason == T) %>% 
  mutate(home_win = home_team_runs > away_team_runs) %>% 
  merge(power_ratings, by.x = c("home_team","season"), by.y = c("team","season")) %>% 
  merge(power_ratings, by.x = c("away_team","season"), by.y = c("team","season"))


# Create logistic regression that uses offensive and defensive ratings to predict the winner 
log_model <- glm(home_win ~ offensive_rating.x + defensive_rating.x + offensive_rating.y + defensive_rating.y +
                   rank.x + rank.y, 
             data = postseason, family = "binomial")


#### USE LOGISTIC MODEL TO PREDICT 2023 ####

scoreboard_2023 <- load_ncaa_softball_scoreboard(2023) %>% mutate(game_date = anydate(game_date),
                                                         postseason = ifelse(game_date >= "2023-05-15",T,F))

regular_season_2023 <- scoreboard_2023 %>% 
  filter(postseason == F)

power_ratings_2023 <- get_power_ratings(regular_season_2023)

# Create dataframe with every possible combination of teams

teams <- unique(c(regular_season_2023$away_team, regular_season_2023$home_team))

prediction_df <- expand.grid(teams, teams) %>% 
  rename(team1 = Var1,
         team2 = Var2) %>% 
  filter(team1 != team2) %>% 
  merge(power_ratings_2023, by.x = "team1", by.y = "team") %>% 
  merge(power_ratings_2023, by.x = "team2", by.y = "team")
  
prediction_df$pred <- predict(log_model, prediction_df, type = "response")

write_csv(prediction_df, "predictions_2023.csv")
