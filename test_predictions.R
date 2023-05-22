library(softballR)
library(tidyverse)
library(anytime)
library(xgboost)
library(pROC)

source("get_power_ratings.R")

#### COMPILE TRAIN DATA ####

# Read in data and create 'postseason' variable
scoreboard_2017 <- load_ncaa_scoreboard(2017) %>% mutate(game_date = anydate(game_date),
                                                         season = 2017,
                                                         postseason = ifelse(game_date >= "2017-05-18",T,F))

scoreboard_2018 <- load_ncaa_scoreboard(2018) %>% mutate(game_date = anydate(game_date),
                                                         season = 2018,
                                                         postseason = ifelse(game_date >= "2018-05-18",T,F))

scoreboard_2019 <- load_ncaa_scoreboard(2019) %>% mutate(game_date = anydate(game_date),
                                                         season = 2019,
                                                         postseason = ifelse(game_date >= "2019-05-31",T,F))

scoreboard_2021 <- load_ncaa_scoreboard(2021) %>% mutate(game_date = anydate(game_date),
                                                         season = 2021,
                                                         postseason = ifelse(game_date >= "2021-05-21",T,F))

regular_season <- rbind(scoreboard_2017, scoreboard_2018, scoreboard_2019, scoreboard_2021) %>% 
  filter(postseason == F)


# Find power ratings for each season
power_ratings <- data.frame()

for(i in c(2017, 2018, 2019, 2021)){
  
  curr_power_ratings <- get_power_ratings(regular_season %>% filter(season == i))
  
  power_ratings <- rbind(power_ratings, curr_power_ratings %>% mutate(season = i))
  
}


postseason <- rbind(scoreboard_2017, scoreboard_2018, scoreboard_2019, scoreboard_2021) %>% 
  filter(postseason == T) %>% 
  mutate(home_win = home_team_runs > away_team_runs) %>% 
  merge(power_ratings, by.x = c("home_team","season"), by.y = c("team","season")) %>% 
  merge(power_ratings, by.x = c("away_team","season"), by.y = c("team","season"))


# Create logistic regression that uses offensive and defensive ratings to predict the winner 
log_model <- glm(home_win ~ offensive_rating.x + defensive_rating.x + offensive_rating.y + defensive_rating.y, 
             data = postseason, family = "binomial")


#### USE LOGISTIC MODEL TO PREDICT 2022 ####

scoreboard_2022 <- load_ncaa_scoreboard(2022) %>% mutate(game_date = anydate(game_date),
                                                         postseason = ifelse(game_date >= "2022-05-20",T,F))

regular_season_2022 <- scoreboard_2022 %>% 
  filter(postseason == F)

power_ratings_2022 <- get_power_ratings(regular_season_2022)


postseason_2022 <- scoreboard_2022 %>% 
  filter(postseason == T) %>% 
  mutate(home_win = home_team_runs > away_team_runs) %>% 
  merge(power_ratings_2022, by.x = "home_team", by.y = "team") %>% 
  merge(power_ratings_2022, by.x = "away_team", by.y = "team")

postseason_2022$pred <- predict(log_model, postseason_2022, type = "response")
postseason_2022$pred_home_win <- postseason_2022$pred > .5

table(postseason_2022$home_win, postseason_2022$pred_home_win)

log_model_accuracy <- (37+56) / (37+56+31+18)

roc_curve <- roc(postseason_2022$home_win, postseason_2022$pred)

plot(roc_curve, main="ROC Curve")

# Next step: incorporate ace stats 

#### XGBOOST MODEL ####

train_label <- postseason$home_win
train_data <- postseason[15:32]

test_label <- postseason_2022$home_win
test_data <- postseason_2022[14:31]

# Define the parameter grid
param_grid <- expand.grid(nrounds = seq(50,250, 50),
                          max_depth = seq(3,15,3),
                          eta = seq(0.1,0.5,0.1))

# Initialize variables to store the best accuracy and parameters
best_accuracy <- 0
best_params <- NULL

# Loop over all parameter combinations in the grid
for (i in 1:nrow(param_grid)) {
  # Train the model using the current parameter values
  xgb_model <- xgboost(data = as.matrix(train_data),
                       label = train_label,
                       nrounds = param_grid$nrounds[i],
                       max_depth = param_grid$max_depth[i],
                       eta = param_grid$eta[i],
                       objective = "binary:logistic")
  
  # Make predictions on the test data
  predictions <- predict(xgb_model, as.matrix(test_data)) > .5
  
  # Calculate the accuracy
  accuracy <- sum(predictions == test_label)/length(predictions)
  
  # Check if the current parameter combination improves the best accuracy
  if (accuracy > best_accuracy) {
    best_accuracy <- accuracy
    best_params <- param_grid[i, ]
  }
}

best_model <- xgboost(data = as.matrix(train_data),
                      label = train_label,
                      nrounds = best_params$nrounds[1],
                      max_depth = best_params$max_depth[1],
                      eta = best_params$eta[1],
                      objective = "binary:logistic")

predictions <- predict(best_model, as.matrix(test_data)) > .5

xgb_accuracy <- sum(predictions == test_label)/length(predictions)

importance <- xgb.importance(feature_names = names(train_data), model = best_model)

xgb.plot.importance(importance_matrix = importance)
