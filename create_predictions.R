library(softballR)
library(tidyverse)
library(anytime)

source("get_power_ratings.R")

#### COMPILE TRAIN DATA ####

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


model <- glm(home_win ~ offensive_rating.x + defensive_rating.x + offensive_rating.y + defensive_rating.y, 
             data = postseason, family = "binomial")


#### USE MODEL TO PREDICT 2022 ####

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

postseason_2022$pred <- predict(model, postseason_2022, type = "response")
postseason_2022$pred_home_win <- postseason_2022$pred > .5

table(postseason_2022$home_win, postseason_2022$pred_home_win)

(37+56) / (37+56+31+18)

# Next step: incorporate ace stats 