library(tidyverse)
library(gt)
library(gtExtras)
library(janitor)
library(softballR)

predictions_df <- read_csv("predictions_2023.csv")

tournament_teams <- read_csv("tournament_teams_2023.csv")

sim_game <- function(team1_name, team2_name){
  
  prob <- predictions_df %>% filter(team1 == team1_name & team2 == team2_name) %>% pull(pred)
  
  sample(c(team1_name, team2_name), prob = c(prob, 1 - prob), 1)
  
}

sim_wcws <- function(){

  game1 <- sim_game("Oklahoma", "Stanford")
  game2 <- sim_game("Tennessee", "Alabama")
  game3 <- sim_game("Florida St.", "Oklahoma St.")
  game4 <- sim_game("Washington", "Utah")
  
  game5 <- sim_game(setdiff(c("Oklahoma", "Stanford"), game1),
                    setdiff(c("Tennessee", "Alabama"), game2))
  
  game6 <- sim_game(setdiff(c("Florida St.", "Oklahoma St."), game3),
                    setdiff(c("Washington", "Utah"), game4))
  
  
  game7 <- sim_game(game1, game2)
  game8 <- sim_game(game3, game4)
  
  game9 <- sim_game(game5, setdiff(c(game3, game4), game8))
  game10 <- sim_game(game6, setdiff(c(game1, game2), game7))
  
  game11 <- sim_game(game7, game9)
  bracket_champ_1 <- ifelse(game7 == game11, game11, sim_game(game7, game9))
  
  game13 <- sim_game(game8, game10)
  bracket_champ_2 <- ifelse(game8 == game13, game13, sim_game(game8, game10))
  
  game15 <- sim_game(bracket_champ_1, bracket_champ_2)
  game16 <- sim_game(bracket_champ_1, bracket_champ_2)
  
  winner <- ifelse(game15 == game16, game15, sim_game(bracket_champ_1, bracket_champ_2))
  
  return(data.frame(bracket_champ_1, bracket_champ_2, winner))
  
}

df <- data.frame()

for(i in 1:1000){
  
  if(i %% 100 == 0) print(i)
  
  df <- rbind(df, sim_wcws())
  
}

df %>% 
  group_by(bracket_champ_1) %>% 
  summarise(pct = n() / 1000) %>% 
  ungroup() %>% 
  arrange(desc(pct))

df %>% 
  group_by(bracket_champ_2) %>% 
  summarise(pct = n() / 1000) %>% 
  ungroup() %>% 
  arrange(desc(pct))

df %>% 
  group_by(winner) %>% 
  summarise(pct = n() / 1000) %>% 
  ungroup() %>% 
  arrange(desc(pct))
