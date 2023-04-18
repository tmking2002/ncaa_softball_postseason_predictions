library(softballR)
library(tidyverse)

#scoreboard <- load_ncaa_scoreboard(2023)

get_current_rpi <- function(scoreboard){
  
  team1_scoreboard <- scoreboard[c(9,1,4,5,8)] %>% `names<-`(c("date","team_name","runs","opponent_name","opponent_runs"))
  team2_scoreboard <- scoreboard[c(9,5,8,1,4)] %>% `names<-`(c("date","team_name","runs","opponent_name","opponent_runs"))
  
  scoreboard_upd <- rbind(team1_scoreboard, team2_scoreboard) %>%
    mutate(win = case_when(runs > opponent_runs ~ 1,
                           runs < opponent_runs ~ 0,
                           runs == opponent_runs ~ 0.5))
  
  
  win_perc <- scoreboard_upd %>%
    group_by(team_name) %>%
    summarise(games = n(),
              win_perc = mean(win)) %>%
    select(-games) %>% 
    drop_na()
  
  scoreboard_upd_2 <- scoreboard_upd %>%
    merge(win_perc, by.x = "opponent_name", by.y = "team_name") %>%
    rename(opponent_win_perc = win_perc) %>%
    merge(win_perc, by = "team_name")
  
  
  opponent_win_perc <- scoreboard_upd_2 %>%
    group_by(team_name) %>%
    summarise(opponent_opponent_win_perc = mean(opponent_win_perc))
  
  scoreboard_upd_3 <- scoreboard_upd_2 %>%
    merge(opponent_win_perc, by.x = "opponent_name", by.y = "team_name")
  
  
  rpi <- scoreboard_upd_3 %>%
    group_by(team_name) %>%
    summarise(rpi_coef = (.5 * mean(win_perc) + .25 * mean(opponent_win_perc) + .25 * mean(opponent_opponent_win_perc)),
              record = paste(floor(sum(win)),floor(n() - sum(win)),ceiling(sum(win) %% 1), sep = "-")) %>%
    ungroup() %>%
    mutate(rpi_rank = rank(-rpi_coef))
  
  
  return(rpi)
}