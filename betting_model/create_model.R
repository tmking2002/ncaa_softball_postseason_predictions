library(softballR)
library(tidyverse)

source("~/Projects/softball-projects/get_power_ratings.R")

scoreboard <- load_ncaa_softball_scoreboard(2023) %>% 
  drop_na(home_team_runs) %>% 
  drop_na(away_team_runs)

power_ratings <- get_power_ratings(scoreboard)

box <- load_ncaa_softball_playerbox(2023, category = "Pitching")

opponent_offensive_stats <- rbind(scoreboard[c(9,1,4,5,8)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs")),
                                  scoreboard[c(9,5,8,1,4)] %>% 
                                    `names<-`(c("date", "team", "runs", "opponent", "opponent_runs"))) %>% 
  merge(power_ratings %>% select(team, offensive_rating), by.x = "opponent", by.y = "team") %>% 
  group_by(team) %>% 
  summarise(avg_opp_off_rating = mean(offensive_rating)) %>% 
  ungroup()

stats <- box %>% 
  separate(ip, c("innings", "frac"), sep = "\\.") %>% 
  mutate(ip = ifelse(is.na(frac), innings, as.numeric(innings) + as.numeric(frac) * 1/3)) %>% 
  select(-c(innings, frac)) %>% 
  mutate(across(c(4:35,39), as.numeric)) %>% 
  group_by(player, team) %>% 
  summarise(across(where(is.numeric), 
                   .fns = sum),
            SOr = so / bf,
            BBr = (bb + hb) / bf,
            ERA = er / ip * 7,
            FIP = ((13 * hr_a) + (3 * (bb + hb)) - 2 * so) / ip) %>% 
  merge(opponent_offensive_stats, by = "team") 

avg_era = mean(stats$ERA * stats$ip) / mean(stats$ip)
avg_fip = mean(stats$FIP * stats$ip) / mean(stats$ip)

fip_constant <- avg_era - avg_fip

stats$FIP <- stats$FIP + fip_constant

model <- lm(FIP ~ avg_opp_off_rating, data = stats)

pitcher_stats_upd <- stats %>% 
  mutate(wFIP = FIP + coef(model)[2] * avg_opp_off_rating)

starters <- box %>% 
  filter(ord_appeared == 1) %>% 
  merge(pitcher_stats_upd %>% select(player, wFIP), by = "player") %>% 
  select(wFIP, team, game_id) 

scores <- scoreboard %>% 
  mutate(margin = home_team_runs - away_team_runs) %>% 
  merge(power_ratings %>% select(team, offensive_rating, defensive_rating), by.x = "away_team", by.y = "team") %>% 
  rename(away_team_off_rating = offensive_rating,
         away_team_def_rating = defensive_rating) %>% 
  merge(power_ratings %>% select(team, offensive_rating, defensive_rating), by.x = "home_team", by.y = "team") %>% 
  rename(home_team_off_rating = offensive_rating,
         home_team_def_rating = defensive_rating) %>% 
  merge(starters %>% rename(away_starter = wFIP), by.x = c("away_team", "game_id"), by.y = c("team", "game_id")) %>% 
  merge(starters %>% rename(home_starter = wFIP), by.x = c("away_team", "game_id"), by.y = c("team", "game_id")) %>% 
  select(away_team, away_team_off_rating, away_team_def_rating, away_starter, away_team_runs, 
         home_team, home_team_off_rating, home_team_def_rating, home_starter, home_team_runs, margin, game_id)

total <- rbind(scores %>% `names<-`(c("team", "off_rating", "def_rating", "starter", "runs", "opp",
                                                 "opp_off_rating", "opp_def_rating", "opp_starter", "opp_runs", "margin", "game_id")),
               scores[c(6:10, 1:5, 11:12)] %>% `names<-`(c("team", "off_rating", "def_rating", "starter", "runs", "opp",
                                                                       "opp_off_rating", "opp_def_rating", "opp_starter", "opp_runs", "margin", "game_id")))

model <- lm(runs ~ off_rating + opp_def_rating + opp_starter, total)

scores$pred_away_runs <- predict(model, scores %>% rename(off_rating = away_team_off_rating,
                                                               opp_def_rating = home_team_def_rating,
                                                               opp_starter = home_starter))

scores$pred_home_runs <- predict(model, scores %>% rename(off_rating = home_team_off_rating,
                                                               opp_def_rating = away_team_def_rating,
                                                               opp_starter = away_starter))

scores$pred_margin <- scores$pred_home_runs - scores$pred_away_runs
scores$resid <- scores$margin - scores$pred_margin

save(model, file = "betting_model/game_model.RDA")

rm(list = setdiff(ls(), c("model", "scoreboard", "power_ratings", "pitcher_stats_upd")))
