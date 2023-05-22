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

sim_region <- function(region_name){
  
  region_teams <- tournament_teams %>% filter(Region == region_name)
  
  team1 <- region_teams %>% filter(Seed == 1) %>% pull(Team)
  team2 <- region_teams %>% filter(Seed == 2) %>% pull(Team)
  team3 <- region_teams %>% filter(Seed == 3) %>% pull(Team)
  team4 <- region_teams %>% filter(Seed == 4) %>% pull(Team)
  
  # 1 vs. 4 seed
  game1 <- sim_game(team1, team4)
  
  # 2 vs. 3 seed
  game2 <- sim_game(team2, team3)
  
  # Winners bracket game 1
  game3 <- sim_game(game1, game2)
  
  # Losers bracket game 1
  losers1 <- setdiff(c(team1, team2, team3, team4), c(game1, game2))
  game4 <- sim_game(losers1[1], losers1[2])
  
  # Losers bracket game 2
  losers2 <- c(setdiff(c(game1, game2), game3), game4)
  game5 <- sim_game(losers2[1], losers2[2])
  
  # Championship
  game6 <- sim_game(game3, game5)
  
  # If necessary
  if(game6 != game3){
    champion <- sim_game(game3, game6)
  } else {
    champion <- game6
  }
  
  return(champion)
  
}

sim_regionals <- function(){
  
  regional_champs <- data.frame()
  
  for(i in 1:length(unique(tournament_teams$Region))){
    
    region <- unique(tournament_teams$Region)[i]
    
    regional_champs <- rbind(regional_champs, data.frame(team = sim_region(region),
                                                         region = region))
    
  }
  
  return(regional_champs)
  
}

sim_supers <- function(regional_champs){
  
  super_champs <- data.frame()
  
  for(i in 1:8){
    
    game1 <- sim_game(regional_champs$team[2 * i - 1],
                      regional_champs$team[2 * i])
    
    game2 <- sim_game(regional_champs$team[2 * i - 1],
                      regional_champs$team[2 * i])
    
    if(game1 != game2){
      
      champ <- sim_game(regional_champs$team[2 * i - 1],
                        regional_champs$team[2 * i])
      
    } else{
      
      champ <- game2
      
    }
    
    super_champs <- rbind(super_champs, data.frame(team = champ,
                                                   region = tournament_teams[which(tournament_teams$Team == champ),]$Region))
    
  }
  
  return(super_champs)
  
}

sim_wcws <- function(super_champs){
  
  bracket1 <- sample(super_champs$team, 4)
  bracket2 <- setdiff(super_champs$team, bracket1)
  
  # First Round
  
  game1 <- sim_game(bracket1[1], bracket1[2])
  game2 <- sim_game(bracket1[3], bracket1[4])
  game3 <- sim_game(bracket2[1], bracket2[2])
  game4 <- sim_game(bracket2[3], bracket2[4])
  
  # First Elimination Round
  
  game5 <- sim_game(setdiff(c(bracket1[1], bracket1[2]), game1),
                    setdiff(c(bracket1[3], bracket1[4]), game2))
  
  game6 <- sim_game(setdiff(c(bracket2[1], bracket2[2]), game3),
                    setdiff(c(bracket2[3], bracket2[4]), game4))
  
  # Second Round
  
  game7 <- sim_game(game1, game2)
  game8 <- sim_game(game3, game4)
  
  # Second Elimination Round
  
  game9 <- sim_game(game5, setdiff(c(game3, game4), game8))
  game10 <- sim_game(game6, setdiff(c(game1, game2), game7))
  
  # Semifinal 
  
  game11 <- sim_game(game7, game9)
  game12 <- sim_game(game8, game10)
  
  # If Necessary Semifinal
  
  if(game7 != game11){
    semifinalist1 <- sim_game(game7, game11)
  } else{
    semifinalist1 <- game7
  }
  
  if(game8 != game12){
    semifinalist2 <- sim_game(game8, game12)
  } else{
    semifinalist2 <- game8
  }
  
  semifinalists <- c(semifinalist1, semifinalist2)
  
}

sim_champ <- function(semifinalists){
  
  # Finals
  
  finals1 <- sim_game(semifinalists[1], semifinalists[2])
  finals2 <- sim_game(semifinalists[1], semifinalists[2])
  
  if(finals1 != finals2){
    champ <- sim_game(semifinalists[1], semifinalists[2])
  } else{
    champ <- finals1
  }
  
  return(champ)
}

sim_tournament <- function(num_sims){
  
  regionals <- data.frame()
  supers <- data.frame()
  semis <- data.frame()
  champ <- data.frame()
  
  for(i in 1:num_sims){
    
    regional_champs <- data.frame(team = c("Oklahoma", "Clemson", "Duke", "Stanford", "Alabama", "Northwestern", "Tennessee", "Texas",
                                           "Florida St.", "Georgia", "Oklahoma St.", "Oregon", "Washington", "Louisiana", "Utah", "San Diego St."),
                                  region = unique(tournament_teams$Region))
    regionals <- rbind(regionals, data.frame(regional_champs, sim_id = i))
    
    super_champs <- sim_supers(regional_champs)
    supers <- rbind(supers, data.frame(super_champs, sim_id = i))
    
    semifinalists <- sim_wcws(super_champs)
    semis <- rbind(semis, data.frame(semifinalists, sim_id = i))
    
    wcws_champ <- sim_champ(semifinalists)
    champ <- rbind(champ, data.frame(wcws_champ, sim_id = i))
   
    if(i %% 100 == 0) print(i)
     
  }
  
  return(list(regionals, supers, semis, champ))
  
}

find_proportions <- function(results){
  
  num_sims <- max(results[[1]]$sim_id)
  
  proportions <- data.frame(team_name = unique(tournament_teams$Team))
  
  for(i in 1:64){
  
    proportions$regionals[i] = nrow(results[[1]][which(results[[1]]$team == proportions$team_name[i]),]) / num_sims
    proportions$supers[i] = nrow(results[[2]][which(results[[2]]$team == proportions$team_name[i]),]) / num_sims
    proportions$semis[i] = nrow(results[[3]][which(results[[3]]$semifinalists == proportions$team_name[i]),]) / num_sims
    proportions$champs[i] = nrow(results[[4]][which(results[[4]]$wcws_champ == proportions$team_name[i]),]) / num_sims
      
  }
  
  return(proportions)
  
}

tictoc::tic()
results <- sim_tournament(num_sims = 10000)
tictoc::toc()


proportions <- find_proportions(results)

saveRDS(results, "sim_results_2023.RDS")
saveRDS(proportions, "sim_proportions_2023.RDS")

#results <- readRDS("sim_results_2023.RDS")
#proportions <- readRDS("sim_proportions_2023.RDS") %>% 
#  merge(tournament_teams, by.x = "team_name", by.y = "Team")

logos <- rbind(load_espn_softball_scoreboard(2022)[c(2,4)] %>% `names<-`(c("team","logo")),
               load_espn_softball_scoreboard(2022)[c(10,12)] %>% `names<-`(c("team","logo"))) %>% 
  distinct(team, logo) %>% 
  mutate(team = str_replace(team, "State", "St."),
         team = str_replace(team, "Illinois", "Ill."),
         team = str_replace(team, "Colorado", "Colo."),
         team = str_replace(team, "Northern Kentucky", "Northern Ky."),
         team = str_replace(team, "Boston University", "Boston U."),
         team = str_replace(team, "Loyola Marymount", "LMU (CA)"),
         team = str_replace(team, "Middle Tennessee", "Middle Tenn."),
         team = str_replace(team, "Central Arkansas", "Central Ark."),
         team = str_replace(team, "Long Island University", "LIU"),
         team = str_replace(team, "&", "&amp;"),
         team = str_replace(team, "North Carolina Central", "N.C. Central"),
         team = str_replace(team, "Prairie View A&amp;M", "Prairie View")) %>% 
  rbind(data.frame(team = "George Mason", logo = "https://a.espncdn.com/combiner/i?img=/i/teamlogos/ncaa/500/2244.png&h=200&w=200"))

proportions <- proportions %>% 
  merge(logos, by.x = "team_name", by.y = "team")

proportions_1 <- proportions %>% 
  mutate(total = regionals + supers + semis + champs) %>% 
  arrange(desc(champs)) %>% 
  head(n = 8)

proportions_2 <- proportions %>% 
  mutate(total = regionals + supers + semis + champs) %>% 
  arrange(desc(champs)) %>% 
  filter(!(team_name %in% proportions_1$team_name)) %>% 
  head(n = 8)

table <- cbind(proportions_1, data.frame(X1 = "", X2 = ""), proportions_2) %>% 
  clean_names() %>% 
  select(logo, team_name, supers, semis, champs, x1, x2, logo_2, team_name_2, supers_2, semis_2, champs_2) %>% 
  gt() %>% 
  gt_img_rows(columns = logo) %>% 
  gt_img_rows(columns = logo_2) %>% 
  fmt_percent(c(3:5, 10:12), decimals = 1) %>% 
  cols_label(x1 = "", x2 = "",
             logo = "", logo_2 = "",
             team_name = "", team_name_2 = "",
             supers = "WCWS", supers_2 = "WCWS",
             semis = "Champ. Series", semis_2 = "Champ. Series",
             champs = "Champion", champs_2 = "Champion") %>% 
  tab_style(style = cell_borders(sides = "left", color = "#d3d3d3"),
            locations = list(cells_body(columns = x2),
                             cells_column_labels(columns = x2))) %>% 
  tab_style(style = cell_borders(sides = "left", style = "dashed", color = "#d3d3d3"),
            locations = cells_body(columns = c(supers, supers_2))) %>% 
  tab_spanner(columns = 3:4,
              label = "% Chance To Reach...",
              id = "spanner_1") %>% 
  tab_spanner(columns = 10:12,
              label = "% Chance To Reach...",
              id = "spanner_2") %>% 
  tab_header(title = "2023 NCAA Tournament Predictions",
             subtitle = "After 10000 Sims Using Model by @tking0426") %>% 
  gt_theme_espn() %>% 
  tab_options(heading.align = 'center')

gtsave(table, "predictions_table.png")

second_favorites <- proportions %>% 
  group_by(Region) %>% 
  arrange(desc(regionals)) %>% 
  slice(2) %>% 
  select(team_name, regionals)

third_favorites <- proportions %>% 
  group_by(Region) %>% 
  arrange(desc(regionals)) %>% 
  slice(3) %>% 
  select(team_name, regionals)

regional_parity <- proportions %>% 
  group_by(Region) %>% 
  summarise(favorite = team_name[which(regionals == max(regionals))],
            favorite_pct = regionals[which(regionals == max(regionals))],
            favorite_2 = team_name[which(team_name %in% second_favorites$team_name)],
            favorite_pct_2 = regionals[which(team_name %in% second_favorites$team_name)],
            favorite_3 = team_name[which(team_name %in% third_favorites$team_name)],
            favorite_pct_3 = regionals[which(team_name %in% third_favorites$team_name)],
            sd = sd(regionals)) %>% 
  ungroup() %>% 
  mutate(parity = percentile(-sd),
         parity = ifelse(is.na(parity), 0, parity)) %>% 
  merge(logos, by.y = "team", by.x = "favorite") %>% 
  merge(logos, by.y = "team", by.x = "favorite_2") %>% 
  merge(logos, by.y = "team", by.x = "favorite_3")

table <- regional_parity %>% 
  mutate(blank = "") %>% 
  select(Region, blank, logo.x, favorite_pct, logo.y, favorite_pct_2, logo, favorite_pct_3, parity) %>% 
  arrange(desc(parity)) %>% 
  gt() %>% 
  gt_img_rows(logo.x, height = 40) %>% 
  gt_img_rows(logo.y, height = 40) %>% 
  gt_img_rows(logo, height = 40) %>% 
  fmt_percent(columns = c(favorite_pct, favorite_pct_2, favorite_pct_3), decimals = 1) %>% 
  tab_spanner(label = "Favorite",
              columns = 3:4) %>% 
  tab_spanner(label = "2nd Favorite",
              columns = 5:6) %>% 
  tab_spanner(label = "3rd Favorite",
              columns = 7:8) %>% 
  cols_label(blank = "",
             logo.x = "",
             favorite_pct = "Advance %",
             logo.y = "",
             favorite_pct_2 = "Advance %",
             logo = "",
             favorite_pct_3 = "Advance %",
             parity = "Parity Rating") %>% 
  tab_style(cell_borders(sides = "right", color = "#d3d3d3"),
            locations = cells_body(columns = c(Region, favorite_pct, favorite_pct_2, favorite_pct_3))) %>% 
  data_color(columns = parity,
             fn = scales::col_numeric(
               palette = c("#FF6962", "#77DE78"),
               domain = c(0,100)
             )) %>%
  tab_header(title = "Most 'Up for Grabs' Regions",
             subtitle = "(NCAA Softball Tournament 2023)") %>% 
  gt_theme_espn() %>% 
  tab_options(heading.align = 'center') %>% 
  opt_horizontal_padding()

gtsave(table, "regional_parity.png")
