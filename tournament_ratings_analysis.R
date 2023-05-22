library(softballR)
library(tidyverse)
library(nflplotR)
library(fmsb)
library(hrbrthemes)
library(gt)
library(gtExtras)

source("get_power_ratings.R")

scoreboard <- load_ncaa_softball_scoreboard(2023)
ratings <- get_power_ratings(scoreboard) %>% 
  mutate(power_rank = rank(-power_rating))

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

tournament_teams <- read_csv("tournament_teams_2023.csv")

combined <- merge(ratings, tournament_teams, by.x = "team", by.y = "Team") %>% 
  merge(logos, by = "team") %>% 
  mutate(offensive_perc = percentile(offensive_rating),
         defensive_perc = percentile(defensive_rating),
         offensive_perc = ifelse(is.na(offensive_perc), 0, offensive_perc),
         defensive_perc = ifelse(is.na(defensive_perc), 0, defensive_perc),
         host = Seed == 1)

ggplot(combined) +
  geom_from_path(aes(x = defensive_perc, y = offensive_perc,
                     path = logo, alpha = host), width = .05) +
  scale_alpha_manual(values = c(0.2, 0.8)) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_vline(xintercept = 50, linetype = "dashed") +
  geom_text(x = 90, y = 108, label = "Better Defense", size = 3, check_overlap = TRUE) +
  geom_text(x = 10, y = 108, label = "Worse Defense", size = 3, check_overlap = TRUE) +
  geom_text(x = 108, y = 90, label = "Better Offense", size = 3, angle = 90, check_overlap = TRUE) +
  geom_text(x = 108, y = 10, label = "Worse Offense", size = 3, angle = 90, check_overlap = TRUE) +
  labs(x = "Defensive Rating (Percentile)",
       y = "Offensive Rating (Percentile)",
       title = "2023 NCAA Tournament Team Ratings",
       subtitle = "Viz by @tking0426 | Data from softballR") +
  coord_cartesian(xlim = c(0,100),
                  ylim = c(0,100),
                  clip = "off") +
  theme_ipsum() +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
        plot.subtitle = element_text(hjust = 0.5))

ggsave("tournament_ratings_plot.png")

region_stats <- combined %>% 
  filter(host == FALSE) %>%
  merge(combined %>% filter(host == TRUE) %>% select(Region, logo) %>% rename(host_logo = logo), by = "Region") %>% 
  group_by(host_logo, Region) %>% 
  summarise(avg_off = mean(offensive_rating),
            avg_def = mean(defensive_rating),
            avg_rank = mean(power_rank)) %>% 
  ungroup()

table <- region_stats %>% 
  arrange(avg_rank) %>% 
  gt() %>% 
  gt_img_rows(columns = host_logo) %>% 
  fmt_number(columns = 3:5, decimals = 2) %>% 
  cols_label(host_logo = "Host",
             avg_off = "Avg. Off. Rating",
             avg_def = "Avg. Def. Rating",
             avg_rank = "Avg. Power Rank") %>% 
  tab_header(title = "NCAA Tournament Regional Stats",
             subtitle = "(Excluding Host Teams)") %>% 
  data_color(columns = avg_off,
             fn = scales::col_numeric(
               palette = c("#77DE78", "#FF6962"),
               domain = c(min(region_stats$avg_off), max(region_stats$avg_off))
             )) %>% 
  data_color(columns = avg_def,
             fn = scales::col_numeric(
               palette = c("#77DE78", "#FF6962"),
               domain = c(min(region_stats$avg_def), max(region_stats$avg_def))
             )) %>% 
  data_color(columns = avg_rank,
             fn = scales::col_numeric(
               palette = c("#FF6962", "#77DE78"),
               domain = c(min(region_stats$avg_rank), max(region_stats$avg_rank))
             )) %>% 
  gt_theme_538() %>% 
  tab_options(heading.align = 'center') 

gtsave(table, "region_stats.png")


biggest_comp <- combined %>% 
  filter(host == FALSE) %>%
  merge(combined %>% filter(host == TRUE) %>% select(Region, logo) %>% rename(host_logo = logo), by = "Region") %>% 
  group_by(host_logo, Region) %>% 
  filter(power_rank == min(power_rank)) %>% 
  ungroup() %>% 
  select(host_logo, logo, offensive_rating, defensive_rating, power_rank)

table <- biggest_comp %>% 
  arrange(power_rank) %>% 
  gt() %>% 
  gt_img_rows(columns = host_logo) %>% 
  gt_img_rows(columns = logo) %>% 
  fmt_number(columns = 3:5, decimals = 2) %>% 
  cols_label(host_logo = "Host",
             logo = "Team",
             offensive_rating = "Off. Rating",
             defensive_rating = "Defensive Rating",
             power_rank = "Power Rank") %>% 
  tab_header(title = "NCAA Tournament Regional Stats",
             subtitle = "(Top ranked non-hosts)") %>% 
  data_color(columns = offensive_rating,
             fn = scales::col_numeric(
               palette = c("#77DE78", "#FF6962"),
               domain = c(min(biggest_comp$offensive_rating), max(biggest_comp$offensive_rating))
             )) %>% 
  data_color(columns = defensive_rating,
             fn = scales::col_numeric(
               palette = c("#77DE78", "#FF6962"),
               domain = c(min(biggest_comp$defensive_rating), max(biggest_comp$defensive_rating))
             )) %>% 
  data_color(columns = power_rank,
             fn = scales::col_numeric(
               palette = c("#FF6962", "#77DE78"),
               domain = c(min(biggest_comp$power_rank), max(biggest_comp$power_rank))
             )) %>% 
  tab_style(style = cell_borders(sides = "right", color = "#d3d3d3"),
            locations = cells_body(columns = host_logo)) %>% 
  gt_theme_538() %>% 
  tab_options(heading.align = 'center') 

gtsave(table, "biggest_comp.png")
