library(nflfastR)
library(tidyverse)
library(ggimage)
library(gt)
library(ggthemes)

pbp_2022 = load_pbp(2022)
pbp_2021 = load_pbp(2021)
pbp = load_pbp(2021:2022)

POE_winning = 
  pbp %>%
  filter(game_half == 'Half2') %>%
  filter(wp >= 0.9) %>%
  filter(play == 1 & special == 0) %>%
  filter(!is.na(pass_oe)) %>%
  group_by(posteam) %>%
  summarise(POE_mean = mean(pass_oe))

pass_sr = 
  pbp %>%
  filter(game_half == 'Half2') %>%
  filter(wp >= .9) %>%
  filter(pass == 1 & special == 0) %>%
  group_by(posteam) %>%
  summarise(pass = mean(success), n = n())

POE_PR = 
  POE_winning %>%
  left_join(pass_sr, by = c('posteam' = 'posteam')) %>%
  left_join(teams_colors_logos, by = c('posteam' = 'team_abbr'))

ggplot(POE_PR, aes(x = POE_mean, y = pass)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_vline(xintercept = mean(POE_PR$POE_mean), linetype = 'dashed') +
  geom_hline(yintercept = mean(POE_PR$pass), linetype = 'dashed') +
  labs(title = "Pass Over Expected per Play vs Passing Success Rate",
       subtitle = "2021 Season, 2nd Half, WP >= 90%",
       x = 'Pass Over Expected Per Play',
       y = "Pass Success Rate",
       caption = "By Robby Patterson | @rcpiv_analytics | @nflfastr") +
  theme_bw()

