library(nflfastR)
library(tidyverse)
library(gt)
library(ggimage)
library(ggthemes)

pbp = nflfastR::load_pbp(2021:2022)
teams = nflfastR::teams_colors_logos

yac_percent =
  pbp %>%
  filter(pass == 1) %>%
  group_by(posteam) %>%
  summarise(
    yac_prop = sum(yards_after_catch, na.rm = TRUE)/sum(yards_gained, na.rm = TRUE)
  )

yac_oe = 
  pbp %>%
  filter(pass == 1) %>%
  mutate(
    yac_oe = yards_after_catch - xyac_mean_yardage
    ) %>%
  group_by(posteam) %>%
  summarise(mean_yac_oe = mean(yac_oe, na.rm = TRUE))

yac = 
  left_join(yac_percent, yac_oe, by = 'posteam') %>%
  left_join(teams, by = c('posteam' = 'team_abbr')) %>%
  select(posteam, yac_prop, mean_yac_oe, team_logo_espn)

ggplot(yac, aes(x = mean_yac_oe, y = yac_prop)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_vline(xintercept = mean(yac$mean_yac_oe), linetype = 'dashed') +
  geom_hline(yintercept = mean(yac$yac_prop), linetype = 'dashed') +
  labs(title = "YAC'n it up: what teams rely on yards after catch",
       subtitle = "2021-2022 season",
       x = 'Yards After Catch Over Expected',
       y = "Yards After Catch % of Total Air Yards",
       caption = "By Robby Patterson | @rcpiv_analytics | @nflfastr") +
  theme_clean()
ggsave("C:\\Users\\rcpat\\Desktop\\Personal Projects\\nflfastR\\nflanalytics\\Plots\\YAC_OE.png", width = 14, height = 9, dpi = 'retina')
