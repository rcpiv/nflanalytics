# Install packages if needed
# install.packages('nflfastR')
# install.packages('tidyverse')

# Import libraries
library(nflfastR)
library(tidyverse)

# Load in play by play, weekly, and roster data
# View color and info data
pbp_2021 = load_pbp(2021)
ps_2021 = load_player_stats(2021)
roster_2021 = fast_scraper_roster(2021)
team_colors = 
  teams_colors_logos %>%
  select(team_abbr, team_color, team_color2, team_color3)

# Create points per first down column
ps_2021_ppfd =
  ps_2021 %>%
  mutate(fantasy_points_ppfd = fantasy_points + receiving_first_downs + rushing_first_downs) %>%
  left_join(team_colors, by = c('recent_team' = 'team_abbr'))

# Plot
ggplot(data = ps_2021_ppfd, aes(x = fantasy_points_ppr, y = fantasy_points_ppfd)) +
  geom_point(aes(colour = team_color)) + 
  scale_color_identity(aesthetics = "colour") + # This is needed to assign proper hex codes from teams_colors_logos
  labs(x = "Points per Reception",
       y = "Points per First Down",
       title = 'Points per Reception vs Points per First Down',
       caption = "By: Robby Patterson | @rcpiv_analytics | nflfastR")

# Group by and summarise to get average per player, filter for RB/WR/TE
ps_2021_sum = 
  roster_2021 %>%
  select(position, gsis_id) %>%
  right_join(ps_2021, by = c("gsis_id" = "player_id")) %>%
  filter(position %in% c('WR','TE','RB','FB'), season_type == 'REG') %>%
  mutate(fantasy_points_ppfd = fantasy_points + receiving_first_downs + rushing_first_downs) %>%
  group_by(gsis_id, player_name, recent_team) %>%
  summarise(avg_fp = mean(fantasy_points), avg_fp_ppr = mean(fantasy_points_ppr), avg_fp_ppfd = mean(fantasy_points_ppfd)) %>%
  left_join(team_colors, by = c("recent_team" = "team_abbr"))
  
ggplot(ps_2021_sum, aes(x = avg_fp_ppr, y = avg_fp_ppfd)) +
  geom_point(aes(colour = team_color)) +
  geom_smooth(method='lm', aes(colour = 'red')) +
  scale_color_identity(aesthetics = "colour") +
  labs(x = "Avg Points per Reception",
       y = "Avg Points per First Down",
       title = 'Avg Points per Reception vs Avg Points per First Down',
       caption = "By: Robby Patterson | @rcpiv_analytics | nflfastR")
  

