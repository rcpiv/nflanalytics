library(tidyverse)
library(nflreadr)
library(ggimage)
library(gt)
library(ggthemes)

personnel = nflreadr::load_participation(2021)
pbp = nflfastR::load_pbp(2021)

pbp = left_join(personnel, pbp, by = c('old_game_id','play_id')) %>%
  mutate(passer_player_name =
           case_when(
             passer_player_name == 'Aa.Rodgers' ~ 'A.Rodgers',
             TRUE ~ passer_player_name
           )
  )

pbp_personnel =
  pbp %>%
  mutate(
    o_grouping = paste0(substr(offense_personnel, 1, 1), substr(offense_personnel,7,7))
  ) %>%
  mutate(d_grouping =
    case_when(
      substr(defense_personnel, 13, 13) == '5' ~ 'Nickel',
      substr(defense_personnel, 13, 13) == '6' ~ 'Dime',
      substr(defense_personnel, 13, 13) == '7' ~ 'Quarters',
      TRUE ~ 'Base'
    )
  )

epa_cpoe_drop8 =
  pbp_personnel %>%
  filter(qb_dropback == 1, number_of_pass_rushers == 3, !is.na(cpoe), !is.na(epa)) %>%
  group_by(passer_player_name) %>%
  summarise(EPA_pp = mean(epa),
            CPOE_pp = mean(cpoe),
            snaps = n()) %>%
  filter(snaps >= 10)

epa_cpoe_blitz =
  pbp_personnel %>%
  filter(qb_dropback == 1, number_of_pass_rushers > 4, !is.na(cpoe), !is.na(epa)) %>%
  group_by(passer_player_name, posteam) %>%
  summarise(EPA_pp = mean(epa),
            CPOE_pp = mean(cpoe),
            snaps = n()) %>%
  filter(snaps >= 50)

epa_cpoe_merged = inner_join(epa_cpoe_blitz, epa_cpoe_drop8, by = 'passer_player_name',
                             suffix = c('_blitz','_drop8')) %>%
                  left_join(nflfastR::teams_colors_logos, by = c('posteam' = 'team_abbr'))

ggplot(epa_cpoe_merged, aes(x = EPA_pp_drop8, y = EPA_pp_blitz)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9) +
  geom_vline(xintercept = mean(epa_cpoe_merged$EPA_pp_blitz), linetype = 'dashed') +
  geom_hline(yintercept = mean(epa_cpoe_merged$EPA_pp_drop8), linetype = 'dashed') +
  labs(title = "Which QBs perform better vs the blitz and drop 8 coverage",
       subtitle = "A blitz is 5 or more pass rushers",
       x = 'Drop 8 EPA per play',
       y = "Blitz EPA per play",
       caption = "By Robby Patterson | @rcpiv_analytics | @nflfastr") +
  theme_clean()
ggsave("C:\\Users\\rcpat\\Desktop\\Personal Projects\\nflfastR\\nflanalytics\\Plots\\EPA_Drop8vBlitz.png", width = 14, height = 9, dpi = 'retina')

