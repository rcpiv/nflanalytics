library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)

pbp_21 = nflfastR::load_pbp(2021)
pbp_22 = nflfastR::load_pbp(2022)

epa_21 = 
  pbp_21 %>%
  filter(pass == 1 | rush == 1, season_type == 'REG') %>%
  group_by(defteam) %>%
  mutate(defteam_mean = mean(epa, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(posteam, week) %>%
  mutate(week_posteam_epa = mean(epa, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(posteam) %>%
  mutate(posteam_mean = mean(epa, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(defteam, week) %>%
  mutate(week_defteam_epa = mean(epa, na.rm = TRUE)) %>%
  ungroup() %>%
  select(week, posteam, defteam, week_posteam_epa, week_defteam_epa, posteam_mean, defteam_mean)

posteam_epa_21 =
  epa_21 %>%
  group_by(posteam, week) %>%
  summarise(epa_pp_week = mean(week_posteam_epa),
            def_epa_pp = mean(defteam_mean)) %>%
  mutate(epa_oa = epa_pp_week - def_epa_pp) %>%
  group_by(posteam) %>%
  summarise(mean_epa_oa = mean(epa_oa))

defteam_epa_21 =
  epa_21 %>%
  group_by(defteam, week) %>%
  summarise(epa_pp_week = mean(week_defteam_epa),
            off_epa_pp = mean(posteam_mean)) %>%
  mutate(epa_oa = epa_pp_week - off_epa_pp) %>%
  group_by(defteam) %>%
  summarise(mean_epa_oa = mean(epa_oa))

epa_21_teams = 
  left_join(posteam_epa_21, defteam_epa_21, 
            by = c("posteam" = "defteam"), 
            suffix = c(".off",".def")) %>%
  left_join(teams_colors_logos,
            by = c("posteam" = "team_abbr")) %>%
  select(posteam, mean_epa_oa.off, mean_epa_oa.def, team_logo_espn)


ggplot(data = epa_21_teams, aes(x = mean_epa_oa.off, y = mean_epa_oa.def)) +
  geom_image(aes(image = team_logo_espn), size = 0.05, asp = 16/9, alpha = .9) +
  scale_y_reverse() +
  geom_vline(xintercept = mean(epa_21_teams$mean_epa_oa.off), linetype = 'dashed') +
  geom_hline(yintercept = mean(epa_21_teams$mean_epa_oa.def), linetype = 'dashed') +
  labs(title = "Outperforming the expectation: 2021 Season",
       subtitle = "An analysis on which teams outperformed their opponents season average EPAs",
       x = 'Mean Offensive EPA PP Over Opponent Average',
       y = "Mean Defensive EPA PP Over Opponent Average",
       caption = "By Robby Patterson | @rcpiv_analytics | @nflfastr") +
  theme_clean()
ggsave("C:\\Users\\rcpat\\Desktop\\Personal Projects\\nflfastR\\nflanalytics\\Plots\\TeamTiers2021.png", width = 14, height = 9, dpi = 'retina')

