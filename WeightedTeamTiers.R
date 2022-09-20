library(tidyverse)
library(nflfastR)
library(ggimage)

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
