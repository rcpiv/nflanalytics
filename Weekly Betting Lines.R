library(nflfastR)
library(tidyverse)

pbp_2022 = load_pbp(2022)
pbp_2021 = load_pbp(2021)
pbp_2020 = load_pbp(2020)

sc = fast_scraper_schedules()

lines =
  sc %>%
    filter(week == 2) %>%
    #filter(season >= 2012) %>%
    filter(!is.na(result)) %>%
    filter(spread_line != 0) %>%
    select(season, week, home_team,away_team,home_score,away_score,result,spread_line) %>%
    mutate(
      cover = case_when(
        result >= 0 & spread_line < 0 ~ 0,
        result == 0 & spread_line > 0 ~ 1,
        abs(result) > abs(spread_line) ~ 1,
        TRUE ~ 0
      )
    ) %>%
  mutate(
    mov = case_when(
      spread_line < 0 & result <= 0 ~ abs(result),
      spread_line > 0               ~ result,
      spread_line < 0 & result >= 0 ~ -result
    )
  ) %>%
  mutate(line = abs(spread_line)) %>%
  select(season, week, line, mov) %>%
  filter(line %in% c(3.5,1,6.5,2.5,3,4,10.5,10,7,5.5,1.5)) %>%
  mutate(line = as.factor(line))

ggplot(lines, aes(y = mov, x = line)) + geom_boxplot()
