# Load Packages
library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)
library(ggthemes)

# Load play-by-play and roster data
pbp_2021 = load_pbp(2021)
roster = fast_scraper_roster(2021)

# Get all QB dropbacks
dropback = pbp_2021 %>%
  filter(qb_dropback == 1, season_type == 'REG') %>%
  group_by(passer_player_name) %>%
  filter(n() >= 100) %>%
  left_join(teams_colors_logos, by=c("posteam" = "team_abbr")) %>%
  select(passer_player_name, epa, team_color, team_color2) %>%
  mutate(avg_epa = mean(epa)) %>%
  drop_na() %>%
  arrange(desc(avg_epa))

# Show distribution of EPA for all dropbacks
ggplot(dropback,aes(epa)) +
  geom_histogram(binwidth = 0.5, aes( y = ..density..), fill = "white", color = "grey30") +
  geom_density(alpha = .2) +
  labs(title = "QB Dropback EPA: League Wide",
       subtitle = "Weeks 1-18",
       x = "EPA",
       y = "",
       caption = "By Robby Patterson | @rcpiv | @nflfastr") +
  theme_clean()

# Get plot for each QB
for (pp in unique(dropback$passer_player_name)){
  db = dropback %>% filter(passer_player_name == pp)
  
  print(
    ggplot(db, aes(epa)) +
      geom_density(aes(fill = team_color, color=team_color2),alpha = 0.5, size = 1) +
      scale_color_identity(aesthetics = c("fill","color")) + 
      labs(title = paste(db$passer_player_name, "EPA on Dropbacks"),
           subtitle = "2021 Weeks 1-18",
           x = 'EPA',
           y = "",
           caption = "By Robby Patterson | @rcpiv_analytics | @nflfastr") +
      theme_clean()
  )
}
