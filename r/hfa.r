library(tidyverse)
library(ggtext) 
library(glue)

pbp <- purrr::map_df(c(2001:2021), function(x) {
  readRDS(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
}) %>%
  filter(season_type == 'REG')

hfa <- pbp %>%
  select(game_id, season, week, home_team, home_score, away_team, away_score) %>%
  distinct(game_id, .keep_all = TRUE) 

hfa %>%
  mutate(margin_of_victory = home_score - away_score) %>%
  summarise(hfa = mean(margin_of_victory)) %>% 
  pull(hfa)
#[1] 2.247575

# average by season
hfa_season <- hfa %>%
  mutate(margin_of_victory = home_score - away_score) %>%
  group_by(season) %>% 
  summarise(hfa = mean(margin_of_victory), .groups = "drop")

# league wide hfa by season

hfa_season %>% 
  ggplot(aes(
    x = as_factor(season), 
    y = hfa)) +
  geom_bar(stat = "identity", fill = "azure3", col = "white") +
  labs(
    x = "Season", 
    y = "Home Field Advantage",
    subtitle = "Seasons 2001 - 2021",
    caption = "data: nflfastR"
  ) +
  scale_x_discrete(labels = paste0("'", substr(seq(1999, 2021, 1), 3, 4)), breaks = seq(1999, 2021, 1)) + 
  #scale_x_discrete(breaks = seq(2001, 2021, 1)) +
  scale_y_continuous(breaks = seq(-0.5, 4, 0.5)) +
  becausejustynfun::white_theme()

# hfa by team

hfa_by_team <- hfa %>%
  gather(location, team, c(home_team, away_team)) %>%
  mutate(margin_of_victory = home_score - away_score) %>%
  group_by(team, location) %>%
  summarise(margin_of_victory_mean = mean(margin_of_victory), .groups = "drop") %>%
  mutate(margin_of_victory_mean = ifelse(location == "away_team", margin_of_victory_mean * -1, margin_of_victory_mean)) %>%
  spread(location, margin_of_victory_mean) %>% 
  mutate(
    mov_spread = home_team - away_team, 
    hfa = mov_spread / 2
  ) %>% 
  arrange(-hfa) %>% 
  select(team, hfa) %>%
  left_join(select(nflfastR::teams_colors_logos, team = team_abbr, team_color, team_color2))


hfa_by_team %>%
  ggplot(aes(
    x = reorder(team, hfa),
    y = hfa,
    fill = team_color,
    colour = team_color2
  )) +
  geom_point(size = 2.5) +
  coord_flip() +
  labs(
    x = "Team",
    y = "Home Field Advantage",
    title = "Home Field Advantage by Team",
    subtitle = "Regular Season, 1999 to 2021",
    caption = "data: nflfastR"
  ) +
  scale_y_continuous(breaks = seq(0.5, 4, 0.5)) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_hline(aes(yintercept = mean(hfa)), linetype = 2, col = "#FF6E6E") +
  geom_text(aes(x = 4, y = 2.75, label = "League Average"), stat = "unique", check_overlap = TRUE, colour = "#DB4848", size = 5, fontface = "bold") +
  becausejustynfun::white_theme() +
  theme(
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_line(colour = "gray90"),
    axis.title = element_text(family = "sans")
  )


# HFA by team and season

hfa_team_season <- hfa %>%
  gather(location, team, c(home_team, away_team)) %>%
  mutate(margin_of_victory = home_score - away_score) %>%
  group_by(team, location, season) %>%
  summarise(margin_of_victory_mean = mean(margin_of_victory), .groups = "drop") %>%
  mutate(margin_of_victory_mean = ifelse(location == "away_team", margin_of_victory_mean * -1, margin_of_victory_mean)) %>%
  spread(location, margin_of_victory_mean) %>% 
  mutate(
    mov_spread = home_team - away_team, 
    hfa = mov_spread / 2
  ) %>% 
  arrange(-hfa) %>% 
  select(team, hfa, season)


hfa_team_season1 <- hfa %>%
  gather(location, team, c(home_team, away_team)) %>%
  mutate(margin_of_victory = home_score - away_score) %>%
  group_by(team, location, season) %>%
  summarise(margin_of_victory_mean = mean(margin_of_victory), .groups = "drop") %>%
  mutate(margin_of_victory_mean = ifelse(location == "away_team", margin_of_victory_mean * -1, margin_of_victory_mean)) %>%
  spread(location, margin_of_victory_mean) %>% 
  mutate(
    mov_spread = home_team - away_team, 
    hfa = mov_spread / 2
  ) %>%
  left_join(select(nflfastR::teams_colors_logos, team = team_abbr, team_color, team_color2, team_color3, team_color4))


hfa_team_season1 %>%
  mutate(
    name = glue::glue("<i style='color:{team_color2}'>{team}</i>")
  ) %>%
  ggplot(aes(
    x = season, 
    y = home_team,
    colour = team_color
  )) +
  geom_line(size = 1.25) +
  labs(
    title = "Home Field Advantage",
    subtitle = "Regular Season, 1999:2021",
    x = "Season", 
    y = "Home Field Advantage",
    caption = "data: nflfastR") +
  scale_x_continuous(labels = paste0("'", substr(seq(1999, 2021, 2), 3, 4)), breaks = seq(1999, 2021, 2)) + 
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(vars(name), scales = "free_x", ncol = 8L) +
  becausejustynfun::white_theme() +
  theme(
    strip.text = element_markdown(),
    axis.text = element_text(size = 8)
  )
