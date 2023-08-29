

# I want to first look at if a team covers

library(tidyverse)
library(ggtext) 
library(glue)
future::plan('multisession')

pbp <- purrr::map_df(c(1999:2021), function(x) {
  read_rds(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )
}) %>%
  filter(season_type == 'REG')

pbp %>%
  mutate(dist_line = score_differential - spread_line) %>%
  filter(!is.na(dist_line)) %>%
  ggplot(aes(x = dist_line)) +
  geom_histogram(aes(y = ..density..), bins = 30L) +
  stat_function(fun = dnorm,
                geom = "line",
                size = 2,
                colour = "steelblue",
                args = list(
                  mean = -3.453258,
                  sd = 12.34763
                )) +
  scale_x_continuous(breaks = seq(-60, 60, 10)) +
  labs(
    y = "Frequency",
    x = "Distance from Actual Spread",
    caption = "data: nflfastR"
  )

spread_df <- pbp %>%
  filter(!is.na(posteam_type)) %>% 
  mutate(dist_line = score_differential - spread_line) %>%
  select(game_id, spread_line, result, dist_line, home_team, away_team) %>%
  distinct(game_id, .keep_all = TRUE) %>% # include posteam_type if you want double weeks
  group_by(home_team) %>%
  mutate(
    home_covered = if_else(-1 * spread_line + result > 0, 1, 0),
    away_covered = if_else(-1 * spread_line + result < 0, 1, 0),
    push = ifelse(spread_line == result, 1, 0),
    cover_result = case_when(
      -1 * spread_line + result > 0 ~ 'home_covered',
      -1 * spread_line + result < 0 ~ 'away_covered',
      spread_line == result ~ 'push')
  ) %>%
  ungroup()

# just to check that there is exact 2 0's and 1 1 across those 3 columns for each game 
# spread_df |> group_by(game_id) |> summarise(cover_sum = home_covered + away_covered + push) |> arrange(-cover_sum)


spread_df |>
  ggplot(aes(x = dist_line)) +
  geom_histogram(aes(y = ..density..), bins = 30L) +
  stat_function(fun = dnorm, # away
                geom = "area",
                size = 2,
                fill = "#386cb0",
                alpha = 0.5,
                args = list(
                  mean = -1.44,
                  sd = 5.96
                )) +
  stat_function(fun = dnorm, # home
                geom = "area",
                size = 2,
                fill = "#7fc97f",
                alpha = 0.5,
                args = list(
                  mean = -1.72,
                  sd = 7.90
                )) +
  scale_x_continuous(breaks = seq(-60, 60, 10)) +
  labs(
    y = "Frequency",
    x = "Distance from Actual Spread",
    caption = "data: nflfastR"
  )






df <- pbp %>%
  filter(season_type == 'REG') %>%
  group_by(game_id, home_team, away_team) %>%
  select(result:total_line) %>%
  distinct(game_id, .keep_all = TRUE) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(home_team, away_team),
    values_to = "team",
    names_to = "home_away"
  ) %>%
  mutate(
    across(game_id, stringr::str_replace, "2021_", ""),
    week = substr(game_id, 1, 2),
    week = as.numeric(week)
  ) %>%
  left_join(nflfastR::teams_colors_logos, by = c("team" = "team_abbr"))

df %>%
  mutate(name = glue::glue("<i style='color:{team_color}'>{team}</i>")) %>%
  ggplot(aes(
    x = week, 
    y = spread_line, 
    group = 1, 
    colour = team_color)) +
  geom_line() +
  scale_color_identity(aesthetics = c('fill', 'colour')) + 
  scale_x_continuous(breaks = seq(1, 18, 2)) +
  facet_wrap(vars(name), scales = "free_x") +
  becausejustynfun::white_theme() +
  theme(
    strip.text = element_markdown()
  ) +
  labs(
    x = "Week",
    y = "Spread",
    caption = "data: nflfastR"
  )

df %>%
  mutate(name = glue::glue("<i style='color:{team_color}'>{team}</i>")) %>%
  ggplot(aes(
    x = week, 
    y = spread_line, 
    group = 1, 
    colour = team_color)) +
  geom_line() +
  geom_hline(aes(yintercept = 0, colour = team_color2), linetype = "dashed") +
  scale_color_identity(aesthetics = c('fill', 'colour')) + 
  scale_x_continuous(breaks = seq(1, 18, 2)) +
  facet_wrap(vars(name), scales = "free_x", ncol = 5) +
  becausejustynfun::white_theme() +
  theme(
    strip.text = element_markdown()
  ) +
  labs(
    x = "Week",
    y = "Spread",
    caption = "data: nflfastR"
  )


pbp_2021 <- pbp %>%
  select(game_id, season, home_team, home_score, away_team, away_score, week, spread_line, game_date) %>%
  filter(season == 2021) %>%
  group_by(game_id) %>%
  #keeps only the last row
  slice(n()) %>%
  summarise(
    winner = if_else(home_score > away_score, home_team, away_team),
    loser = if_else(away_score < home_score, away_team, home_team),
    date = as.Date(game_date),
    play_week = week, 
    spread_line = spread_line,
    points_winner = if_else(home_score > away_score, home_score, away_score),
    points_loser = if_else(away_score < home_score, away_score, home_score),
    score_delta = points_winner - points_loser,
    # did the home team win
    is_home = if_else(home_score > away_score, 1, -1),
    #adjust this with more data
    period = season,
    .groups = "drop"
  ) %>%
  select(-game_id)

pbp_2021 %>%
  ggplot(aes(
    x = score_delta
  )) +
  geom_histogram(bins = 30L) +
  scale_x_continuous(breaks = seq(0, 35, 7)) +
  labs(
    x = "Score Difference",
    y = "Frequency"
  )

pbp_2021 %>%
  ggplot(aes(
    x = spread_line
  )) +
  geom_histogram(bins = 30L) +
  scale_x_continuous(breaks = seq(-21, 21, 7), sec.axis = sec_axis(trans = ~ ., breaks = seq(-17, 22, 3))) +
  labs(
    x = "Spread",
    y = "Frequency"
  )