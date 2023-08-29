
library(tidyverse, warn.conflicts = FALSE)
future::plan("multisession")

pbp <- purrr::map_df(c(2001:2021), function(x) {
  read_rds(
    glue::glue("~/Documents/nfl/data/pbp/play_by_play_{x}.rds")
  )}) |>
  filter(
    # playoffs is noisy
    season_type == 'REG',
    # drop values that no plays occur e.g. timeout, halftime, etc.
    !is.na(posteam_type),
    # OT is hard
    qtr <= 4
  ) |>
  select(game_id, season, week, posteam_type, season_type, home_team, away_team, home_score, away_score) |>
  group_by(game_id) |>
  # this will give two rows for each game, one for the home team, one for the away team
  #slice(n()) or distinct(game_id, posteam_type, .keep_all = TRUE) if you only want one observation per row
  distinct(.keep_all = TRUE) |> 
  # removing draws since they are so rare - 24 over 21 seasons, (interestingly 2016 and 2018 had 2)
  filter(!home_score == away_score) |> 
  ungroup() |>
  mutate(
    home_result = home_score - away_score, # margin_of_victory
    home_win = ifelse(home_score - away_score > 0, 1, 0) # mean(home_score - away_score > 0)
  )

write_csv(pbp, 'data/hfa_df.csv')