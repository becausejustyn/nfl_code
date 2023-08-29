
library(tidyverse)

nfl <- read_csv('games_nfl.csv')

nfl1 <- nfl |> 
  select(game_id:week, away_team:home_score, result, total, away_moneyline:over_odds) |> 
  filter(!is.na(away_moneyline))

# see implied win prob corr with moneyline

cor(nfl1[['spread_line']], nfl1[['away_moneyline']]) # 0.9731934

# check range for each spread

american_to_decimal <- function(moneyline){
  odds <- ifelse(
    moneyline > 0,
    # if american odds is positive, 1 plus (the american odds divided by 100)
    1 + (moneyline / 100),
    # if american odds is negative, 1 minus (100 divided by the american odds)
    1 - (100 / moneyline)
    )
  return(odds)
}
  
dec_to_prob <- function(decimal){
  prob <- 1 / decimal
  return(prob)
}

# nfl |> group_by(season) |> summarise(across(c(home_moneyline, away_moneyline), ~ sum(is.na(.))))

nfl1 <- nfl |>
  select(season, game_type, away_score, home_score, location, result, total, away_moneyline:over_odds) |>
  filter(!is.na(home_moneyline)) |>
  mutate(
    away_win_prob = american_to_decimal(away_moneyline) |> dec_to_prob(),
    home_win_prob = american_to_decimal(home_moneyline) |> dec_to_prob()
  ) |>
  select(-c(away_moneyline, home_moneyline))

nfl1 |>
  mutate(
    fav_win_prob = ifelse(away_win_prob > home_win_prob, away_win_prob, home_win_prob)
    ) |>
  select(spread_line, fav_win_prob)

nfl2 <- nfl1 |> 
  select(spread_line, home_win_prob) 


nfl1 |>
  group_by(spread_line) |>
  summarise(
    away_low = min(away_win_prob, na.rm = TRUE),
    away_high = max(away_win_prob, na.rm = TRUE),
    away_mean = mean(away_win_prob, na.rm = TRUE),
    away_sd = sd(away_win_prob, na.rm = TRUE),
    home_low = min(home_win_prob, na.rm = TRUE),
    home_high = max(home_win_prob, na.rm = TRUE),
    home_mean = mean(home_win_prob, na.rm = TRUE),
    home_sd = sd(home_win_prob, na.rm = TRUE)
  ) |> View()


mod <- lm(spread_line ~ home_win_prob, data = nfl2)

mod2 <- lm(home_win_prob ~ spread_line, data = nfl2)

get_formula <- function(model) {
  
  broom::tidy(model)[, 1:2] %>%
    mutate(sign = ifelse(sign(estimate) == 1, ' + ', ' - ')) %>% #coeff signs
    mutate_if(is.numeric, ~ abs(round(., 2))) %>% #for improving formatting
    mutate(a = ifelse(term == '(Intercept)', paste0('y ~ ', estimate), paste0(sign, estimate, ' * ', term))) %>%
    summarise(formula = paste(a, collapse = '')) %>%
    as.character
  
}

get_formula(mod)
# y ~ 16.35 + 31.86 * home_win_prob

new <- data.frame(home_win_prob = seq(0.05, 0.95, 0.05))

predict(mod, new, se.fit = TRUE)[['fit']] |> round(.5)

library(purrr)

predict(mod, new, se.fit = TRUE)[['fit']] |> map_dbl(., ceiling(./2)*2)

sam_data <- data.frame(home_win_prob = c(.81, .6, .95, .67, .8, .4))

predict(mod, newdata = data.frame(home_win_prob = seq(0.05, 0.95, 0.05)), se.fit = TRUE)[['fit']] %>% {round(. * 2) / 2}


predict(mod, newdata = sam_data, se.fit = TRUE)[['fit']] %>% {round(. * 2) / 2}


lines <- data.frame(spread_line = seq(0, 14, .5))

predict(mod2, newdata = lines, se.fit = TRUE)[['fit']] %>% {round(. * 2) / 2} 

lines %>%
  mutate(
    win_prob = predict(mod2, newdata = spread_line)[['fit']] %>% {round(. * 2) / 2}
  )

tibble(
  spread_line = seq(0, 14, .5)
) %>%
  mutate(
    win_prob = predict(mod2, newdata = spread_line)[['fit']] %>% {round(. * 2) / 2}
  )
library(broom)

df <- augment(mod2, newdata = lines) |>
  rename(win_prob = .fitted)

