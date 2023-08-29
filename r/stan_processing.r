

### Stan

library(tidyverse, warn.conflicts = FALSE)
library(rstan)
future::plan("multisession")
options(mc.cores = parallel::detectCores())

# created in the previous script
hfa_df <- read_csv('data/hfa.csv')

# list of teams
team_vars <- distinct(hfa_df, home_team) |> pull()

fits <- lapply(2002:2021,function(y){
  year <- y
  stan_df <- hfa_df |> 
    filter(
      season == year #, location == "Home"
    )
  stan_list <- list(
    N_games = stan_df |> nrow,
    N_teams = 32,
    home_team = stan_df |> 
      pull(home_team) |> match(team_vars),
    away_team = stan_df |> 
      pull(away_team) |> match(team_vars),
    score = stan_df |> 
      pull(home_result)
  )
  
  fit <- stan('posts/nfl_hfa/season_model.stan',
              data = stan_list,
              iter = 2000,
              chains = 3,
              control = list(adapt_delta = 0.99),
              pars = c("score_mean"),
              include = FALSE)
  fit
})

if(!dir.exists("posts/nfl_hfa/output")) dir.create("posts/nfl_hfa/output")
save(team_vars, fits, file = "posts/nfl_hfa/output/team_effects_default_priors.Rdata")

load("output/team_effects_default_priors.Rdata")

stan_home_results <- map_df(1999:2021,function(y){
  fit <- fits[[y-1998]]
  output <- summary(fit,pars = c("alpha","alpha_mean","alpha_sigma"))$summary |>
    as_tibble(rownames = "parameter") |>
    mutate(season = y,
           team = c(team_vars,"ALL","ALL"))
  output
})

write_csv(stan_home_results, "stan_home_results.csv")

