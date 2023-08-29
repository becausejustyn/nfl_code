

# Stan Model

library(tidyverse, warn.conflicts = FALSE)
library(rstan)
future::plan("multisession")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

hfa_df <- read_csv('data/hfa_df.csv') |>
  filter(season >= 2002)

team_id <- read_csv('data/team_id.csv')

# list of teams
team_vars <- distinct(hfa_df, home_team) |> pull()

fits <- lapply(2002:2021,function(y){
  year <- y
  stan_df <- filter(hfa_df, season == year) #, location == "Home"
  mod1_list <- list(
    N_games = stan_df |> nrow(),
    N_teams = 32,
    home_team = stan_df |> 
      pull(home_team) |> match(team_vars),
    away_team = stan_df |> 
      pull(away_team) |> match(team_vars),
    score = stan_df |> 
      pull(home_result)
  )
  
  fit <- stan('models/model1.stan',
              data = mod1_list,
              iter = 2000,
              chains = 3,
              control = list(adapt_delta = 0.99),
              pars = c("score_mean"),
              include = FALSE)
  fit
})

save(team_vars, fits, file = "model_output/mod1.Rdata")

stan_home_results1 <- map_df(2002:2021,function(y){
  fit <- fits[[y-2001]]
  output <- summary(fit,pars = c("alpha","alpha_mean","alpha_sigma"))[['summary']] |>
    as_tibble(rownames = "parameter") |>
    mutate(season = y,
           team = c(team_vars,"ALL","ALL"))
  output
})

write_csv(stan_home_results1, "model_output/stan_home_results1.csv")





fits2 <- lapply(2002:2021,function(y){
  year <- y
  stan_df <- filter(hfa_df, season == year) #, location == "Home"
  mod2_list <- list(
    N_games = stan_df |> nrow(),
    N_teams = 32,
    home_team = pull(stan_df, home_team) |> match(team_vars),
    away_team = pull(stan_df, away_team) |> match(team_vars),
    score = pull(stan_df, home_result)
  )
  
  fit2 <- stan('models/model2.stan',
              data = mod2_list,
              iter = 2000,
              chains = 3,
              control = list(adapt_delta = 0.99),
              pars = c("score_mean"),
              include = FALSE)
  fit2
})

save(team_vars, fits2, file = "model_output/mod2.Rdata")

stan_home_results2 <- map_df(2002:2021,function(y){
  fit <- fits2[[y-2001]]
  output <- summary(fit, pars = c("alpha", "alpha_mean", "alpha_sigma"))[['summary']] |>
    as_tibble(rownames = "parameter") |>
    mutate(season = y,
           team = c(team_vars, "ALL", "ALL"))
  output
})

write_csv(stan_home_results2, "model_output/stan_home_results2.csv")


stan_home_results1 %>%
  filter(team == "ALL") %>%
  mutate(wrap = ifelse(parameter == "alpha_mean","Typical HFA (Points)","Standard Deviation of HFA (Points)"),
         wrap = factor(wrap,levels = unique(wrap)),
         `2.5%` = ifelse(parameter == "alpha_sigma",NA,`2.5%`),
         `97.5%` = ifelse(parameter == "alpha_sigma",NA,`97.5%`)) %>%
  ggplot(aes(x=season,y=mean)) +
  geom_line() + #geom_point() +
  geom_errorbar(aes(ymin = `2.5%`,ymax = `97.5%`),alpha = .5) +
  labs(y = "HFA (Points)",x = "Season") +
  theme_bw() +
  facet_wrap(~ wrap,scales = "free") +
  ggtitle(latex2exp::TeX("HFA Distribution Parameters ($\\alpha_0$ and $\\sigma_{\\alpha}$)"))


all_teams1 <- stan_home_results1 %>%
  left_join(nflfastR::teams_colors_logos,by = c("team" = "team_abbr")) %>%
  left_join(team_id, by = c("team" = "team_abbr")) %>%
  filter(team != "ALL") %>%
  group_by(season) %>%
  mutate(mean_rank = rank(mean)) %>%
  ggplot(aes(x=season,y=mean,color = team)) +
  scale_color_manual(breaks = nflfastR::teams_colors_logos$team_abbr,
                     values = nflfastR::teams_colors_logos$team_color) +
  geom_line() +
  theme_bw() + labs(y = "HFA (Points)",x="Season",title = "Home-Field Advantage by Team and Season") +
  guides(color = "none")


stan_home_results2 %>%
  left_join(nflfastR::teams_colors_logos,by = c("team" = "team_abbr")) %>%
  left_join(team_id, by = c("team" = "team_abbr")) %>%
  filter(team != "ALL") %>%
  group_by(season) %>%
  mutate(mean_rank = rank(mean),n=n()) %>%
  filter(mean_rank %in% c(1,32)) %>%
  mutate(hfa_bw = case_when(mean_rank == 1 ~ "Worst",
                            mean_rank == 32 ~ "Best")) %>%
  ggplot(aes(x=season,y=mean,color = hfa_bw)) +
  # scale_color_manual(breaks = nflfastR::teams_colors_logos$team_abbr,
  #                    values = nflfastR::teams_colors_logos$team_color) +
  geom_line() +
  ggimage::geom_image(aes(image = team_logo_wikipedia.x,color = NULL), size = 0.05, by = "width") +
  theme_bw() + scale_color_brewer(palette = "Set1") + theme(legend.position = "bottom") +
  labs(color = "HFA Rank",x = "Season",y = "HFA (Points)") +
  ggtitle("Best and Worst Home-Field Advantage by Season")

stan_home_results1 %>%
  filter(parameter != "alpha_sigma",parameter != "alpha_mean") %>%
  group_by(team) %>%
  summarise(sd = sd(mean),mean = mean(mean)) %>%
  arrange(-mean) %>% 
  left_join(nflfastR::teams_colors_logos %>% select(team = team_abbr,team_logo_espn),by = "team") %>%
  mutate(team_logo_espn = str_c("![](",team_logo_espn,"){width=30px}")) %>%
  select(team_logo_espn,team,mean,sd) %>% 
  knitr::kable(digits = 2,col.names = c("","Team","HFA (Mean)","HFA (SD)"),align = 'clcc') %>% 
  kableExtra::kable_styling(bootstrap_options = "striped",full_width = F) 


