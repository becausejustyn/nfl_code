
library(tidyverse, warn.conflicts = FALSE)
library(broom)
library(ggtext)
library(ggrepel)
library(gghighlight)
# library(RColorBrewer) 
# rdgy <- brewer.pal(n = 11, name = "RdGy") # display.brewer.pal(11, "RdGy")
# library(gt)
# library(gtExtras)

future::plan("multisession")
options(scipen = 999)

team_id <- read_csv('data/team_id.csv')
pbp <- read_csv('data/hfa_df.csv')

pbp[['home_result']] |> mean() # 2.197878

### League by Season ----

hfa_league_season <- pbp |>
  distinct(game_id, .keep_all = TRUE) |>
  mutate(score_diff = home_score - away_score) |>
  group_by(season) |>
  summarise(
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score)
  ) |> 
  mutate(mean_point_diff = home_points_diff / n)

hfa_league_season |>
  ggplot(aes(x = season, weight = mean_point_diff)) +
  geom_bar() +
  scale_x_continuous(
    breaks = seq(2000, 2021, 1),
    labels = paste0("'", substr(c(2001:2022), 3, 4))) +
  scale_y_continuous(breaks = seq(-.5, 4, .5)) +
  theme_minimal()

### League by Season, Week ----

hfa_league_season_week <- pbp |>
  distinct(game_id, .keep_all = TRUE) |>
  mutate(score_diff = home_score - away_score) |>
  group_by(season, week) |>
  summarise(
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score),
    .groups = 'drop') |> 
  mutate(
    mean_point_diff = home_points_diff / n,
    week_n = row_number()
  ) 

hfa_league_season_week |>
  ggplot(aes(x = week, y = mean_point_diff, group = season)) +
  geom_line(size = 0.5, colour = "#112446") +
  scale_x_continuous(breaks = seq(1, 18, 1)) +
  theme_minimal()

hfa_league_season_week |>
  ggplot(aes(x = factor(week), weight = mean_point_diff)) +
  geom_bar() +
  theme_minimal()

hfa_league_season |> 
  ggplot(aes(
    x = as_factor(season), y = mean_point_diff, group = 1)) +
  geom_bar(stat = "identity", fill = "azure3", col = "white") +
  geom_hline(yintercept = pbp[['home_result']] |> mean(), linetype = 'dashed', colour = 'black', size = .75, alpha = 0.7) +
  labs(
    x = "Season", 
    y = "Home Field Advantage",
    subtitle = "Seasons 2001 - 2021",
    caption = "data: nflfastR") +
  scale_x_discrete(labels = paste0("'", substr(seq(2001, 2021, 1), 3, 4)), breaks = seq(2001, 2021, 1)) + 
  scale_y_continuous(breaks = seq(-0.5, 4, 0.5)) 
#ggsave("hfa_league.jpg", plot = last_plot(), dpi = 600)

## HFA by team ----

home_df <- pbp |>
  filter(posteam_type == 'home') |>
  group_by(home_team, season) |>
  summarise(
    n = n(),
    pts_scored = sum(home_score),
    pts_allowed = sum(away_score),
    home_win = mean(home_score - away_score > 0),
    .groups = 'drop'
  )

away_df <- pbp |>
  filter(posteam_type == 'away') |>
  group_by(away_team, season) |>
  summarise(
    n = n(),
    pts_scored = sum(away_score),
    pts_allowed = sum(home_score),
    home_win = mean(away_score - home_score > 0),
    .groups = 'drop'
  )




hfa_team <- pbp |>
  gather(posteam_type, team, c(home_team, away_team)) |>
  mutate(margin_of_victory = home_score - away_score) |>
  group_by(team, posteam_type) |>
  summarise(
    margin_of_victory_mean = mean(margin_of_victory), .groups = "drop") |>
  mutate(
    margin_of_victory_mean = ifelse(posteam_type == "away_team", margin_of_victory_mean * -1, margin_of_victory_mean)) |>
  spread(posteam_type, margin_of_victory_mean) |> 
  mutate(
    mov_spread = home_team - away_team, 
    hfa = mov_spread / 2
  ) |> 
  arrange(-hfa) |> 
  select(team, hfa) |>
  left_join(select(team_id, team = team_abbr, team_color, team_color2))

hfa_team |>
  ggplot(aes(
    x = reorder(team, hfa), y = hfa,
    fill = team_color, colour = team_color2)) +
  geom_point(size = 2.5) +
  coord_flip() +
  labs(
    x = "Team",
    y = "Home Field Advantage",
    title = "Home Field Advantage by Team",
    subtitle = "Regular Season, 2001 to 2021",
    caption = "data: nflfastR") +
  scale_y_continuous(breaks = seq(0.5, 4, 0.5)) +
  scale_color_identity(aesthetics = c("fill", "color")) +
  geom_hline(aes(yintercept = mean(hfa)), linetype = 2, col = "#FF6E6E") +
  geom_text(aes(x = 4, y = 2.75, label = "League Average"), stat = "unique", check_overlap = TRUE, colour = "#DB4848", size = 5, fontface = "bold") +
  theme(
    panel.grid.major = element_line(colour = "gray90"),
    panel.grid.minor = element_line(colour = "gray90"),
    axis.title = element_text(family = "sans")
  )

## HFA by Team and Season ----

league_wide_win_team <- pbp |>
  group_by(season, home_team) |>
  summarise(
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score),
    .groups = 'drop') |> 
  mutate(
    mean_point_diff = home_points_diff / n,
    # so you can plot is sequentially
    week_n = row_number()) |>
  # add team colours for plotting
  left_join(select(team_id, home_team = team_abbr, team_name, team_color, team_color2))

hfa_season_team <- hfa |>
  gather(location, team, c(home_team, away_team)) |>
  mutate(
    margin_of_victory = home_score - away_score,
    home_win = ifelse(home_score > away_score, 1, 0)
    ) |>
  group_by(team, location, season) |>
  summarise(
    margin_of_victory_mean = mean(margin_of_victory), .groups = "drop") |>
  mutate(margin_of_victory_mean = ifelse(location == "away_team", margin_of_victory_mean * -1, margin_of_victory_mean)) |>
  spread(location, margin_of_victory_mean) |> 
  mutate(
    mov_spread = home_team - away_team, 
    hfa = mov_spread / 2) |>
  left_join(select(team_id, team = team_abbr, team_color, team_color2, team_color3, team_color4))

hfa_season_team |>
  mutate(name = glue::glue("<i style='color:{team_color2}'>{team}</i>")) |>
  ggplot(aes(
    x = season, y = home_team,
    colour = team_color)) +
  geom_line(size = 1.25) +
  labs(
    title = "Home Field Advantage",
    subtitle = "Regular Season, 2001:2021",
    x = "Season", 
    y = "Home Field Advantage",
    caption = "data: nflfastR") +
  scale_x_continuous(labels = paste0("'", substr(seq(2001, 2021, 2), 3, 4)), breaks = seq(2001, 2021, 2)) + 
  scale_color_identity(aesthetics = c("fill", "color")) +
  facet_wrap(vars(name), scales = "free_x", ncol = 8L) +
  theme(
    strip.text = element_markdown(),
    axis.text = element_text(size = 8))


league_wide_win_team |>
  mutate(
    #LV colour is grey so I am using their secondary colour here
    team_color = ifelse(team_color == "#a5acaf", "#000000", team_color),
    season_label = paste0("'", substr(season, 3, 4)),
    season_label = as_factor(season_label)
  ) |>
  ggplot(aes(
    # the label is a character, so get 99 at the start
    x = fct_reorder(season_label, season), y = mean_point_diff,
    colour = team_color, group = home_team,)) +
  geom_line(size = 1.15) +
  scale_x_discrete(breaks = paste0("'", seq(1999, 2021, 3) |> substr(3, 4))) +
  gghighlight::gghighlight(use_direct_label = FALSE, keep_scales = TRUE) +
  scale_color_identity(aesthetics = c("color", "fill")) +
  facet_wrap(~home_team, scales = "free", ncol = 5L) +
  labs(
    x = "Season",
    y = "HFA",
    title = "HFA By Team",
    subtitle = "Regular Season, 1999:2021",
    caption = "data: nflfastR") +
  theme(axis.text = element_text(size = 6))

league_wide_win_team |>
  mutate(season_label = paste0(substr(season, 3, 4), "/", substr(season+1, 3, 4))) |>
  ggplot(aes(
    x = factor(season_label), 
    y = mean_point_diff, group = 1)) +
  geom_line(aes(group = factor(home_team)), alpha = 1/2) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  scale_y_continuous(breaks = seq(-22, 24, 4)) +
  scale_x_discrete(labels = paste0("'", substr(c(2001:2021), 3, 4))) +
  labs(
    x = "Season",
    y = "HFA"
  ) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  hrbrthemes::theme_ft_rc()

league_wide_win_team |>
  ggplot(aes(
    x = factor(season), y = mean_point_diff, 
    group = 1)) +
  geom_line(aes(group = factor(home_team)), alpha = 0.5) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  scale_y_continuous(breaks = seq(-22, 24, 4)) +
  scale_x_discrete(labels = paste0("'", substr(c(1999:2021), 3, 4))) +
  labs(
    x = "Season",
    y = "HFA",
    title = "Home Field Advantage",
    subtitle = "Regular Season, 1999:2021",
    caption = "data: nflfastR \nBlue line is the mean HFA by season.") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) 

### Modelling/Hypothesis Testing ----

mod <- lm(mean_point_diff ~ factor(season), data = league_wide_win_team)

broom::tidy(mod) |> 
  gt::gt() |> 
  fmt_number(
    columns = c(2:5), 
    decimals = 3)

league_wide_win_team |>
  ggplot(aes(
    x = factor(season), y = mean_point_diff, group = 1)) +
  geom_line(aes(group = factor(home_team)), alpha = 1/2) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  labs(
    x = "Season",
    y = "HFA",
    subtitle = "Regular Season, 1999:2021",
    caption = "data: nflfastR \n Pink line is the linear trend over time"
  ) +
  stat_smooth(method = 'lm', col = '#984ea3', se = FALSE, size = 1) +
  scale_x_discrete(labels = paste0("'", substr(c(1999:2021), 3, 4))) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  hrbrthemes::theme_ft_rc()

league_wide_win2 |>
  ggplot(aes(
    x = factor(season), 
    y = mean_point_diff, 
    group = 1)) +
  geom_line(aes(group = factor(week)), alpha = 1/2) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  labs(
    x = "Season",
    y = "Mean Home Point Differential"
  ) +
  stat_smooth(method = 'lm', col = '#984ea3', se = FALSE, size = 1) +
  scale_x_discrete(labels = paste0("'", substr(c(2001:2021), 3, 4))) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  hrbrthemes::theme_ft_rc()

# Step 4a: Remove this linear trend and focus on residuals
# This will examine teams' performance after controlling for this decrease in
# hfa in the last decade

league_wide_win_team <- league_wide_win_team |>
  mutate(rel_hfa = resid(mod))

best_hfa <- league_wide_win_team |> 
  group_by(home_team) |>
  summarise(
    mean = mean(rel_hfa),
    sd = sd(rel_hfa),
    sem = sd(rel_hfa)/sqrt(n()),
    tpval = t.test(rel_hfa)[['p.value']]
    #tpval = t.test(rel_hfa)$p.value
  ) |>
  mutate(sig = tpval < .05)

best_hfa2 <- league_wide_win2 |> 
  group_by(team) |>
  summarise(
    mean = mean(rel_ppg),
    sd = sd(rel_ppg),
    sem = sd(rel_ppg)/sqrt(n()),
    tpval = t.test(rel_ppg)[['p.value']]
  ) |>
  mutate(sig = tpval < .05)

# Step 4b: Average the residuals for each team over the past decade. Then,
# perform a one-sample t-test (i.e., from zero) to see teams that were above
# or below the average power play goals/game despite the decrease in 
# power play goals in the last decade  

best_hfa |>
  gt() |>
  fmt_number(columns = c(2:5), decimals = 3) |>
  cols_label(
    home_team = "Home Team",
    mean = 'Mean',
    sd = 'SD',
    sem = 'SEM',
    tpval = "T P-val",
    sig = 'Sig'
  ) 

# Step 3b: Plot this linear trend over time
league_wide_win_team |>
  mutate(season_label = paste0(substr(season, 3, 4), "/", substr(season+1, 3, 4))) |>
  ggplot(aes(
    x = factor(season_label), 
    y = mean_point_diff, group = 1)) +
  geom_line(aes(group = factor(home_team)), alpha = 1/2) + 
  geom_line(stat = 'summary', fun = 'mean', colour = 'blue', size = 1) +
  scale_x_discrete(labels = paste0("'", substr(c(2001:2021), 3, 4))) +
  labs(
    x = "Season",
    y = "HFA"
  ) +
  stat_smooth(method = 'lm', col = '#984ea3', se = FALSE, size = 1) +
  theme(axis.text.x = element_text(angle = 15, hjust = 1)) +
  hrbrthemes::theme_ft_rc()

### Plot ----

# Step 4c: Plot the average residuals, their standard error of the mean,
# and color based on if significantly different from 0 (i.e., average PPG/Game)

# y.axis bold conditional 
best_hfa_sort <- arrange(best_hfa, mean)                       
axisFace <- if_else(best_hfa_sort[['sig']] == TRUE, 'bold', 'plain')

# shading conditional 
above <- filter(best_hfa, mean > 0, sig == TRUE) |> 
  arrange(mean)

average <- best_hfa |> 
  filter(sig == FALSE)

below <- filter(best_hfa, mean < 0, sig == TRUE) |> 
  arrange(mean)

best_hfa |>
  ggplot(aes(
    x = mean, y = fct_reorder(home_team, mean),
    colour = sig)) +
  geom_errorbarh(aes(
    xmin = mean - sem,
    xmax = mean + sem)) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c("grey", "black"),
    guide = guide_legend(
      reverse = TRUE,
      title = "Above/Below Average",
      title.position = "top")
  ) +
  scale_x_continuous(breaks = seq(-7.5, 9, 1.5)) +
  labs(
    x = "Mean HFA (Residuals)",
    y = "NFL Team",
    title = "NFL Home Field Advantage 1999-2021",
    caption = "Error bars are SEM. \n TRUE is statistical signifance.") +
  theme(
    axis.text.y = element_text(face = axisFace),
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom",
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.key.width = unit(1, "in"),
    legend.key.height = unit(.5, "in"),
    legend.title.align = .5,
    axis.title.x = element_text(vjust = -1)) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymax = above[['home_team']][nrow(above)],
           ymin = above[['home_team']][1],
           fill = "#486e48",
           colour = NA,
           alpha = 1 / 3) +
  annotate("rect",
           xmin = -Inf, xmax = Inf,
           ymax = below[['home_team']][nrow(below)],
           ymin = below[['home_team']][1],
           fill = "#346991",
           colour = NA,
           alpha = 1 / 3) +
  annotate("text", x = -.1, y = above[['home_team']][3], label = "Best Teams") +
  annotate("text", x = .1, y = below[['home_team']][3], label = "Worst Teams")

# league wide
league_win <- pbp |>
  distinct(game_id, .keep_all = TRUE) |>
  mutate(score_diff = home_score - away_score) |>
  group_by(season) |>
  summarise(
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score)
  ) |> 
  mutate(mean_point_diff = home_points_diff / n)

#season, week wide
weekly_team_win <- pbp |>
  distinct(game_id, .keep_all = TRUE) |>
  mutate(score_diff = home_score - away_score) |>
  group_by(season, week, home_team) |>
  summarise(
    away_team,
    home_win = mean(home_score - away_score > 0),
    home_result = sum(home_score - away_score),
    .groups = "drop") 

weekly_team_win1 <- pbp |>
  distinct(game_id, .keep_all = TRUE) |>
  mutate(score_diff = home_score - away_score) |>
  group_by(season, week) |>
  summarise(
    team = home_team,
    n = n(),
    home_win = mean(home_score - away_score > 0),
    home_points_diff = sum(home_score - away_score),
    .groups = "drop") |> 
  mutate(
    mean_point_diff = home_points_diff / n,
    week_n = row_number()
  ) 

weekly_team_win <- pbp |>
  distinct(game_id, .keep_all = TRUE) |>
  mutate(score_diff = home_score - away_score) |>
  group_by(season, week, home_team) |>
  summarise(
    away_team,
    home_win = mean(home_score - away_score > 0),
    home_result = sum(home_score - away_score),
    .groups = "drop") 