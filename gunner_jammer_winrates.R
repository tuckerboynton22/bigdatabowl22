library(tidyverse)
library(gganimate)
library(ggthemes)
library(gt)
options(scipen = 9999)
options(remove(list=ls()))

## Read in data
# Note: I went one season at a time for the sake of my machine
df_tracking <- read_csv("tracking2018.csv")

player_data <- read_csv("players.csv")

# Only want plays with certain outcomes
plays <- read_csv("bdb-2022/nfl-big-data-bowl-2022/plays.csv") %>%
  mutate(id = paste0(gameId, playId)) %>%
  filter(specialTeamsResult == "Return" | specialTeamsResult == "Fair Catch" |
           specialTeamsResult == "Touchback" | specialTeamsResult == "Downed" |
           specialTeamsResult == "Muffed" | specialTeamsResult == "Out of Bounds",
         is.na(penaltyCodes), specialTeamsPlayType == "Punt")

pff_scouting <- read_csv("bdb-2022/nfl-big-data-bowl-2022/PFFScoutingData.csv") %>%
  mutate(id = paste0(gameId, playId)) %>%
  filter(!is.na(snapDetail), !is.na(hangTime))

# Fix orientation so all plays go left
df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y),
         o = ifelse(playDirection == "left", 360 - o, o))

# Trim data frame to include only relevant rows, determining offensive/defensive
# player by orientation at snap
df_tracking_trimmed <- df_tracking %>%
  mutate(id = paste0(gameId, playId)) %>%
  mutate(
    id = paste0(gameId, playId),
    is_football = ifelse(displayName == "football", 1, 0),
    ball_snapped = ifelse(event == "ball_snap", 1, 0),
    punt_received = ifelse(event == "punt_received" | event == "punt_land" | event == "punt_muffed", 1, 0)
  ) %>%
  merge(plays) %>%
  filter(specialTeamsPlayType == "Punt") %>%
  group_by(id, nflId) %>%
  mutate(
    player_start_x = max(x*ball_snapped),
    player_start_y = max(y*ball_snapped),
    player_start_o = max(o*ball_snapped)
  ) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    start_ball_x = max(is_football*ball_snapped*x),
    start_ball_y = max(is_football*ball_snapped*y),
    end_ball_x = max(is_football*punt_received*x),
    end_ball_y = max(is_football*punt_received*y),
    play_start_frame = max(frameId*ball_snapped),
    frame_since_start = frameId - play_start_frame,
    play_end_frame = max(frameId*punt_received)
  ) %>%
  ungroup() %>%
  filter(frame_since_start == 35) %>%
  merge(pff_scouting) %>%
  mutate(
    defteam = word(vises, start=1, end=1),
    posteam = word(gunners, start=1, end=1),
    team_number = case_when(
      between(player_start_o, 0, 180) ~ paste0(posteam, " ", jerseyNumber),
      between(player_start_o, 180, 360) ~ paste0(defteam, " ", jerseyNumber))
  ) %>%
  filter(
    stringr::str_detect(vises, paste0(team_number, "$")) |
      stringr::str_detect(gunners, paste0(team_number, "$")) |
      stringr::str_detect(vises, paste0(team_number, "\\;")) |
      stringr::str_detect(gunners, paste0(team_number, "\\;"))
  ) %>%
  mutate(
    jammer = ifelse(stringr::str_detect(vises, team_number), 1, 0),
    gunner = ifelse(stringr::str_detect(gunners, team_number), 1, 0),
    jammer = ifelse(is.na(jammer), 0, jammer),
    gunner = ifelse(is.na(gunner), 0, gunner),
    player_type = case_when(
      gunner & start_ball_y < player_start_y ~ "top_gunner",
      gunner & start_ball_y > player_start_y ~ "bottom_gunner",
      jammer & start_ball_y < player_start_y ~ "top_jammer",
      jammer & start_ball_y > player_start_y ~ "bottom_jammer",
    ),
    top_gunner = ifelse(player_type == "top_gunner", 1, 0),
    bottom_gunner = ifelse(player_type == "bottom_gunner", 1, 0),
    top_jammer = ifelse(player_type == "top_jammer", 1, 0),
    bottom_jammer = ifelse(player_type == "bottom_jammer", 1, 0),
    top = ifelse(top_gunner | top_jammer, 1, 0),
    bottom = ifelse(bottom_gunner | bottom_jammer, 1, 0)
  ) %>%
  select(id:playDirection, playDescription, kickLength,
         kickReturnYardage, player_start_x:play_end_frame,
         hangTime, defteam:bottom)

# Group player performance at play level
df_tracking_grouped <- df_tracking_trimmed %>%
  group_by(id, frameId) %>%
  mutate(
    top_jammers = sum(top_jammer),
    bottom_jammers = sum(bottom_jammer),
    top_gunners = sum(top_gunner),
    bottom_gunners = sum(bottom_gunner)
  ) %>%
  ungroup() %>%
  mutate(
    num_jammers_on_side = ifelse(top, top_jammers, bottom_jammers),
    num_gunners_on_side = ifelse(bottom, bottom_gunners, top_gunners)
  ) %>%
  group_by(id, frameId) %>%
  mutate(
    max_x_top_jammer = max(x*top_jammer),
    max_x_top_gunner = max(x*top_gunner),
    max_x_bottom_jammer = max(x*bottom_jammer),
    max_x_bottom_gunner = max(x*bottom_gunner),
  ) %>%
  ungroup() %>%
  group_by(id, nflId, frameId) %>%
  mutate(
    win = case_when(
      bottom_jammer & max_x_bottom_gunner < x ~ 1,
      top_jammer & max_x_top_gunner < x ~ 1,
      bottom_gunner & max_x_bottom_jammer < x ~ 1,
      top_gunner & max_x_top_jammer < x ~ 1,
      TRUE ~ 0
    )
  )

df_tracking_2018 <- df_tracking_grouped

# Merge all data frames together after running above for 2018, 2019, 2020
# Again, only have to do this if you go one-by-one
plays_tracking <- rbind(df_tracking_2018, df_tracking_2019) %>% rbind(df_tracking_2020)

# Verify that number of gunners/jammers match for PFF & tracking
valid_plays <- plays_tracking %>%
  merge(pff_scouting) %>%
  group_by(id) %>%
  summarize(
    desc = first(playDescription),
    returnyds = first(kickReturnYardage),
    kickyds = first(kickLength),
    n_gunners_tracking = sum(gunner, na.rm = T),
    n_gunners_pff = ifelse(is.na(first(gunners)), 0, stringr::str_count(first(gunners), ";") + 1),
    n_jammers_tracking = sum(jammer, na.rm = T),
    n_jammers_pff = ifelse(is.na(first(vises)), 0, stringr::str_count(first(vises), ";") + 1)
  ) %>%
  mutate(jammers_diff = n_jammers_tracking - n_jammers_pff,
         gunners_diff = n_gunners_tracking - n_gunners_pff) %>%
  filter(gunners_diff == 0, jammers_diff == 0) %>%
  select(id)

valid_plays_tracking <- merge(valid_plays, plays_tracking)

# Summarize performance in plots
# Gunners
gunner_performance <- valid_plays_tracking %>%
  filter(jammer == 0, num_gunners_on_side == 1, num_jammers_on_side <= 2) %>%
  mutate(dt = ifelse(num_jammers_on_side == 2, 1, 0)) %>%
  group_by(nflId) %>%
  summarize(
    win_rate = mean(win, na.rm = T),
    dt_rate = mean(dt, na.rm = T),
    n = n()
  ) %>%
  filter(n >= 75) %>%
  merge(player_data)

gunner_performance %>%
  ggplot(aes(dt_rate, win_rate)) +
  geom_text(label = gunner_performance$displayName) +
  ggthemes::theme_fivethirtyeight() +
  labs(
    x = "Double-Team Rate",
    y = "Gunner Win Rate",
    title = "Gunner Win vs. Double-Team Rate, 2018-20",
    subtitle = "min. 75 qualifying snaps"
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  xlim(0.1, 0.405)

# Summarize performance in plots
# Gunners
jammer_performance <- valid_plays_tracking %>%
  filter(jammer == 1, num_gunners_on_side == 1, num_jammers_on_side <= 2) %>%
  mutate(dt = ifelse(num_jammers_on_side == 2, 1, 0)) %>%
  group_by(nflId) %>%
  summarize(
    win_rate = mean(win, na.rm = T),
    dt_rate = mean(dt, na.rm = T),
    n = n()
  ) %>%
  filter(n >= 70) %>%
  merge(player_data)

jammer_performance %>%
  ggplot(aes(dt_rate, win_rate)) +
  geom_text(label = jammer_performance$displayName) +
  ggthemes::theme_fivethirtyeight() +
  labs(
    x = "Double-Team Rate",
    y = "Jammer Win Rate",
    title = "Jammer Win vs. Double-Team Rate, 2018-20",
    subtitle = "min. 70 qualifying snaps"
  ) +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10)
  ) +
  xlim(0.1, 0.43)

# Summarize performance in tables
# Gunners
gunner_performance_1t <- valid_plays_tracking %>%
  filter(jammer == 0, num_gunners_on_side == 1, num_jammers_on_side == 1) %>%
  group_by(nflId) %>%
  summarize(
    win_rate = mean(win, na.rm = T),
    n = n()
  ) %>%
  merge(player_data) %>%
  filter(n >= 25) %>%
  arrange(desc(win_rate)) %>%
  head(15)

gunner_performance_1t %>%
  select(displayName, n, win_rate) %>%
  gt() %>%
  cols_label(
    displayName = "Gunner",
    n = "# Snaps",
    win_rate = "Win Rate"
  ) %>%
  gtExtras::gt_theme_538() %>%
  tab_header(
    title = "Best Single-Coverage Gunner Win Rates, 2018-20",
    subtitle = "min. 25 qualifying snaps"
  ) %>%
  fmt_percent(columns = 3, decimals = 1)

gunner_performance_2t <- valid_plays_tracking %>%
  filter(jammer == 0, num_gunners_on_side == 1, num_jammers_on_side == 2) %>%
  group_by(nflId) %>%
  summarize(
    win_rate = mean(win, na.rm = T),
    n = n()
  ) %>%
  merge(player_data) %>%
  filter(n >= 15) %>%
  arrange(desc(win_rate)) %>%
  head(15)

gunner_performance_2t %>%
  select(displayName, n, win_rate) %>%
  gt() %>%
  cols_label(
    displayName = "Gunner",
    n = "# Snaps",
    win_rate = "Win Rate"
  ) %>%
  gtExtras::gt_theme_538() %>%
  tab_header(
    title = "Best Double-Coverage Gunner Win Rates, 2018-20",
    subtitle = "min. 15 qualifying snaps"
  ) %>%
  fmt_percent(columns = 3, decimals = 1)

# Summarize performance in tables
# Jammers
jammer_performance_1t <- valid_plays_tracking %>%
  filter(jammer == 1, num_gunners_on_side == 1, num_jammers_on_side == 1) %>%
  group_by(nflId) %>%
  summarize(
    win_rate = mean(win, na.rm = T),
    n = n()
  ) %>%
  merge(player_data) %>%
  filter(n >= 25) %>%
  arrange(desc(win_rate)) %>%
  head(15)

jammer_performance_1t %>%
  select(displayName, n, win_rate) %>%
  gt() %>%
  cols_label(
    displayName = "Jammer",
    n = "# Snaps",
    win_rate = "Win Rate"
  ) %>%
  gtExtras::gt_theme_538() %>%
  tab_header(
    title = "Best Single-Coverage Jammer Win Rates, 2018-20",
    subtitle = "min. 25 qualifying snaps"
  ) %>%
  fmt_percent(columns = 3, decimals = 1)

jammer_performance_2t <- valid_plays_tracking %>%
  filter(jammer == 1, num_gunners_on_side == 1, num_jammers_on_side == 2) %>%
  group_by(nflId) %>%
  summarize(
    win_rate = mean(win, na.rm = T),
    n = n()
  ) %>%
  merge(player_data) %>%
  filter(n >= 20) %>%
  arrange(desc(win_rate)) %>%
  head(15)

jammer_performance_2t %>%
  select(displayName, n, win_rate) %>%
  gt() %>%
  cols_label(
    displayName = "Jammer",
    n = "# Snaps",
    win_rate = "Win Rate"
  ) %>%
  gtExtras::gt_theme_538() %>%
  tab_header(
    title = "Best Single-Coverage Jammer Win Rates, 2018-20",
    subtitle = "min. 25 qualifying snaps"
  ) %>%
  fmt_percent(columns = 3, decimals = 1)

play_results <- plays_tracking %>%
  merge(pff_scouting) %>%
  group_by(id) %>%
  summarize(
    desc = first(playDescription),
    returnyds = first(kickReturnYardage),
    kickyds = first(kickLength),
    n_gunners_tracking = sum(gunner, na.rm = T),
    n_gunners_pff = ifelse(is.na(first(gunners)), 0, stringr::str_count(first(gunners), ";") + 1),
    n_jammers_tracking = sum(jammer, na.rm = T),
    n_jammers_pff = ifelse(is.na(first(vises)), 0, stringr::str_count(first(vises), ";") + 1)
  ) %>%
  mutate(jammers_diff = n_jammers_tracking - n_jammers_pff,
         gunners_diff = n_gunners_tracking - n_gunners_pff,
         returnyds = ifelse(is.na(returnyds), 0, returnyds)) %>%
  filter(gunners_diff == 0, jammers_diff == 0) %>%
  select(id, returnyds, n_gunners_pff) %>%
  merge(plays_tracking) %>%
  filter(jammer == 0, num_gunners_on_side == 1, n_gunners_pff == 2) %>%
  group_by(id) %>%
  summarize(
    wins = sum(win),
    returnyds = mean(returnyds)
  ) %>%
  group_by(wins) %>%
  summarize(
    mean_returnyds = mean(returnyds),
    sd_returnyds = sd(returnyds)
  )

# See overall play result trends
play_results %>%
  gt() %>%
  cols_label(
    wins = "Gunner Wins",
    mean_returnyds = "Mean Yards",
    sd_returnyds = "Standard Deviation of Yards"
  ) %>%
  tab_header(
    title = "Mean and SD of Return Yards by # Gunner Wins",
    subtitle = "Non-penalty, non-blocked punts 2018-20"
  ) %>%
  gtExtras::gt_theme_538() %>%
  fmt_number(columns = 2:3, decimals = 2) %>%
  cols_align("center") %>%
  tab_footnote(footnote="Gunner past jammer at 3.5s mark", locations=cells_column_labels(columns=1))

# Make example plot of play (note: now we're using all frames)
df_tracking_trimmed <- df_tracking %>%
  mutate(id = paste0(gameId, playId)) %>%
  filter(id == "20180909091990") %>%
  mutate(
    id = paste0(gameId, playId),
    is_football = ifelse(displayName == "football", 1, 0),
    ball_snapped = ifelse(event == "ball_snap", 1, 0),
    punt_received = ifelse(event == "punt_received" | event == "punt_land" | event == "punt_muffed", 1, 0)
  ) %>%
  merge(plays) %>%
  filter(specialTeamsPlayType == "Punt") %>%
  group_by(id, nflId) %>%
  mutate(
    player_start_x = max(x*ball_snapped),
    player_start_y = max(y*ball_snapped),
    player_start_o = max(o*ball_snapped)
  ) %>%
  ungroup() %>%
  group_by(id) %>%
  mutate(
    start_ball_x = max(is_football*ball_snapped*x),
    start_ball_y = max(is_football*ball_snapped*y),
    end_ball_x = max(is_football*punt_received*x),
    end_ball_y = max(is_football*punt_received*y),
    play_start_frame = max(frameId*ball_snapped),
    frame_since_start = frameId - play_start_frame,
    play_end_frame = max(frameId*punt_received)
  ) %>%
  ungroup() %>%
  merge(pff_scouting) %>%
  mutate(
    defteam = word(vises, start=1, end=1),
    posteam = word(gunners, start=1, end=1),
    team_number = case_when(
      between(player_start_o, 0, 180) ~ paste0(posteam, " ", jerseyNumber),
      between(player_start_o, 180, 360) ~ paste0(defteam, " ", jerseyNumber))
  ) %>%
  filter(
    stringr::str_detect(vises, paste0(team_number, "$")) |
      stringr::str_detect(gunners, paste0(team_number, "$")) |
      stringr::str_detect(vises, paste0(team_number, "\\;")) |
      stringr::str_detect(gunners, paste0(team_number, "\\;"))
  ) %>%
  mutate(
    jammer = ifelse(stringr::str_detect(vises, team_number), 1, 0),
    gunner = ifelse(stringr::str_detect(gunners, team_number), 1, 0),
    jammer = ifelse(is.na(jammer), 0, jammer),
    gunner = ifelse(is.na(gunner), 0, gunner),
    player_type = case_when(
      gunner & start_ball_y < player_start_y ~ "top_gunner",
      gunner & start_ball_y > player_start_y ~ "bottom_gunner",
      jammer & start_ball_y < player_start_y ~ "top_jammer",
      jammer & start_ball_y > player_start_y ~ "bottom_jammer",
    ),
    top_gunner = ifelse(player_type == "top_gunner", 1, 0),
    bottom_gunner = ifelse(player_type == "bottom_gunner", 1, 0),
    top_jammer = ifelse(player_type == "top_jammer", 1, 0),
    bottom_jammer = ifelse(player_type == "bottom_jammer", 1, 0),
    top = ifelse(top_gunner | top_jammer, 1, 0),
    bottom = ifelse(bottom_gunner | bottom_jammer, 1, 0)
  ) %>%
  select(id:playDirection, playDescription, kickLength,
         kickReturnYardage, player_start_x:play_end_frame,
         hangTime, defteam:bottom)

df_tracking_grouped <- df_tracking_trimmed %>%
  group_by(id, frameId) %>%
  mutate(
    top_jammers = sum(top_jammer),
    bottom_jammers = sum(bottom_jammer),
    top_gunners = sum(top_gunner),
    bottom_gunners = sum(bottom_gunner)
  ) %>%
  ungroup() %>%
  mutate(
    num_jammers_on_side = ifelse(top, top_jammers, bottom_jammers),
    num_gunners_on_side = ifelse(bottom, bottom_gunners, top_gunners)
  ) %>%
  group_by(id, frameId) %>%
  mutate(
    max_x_top_jammer = max(x*top_jammer),
    max_x_top_gunner = max(x*top_gunner),
    max_x_bottom_jammer = max(x*bottom_jammer),
    max_x_bottom_gunner = max(x*bottom_gunner),
  ) %>%
  ungroup() %>%
  group_by(id, nflId, frameId) %>%
  mutate(
    win = case_when(
      bottom_jammer & max_x_bottom_gunner < x ~ 1,
      top_jammer & max_x_top_gunner < x ~ 1,
      bottom_gunner & max_x_bottom_jammer < x ~ 1,
      top_gunner & max_x_top_jammer < x ~ 1,
      TRUE ~ 0
    )
  ) %>%
  filter(frame_since_start <= 35, frame_since_start >= 0)

plot <- df_tracking_grouped %>%
  ggplot(aes(x, y, group = nflId)) +
  geom_point(aes(shape = team), color = ifelse(df_tracking_grouped$win == 1, "green", "purple"),
             size = 8) +
  geom_text(label = df_tracking_grouped$jerseyNumber) +
  scale_y_continuous(breaks = c(0,160/3)) +
  theme(
    panel.background = element_rect(fill = "darkgreen",
                                    colour = "darkgreen",
                                    size = 0.5, linetype = "solid"),
    axis.text.y = element_blank()
  ) +
  labs(
    x = "Distance from Back of Left End Zone (Yards)",
    y = "",
    title = paste0("Play: ", first(df_tracking_grouped$playDescription)),
    subtitle = "Frame: {frame_time}; Purple player is losing",
    shape = "Team"
  ) +
  gganimate::transition_time(as.integer(frame_since_start)) +
  gganimate::ease_aes()

gganimate::animate(plot, end_pause = 30)

anim_save("example_plot.gif")
