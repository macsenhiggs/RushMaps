library(ggplot2)
library(nflfastR)
library(tidyverse)
library(dplyr)
library(nflplotR)
library(nflreadr)
library(ggtext)
library(shadowtext)
library(stringr)
library(ggrepel)

pbp <- load_pbp(2025)

players <- load_players()

pbp_rushers <- pbp |>
  left_join(players |> select(gsis_id, display_name, position, position_group, latest_team),
            by = c("rusher_player_id" = "gsis_id")) |>
  filter(position == "RB")

pbp_receivers <- pbp |>
  left_join(players |> select(gsis_id, display_name, position, position_group, latest_team),
            by = c("receiver_player_id" = "gsis_id")) |>
  filter(position == "RB")

pbp_filtered <- bind_rows(pbp_rushers, pbp_receivers) |>
  filter(play_type == "pass" | play_type == "run")
  

summary <- pbp_filtered |>
  group_by(rusher_player_name, posteam) |>
  summarize(
    epa_rush = mean(epa, na.rm = TRUE),
    fd_or_td_rush = mean(first_down + touchdown),
    yards_rush = mean(yards_gained, na.rm = TRUE),
    plays = n()
  ) |>
  filter(plays >= 50)

teams <- load_teams() |> 
  select(team_abbr, team_color, team_color2)

summary <- summary |>
  left_join(
    teams,
    by = c("posteam" = "team_abbr")
  )


ggplot(summary, aes(x = yards_rush, y = fd_or_td_rush)) +
  geom_point(aes(size = plays, color = summary$team_color2)) +
  scale_color_identity() +
  geom_point(aes(size = plays/2, color = summary$team_color)) +
  geom_shadowtext(
    aes(label = rusher_player_name),
    size = 2,
    fontface = "bold",
    bg.color = "white",
    bg.r = 0.15,
    color = "black",
    nudge_y = 0.01
  ) +
  labs(
    title = "Rushing Yards/Touch vs. First Downs/Touch for RBs",
    subtitle = "2025 Season, Weeks 1–10 (Min. 75 Touches)",
    x = "Yards / Touch",
    y = "First downs / Touch",
    caption = "data: nflfastr",
    size = "Attempts",
    color = "Team"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "top"
  ) +
  guides(size = guide_legend(override.aes = list(alpha = 1)))
