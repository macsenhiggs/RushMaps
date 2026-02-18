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


pbp_default <- load_pbp(1999:2025)

get_boxplot <- function(startYear, endYear, playerName = NULL, teamName = NULL) {
  
  players <- load_players()
  
  #filter to add player teams via players and filter to only RB runs in time frame
  pbp <- pbp_default |>
    left_join(players |> select(gsis_id, display_name, position, position_group, latest_team),
              by = c("rusher_player_id" = "gsis_id")) |>
    filter(season >= startYear,
           season <= endYear,
           play_type == "run",
           position == "RB",
           !is.na(run_location)
           ) |>
    mutate(
      run_location_gap = paste(run_location, run_gap),
      run_location_gap = factor(
        run_location_gap,
        levels = c("left end","left tackle","left guard","middle NA","right guard","right tackle","right end")
      )
    )
  
  #filter for specific team or RB and create generic name for title
  if (!is.null(playerName)) {
    pbp <- pbp |> filter(rusher_player_name == playerName)
    genericName <- playerName
  } else if (!is.null(teamName)) {
    pbp <- pbp |> filter(posteam == teamName)
    genericName <- teamName
  } else {
    genericName <- "All RBs"
  }
  
  teams <- load_teams() |> 
    select(team_abbr, team_color, team_color2)
  
  pbp <- pbp |>
    left_join(
      teams,
      by = c("posteam" = "team_abbr")
    )
  
  team_cols <- pbp |>
    distinct(team_color, team_color2) |>
    slice(1)
  
  summary <- pbp |>
    group_by(run_location_gap) |>
    summarize(
      epa_rush = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      fd_or_td_rush = mean(first_down + touchdown, na.rm = TRUE),
      yards_rush = mean(yards_gained, na.rm = TRUE),
      plays = n()
    )
  
  print(summary)
  
  ggplot(data = pbp, aes(x = run_location_gap , y = epa)) +
    geom_boxplot(alpha = 0.8, outlier.shape = 21, outlier.size = 2,
                 fill = team_cols$team_color, color = "black") + #color can also be team_cols$team_color2
    geom_hline(yintercept = 0, color = "black", linewidth = 1, linetype = "dashed") +
    labs(
      title = paste0(genericName, ": YPC by Run Location and Gap, ", startYear, "-", endYear),
      subtitle = paste0("RB handoffs only (some outliers not shown)"),
      x = "Run Location & Gap",
      y = "EPA",
      caption = "data: nflfastr"
    ) +
    coord_cartesian(ylim = c(2, -1.5)) +
    theme_minimal(base_size = 13)
  
}

#get_boxplot(2025, 2025, playerName = , teamName = "ATL")
















