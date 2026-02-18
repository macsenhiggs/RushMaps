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
library(grid)
library(patchwork)


#pbp_default <- load_pbp(1999:2025)

pbp_default <- load_pbp(2020:2025)

get_rush_map <- function(startYear, endYear, teamName = NULL, playerName = NULL) {
  
  players <- load_players()
  
  pbp <- pbp_default |>
    left_join(
      players |> select(gsis_id, display_name, position, position_group, latest_team),
      by = c("rusher_player_id" = "gsis_id")
    )
  
  if (!is.null(playerName)) {
    candidates <- players |> filter(short_name == playerName, latest_team == teamName)
    player_id <- candidates$gsis_id[1]
    
    pbp <- pbp |> filter(rusher_player_id == player_id)
    genericName <- playerName
  } else if (!is.null(teamName)) {
    pbp <- pbp |> filter(posteam == teamName)
    genericName <- teamName
  } else {
    genericName <- "All RBs"
  }
  
  pbp <- pbp |>
    filter(
      season >= startYear,
      season <= endYear,
      play_type == "run",
      position == "RB",
      !is.na(run_location)
    ) |>
    mutate(
      run_location_gap = paste(run_location, run_gap),
      run_location_gap = factor(
        run_location_gap,
        levels = c(
          "left end", "left tackle", "left guard", "middle NA", "right guard", "right tackle", "right end"
        )
      )
    )
  
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
  
  rgb_vals <- col2rgb(team_cols$team_color2)
  r <- rgb_vals[1,]; g <- rgb_vals[2,]; b <- rgb_vals[3,]
  if(r + g + b > 180) { #light color, use dark text
    textCol <- team_cols$team_color2
  } else { #dark color, use light text
    textCol <- team_cols$team_color
  }
  
  #print(paste("The RGB values of team_color2 in decimal form are:", r, g ,b))
  
  summary <- pbp |>
    group_by(run_location_gap) |>
    summarize(
      rlgAsNum = first(as.numeric(run_location_gap)),
      curvature = first(-0.60 + 0.15 * as.numeric(run_location_gap)),
      epa_rush = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      fd_or_td_rush = mean(first_down + touchdown, na.rm = TRUE),
      yards_rush = mean(yards_gained, na.rm = TRUE),
      plays = n(),
    )
  
  ymax <- max(0.8, max(summary$success_rate, na.rm = TRUE))
  
  print(summary)
  
  p <- ggplot(summary, aes(x = rlgAsNum, y = success_rate)) +
    theme_minimal(base_size = 13) +
    scale_linewidth_continuous(range = c(1, 3.5)) +
    scale_y_continuous(labels = scales::percent, limits = c(0, ymax)) + #use ymax if you want
    geom_hline(yintercept = 0.4, color = "black", linewidth = 1, linetype = "dashed") +
    geom_shadowtext(
      data = data.frame(
        x = c(1.5, 2.75, 4, 5.25, 6.5),
        y = c(0.35, 0.4, 0.45, 0.4, 0.35),
        label = c("T", "G", "C", "G", "T")
      ),
      aes(x = x, y = y, label = label),
      color = textCol,
      bg.color = "black",
      bg.r = 0.10,
      size = 10,
      fontface = "bold"
    ) +
    labs(y = "Success Rate") + 
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y  = element_blank(),
      axis.ticks.y = element_blank(),
      legend.position = "none"
    )
  
  if (!is.null(playerName)) {
    p <- p +
      geom_nfl_headshots(
        data = pbp |> slice(1) |> select(player_gsis = rusher_player_id) |> mutate(x = 4, y = 0.80),
        aes(player_gsis = player_gsis, x = x, y = y),
        width = 0.35
      )
  } else if (!is.null(teamName)) {
    p <- p +
      geom_nfl_wordmarks(
        data = data.frame(team_abbr = teamName, x = 4, y = 0.7),
        aes(team_abbr = teamName, x = x, y = y),
        width = 0.50 # 0.25 if logos, 0.5 if wordmarks
      )
  } else {
    p <- p +
      geom_nfl_logos(
        data = data.frame(team_abbr = "NFL", x = 4, y = 0.7),
        aes(team_abbr = team_abbr, x = x, y = y),
        width = 0.17
      )
  }
  
  # loop over rows to add curved arrows manually
  curve_layers <- lapply(split(summary, 1:nrow(summary)), function(dat) {
    
    if (dat$rlgAsNum == 4 && dat$success_rate == 0)
      return(NULL)
    
    geom_curve(
      data = dat,
      aes(
        x = 4, y = 0,
        xend = rlgAsNum,
        yend = success_rate,
        linewidth = plays
      ),
      curvature = dat$curvature,              # per-row curvature applied here
      color = team_cols$team_color,
      arrow = arrow(length = unit(0.2, "cm"), type = "closed")
    )
  })
  
  # add all the curve layers to the plot
  for (layer in curve_layers) {
    p <- p + layer
  }
  
  p <- p + geom_point(
    x = 4,
    y = 0,
    size = 10,
    color = team_cols$team_color2
  )
  

  
  showVal <- FALSE
  
  if(showVal) {
    p <- p + geom_shadowtext(
      data = summary,
      aes(x = rlgAsNum, y = success_rate + 0.035, label = paste0(round(success_rate * 100, digits = 1), "%")),
      color = "white",
      bg.color = "black",
      bg.r = 0.15,
      size = 5,
      fontface = "bold"
    )
  }
  
  
  
  print(p)
  
  return(p)
  
}

get_carry_leaders <- function(startYear, endYear) {
  carryLeaders <- pbp_default |>
    left_join(players |> select(gsis_id, display_name, position, position_group, latest_team),
              by = c("rusher_player_id" = "gsis_id")) |>
    filter(season >= startYear,
           season <= endYear,
           play_type == "run",
           position == "RB",
           !is.na(run_location)
    ) |>
    group_by(rusher_player_name, latest_team) |>
    summarize(
      epa_rush = mean(epa, na.rm = TRUE),
      success_rate = mean(success, na.rm = TRUE),
      fd_or_td_rush = mean(first_down + touchdown, na.rm = TRUE),
      yards_rush = mean(yards_gained, na.rm = TRUE),
      plays = n(),
      .groups = "drop"
    ) |>
    arrange(desc(plays)) |>
    slice_head(n = 32)
  
  return(carryLeaders)
}

get_rush_map(2020, 2025)
teams <- load_teams()
startYear <- 2025
endYear <- 2025

for (i in seq_len(nrow(teams))) {
  teamName <- teams$team_abbr[i]
  
  teamPlot <- get_rush_map(startYear, endYear, teamName)
  
  fileName <- paste0(teamName, "-rushing-map-", startYear, "-", endYear, ".png")
  
  #ggsave(filename = fileName, plot = teamPlot, width = 6, height = 4, dpi = 300)
  
  if(i == 1) plotGrid <- teamPlot else plotGrid <- plotGrid + teamPlot
}

ggsave(filename = "rushmaps2025.png", plot = plotGrid, width = 30, height = 16, dpi = 300)

players <- load_players()

carryLeaders2025 <- get_carry_leaders(2025, 2025)

for (i in seq_len(nrow(carryLeaders2025))) {
  playerName <- carryLeaders2025$rusher_player_name[i]
  playerTeam <- carryLeaders2025$latest_team[i]
  
  playerPlot <- get_rush_map(startYear, endYear, teamName = playerTeam, playerName = playerName)
  
  fileName <- paste0(playerName, "-rushing-map-", startYear, "-", endYear, ".png")
  
  ggsave(filename = fileName, plot = playerPlot, width = 6, height = 4, dpi = 300)
  
  #if(i == 1) plotGrid <- playerPlot else plotGrid <- plotGrid + playerPlot
}

get_rush_map(2024, 2024, "DET")
get_rush_map(2025, 2025, "DET")




#Title: paste0(genericName, ": Success Rate by Run Location and Gap")
#Subtitle: paste0("RB handoffs only, ", startYear, "-", endYear)
#Caption: "data: nflfastR"



