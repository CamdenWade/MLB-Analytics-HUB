make_hitter_profile_data <- function(data, selected_player) {
  player <- data |> filter(player_name == selected_player)
  if (nrow(player) == 0) return(tibble())
  
  tibble(
    Metric = c("WAR", "wRC+", "OPS", "OBP", "SLG"),
    Value = c(player$war, player$w_rc_plus, player$ops, player$obp, player$slg),
    Percentile = c(
      get_percentile(data, "war", player$war),
      get_percentile(data, "w_rc_plus", player$w_rc_plus),
      get_percentile(data, "ops", player$ops),
      get_percentile(data, "obp", player$obp),
      get_percentile(data, "slg", player$slg)
    )
  )
}

make_pitcher_profile_data <- function(data, selected_player) {
  player <- data |> filter(player_name == selected_player)
  if (nrow(player) == 0) return(tibble())
  
  tibble(
    Metric = c("WAR", "ERA", "FIP", "K/9", "BB/9"),
    Value = c(player$war, player$era, player$fip, player$k_9, player$bb_9),
    Percentile = c(
      get_percentile(data, "war", player$war),
      get_percentile(data, "era", player$era, higher_is_better = FALSE),
      get_percentile(data, "fip", player$fip, higher_is_better = FALSE),
      get_percentile(data, "k_9", player$k_9),
      get_percentile(data, "bb_9", player$bb_9, higher_is_better = FALSE)
    )
  )
}

plot_hitter_profile <- function(data, selected_player) {
  player <- data |> filter(player_name == selected_player)
  if (nrow(player) == 0) return(NULL)
  
  player_team <- player$team[1]
  bar_color <- get_team_color(player_team)
  
  metrics <- make_hitter_profile_data(data, selected_player) |>
    mutate(
      label = paste0(Percentile, "%"),
      Metric = forcats::fct_reorder(Metric, Percentile)
    )
  
  ggplot(metrics, aes(x = Metric, y = Percentile)) +
    geom_col(fill = bar_color, width = 0.65) +
    geom_text(
      aes(label = label),
      hjust = -0.15,
      color = "#1f2937",
      fontface = "bold",
      size = 4
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(0, 110),
      breaks = seq(0, 100, 25)
    ) +
    labs(
      title = paste("Hitter Percentile Profile:", selected_player),
      subtitle = paste("Team:", player_team),
      x = NULL,
      y = "Percentile"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )
}

plot_pitcher_profile <- function(data, selected_player) {
  player <- data |> filter(player_name == selected_player)
  if (nrow(player) == 0) return(NULL)
  
  player_team <- player$team[1]
  bar_color <- get_team_color(player_team)
  
  metrics <- make_pitcher_profile_data(data, selected_player) |>
    mutate(
      label = paste0(Percentile, "%"),
      Metric = forcats::fct_reorder(Metric, Percentile)
    )
  
  ggplot(metrics, aes(x = Metric, y = Percentile)) +
    geom_col(fill = bar_color, width = 0.65) +
    geom_text(
      aes(label = label),
      hjust = -0.15,
      color = "#1f2937",
      fontface = "bold",
      size = 4
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(0, 110),
      breaks = seq(0, 100, 25)
    ) +
    labs(
      title = paste("Pitcher Percentile Profile:", selected_player),
      subtitle = paste("Team:", player_team),
      x = NULL,
      y = "Percentile"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(face = "bold", size = 18),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(face = "bold"),
      panel.grid.major.y = element_blank()
    )
}