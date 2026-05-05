plot_hitter_profile <- function(data, selected_player) {
  player <- data |>
    filter(player_name == selected_player)
  
  if (nrow(player) == 0) return(NULL)
  
  player_team <- player$team[1]
  bar_color <- get_team_color(player_team)
  
  metrics <- player |>
    select(war, w_rc_plus, ops, obp, slg) |>
    pivot_longer(everything(), names_to = "metric", values_to = "value") |>
    mutate(label = round(value, 2))
  
  ggplot(metrics, aes(x = reorder(metric, value), y = value)) +
    geom_col(fill = bar_color) +
    geom_text(
      aes(label = label),
      hjust = 1.15,
      color = "white",
      fontface = "bold",
      size = 4
    ) +
    coord_flip() +
    labs(
      title = paste("Hitter Profile:", selected_player),
      subtitle = paste("Team:", player_team),
      x = NULL,
      y = "Value"
    ) +
    theme_minimal()
}

plot_pitcher_profile <- function(data, selected_player) {
  player <- data |>
    filter(player_name == selected_player)
  
  if (nrow(player) == 0) return(NULL)
  
  player_team <- player$team[1]
  bar_color <- get_team_color(player_team)
  
  metrics <- player |>
    select(war, era, fip, k_9, bb_9) |>
    pivot_longer(everything(), names_to = "metric", values_to = "value") |>
    mutate(label = round(value, 2))
  
  ggplot(metrics, aes(x = reorder(metric, value), y = value)) +
    geom_col(fill = bar_color) +
    geom_text(
      aes(label = label),
      hjust = 1.15,
      color = "white",
      fontface = "bold",
      size = 4
    ) +
    coord_flip() +
    labs(
      title = paste("Pitcher Profile:", selected_player),
      subtitle = paste("Team:", player_team),
      x = NULL,
      y = "Value"
    ) +
    theme_minimal()
}
