make_hitter_comparison <- function(data, player_1, player_2) {
  data |>
    filter(player_name %in% c(player_1, player_2)) |>
    select(player_name, team, pa, hr, r, rbi, sb, avg, obp, slg, ops, w_rc_plus, war)
}

make_pitcher_comparison <- function(data, player_1, player_2) {
  data |>
    filter(player_name %in% c(player_1, player_2)) |>
    select(player_name, team, ip, w, l, era, fip, whip, k_9, bb_9, hr_9, war)
}

plot_hitter_comparison <- function(data, player_1, player_2) {
  comparison <- make_hitter_comparison(data, player_1, player_2)
  
  comparison_long <- comparison |>
    select(player_name, war, w_rc_plus, ops, obp, slg) |>
    pivot_longer(
      cols = -player_name,
      names_to = "metric",
      values_to = "value"
    )
  
  ggplot(comparison_long, aes(x = metric, y = value, fill = player_name)) +
    geom_col(position = "dodge") +
    labs(
      title = paste(player_1, "vs", player_2),
      x = NULL,
      y = "Value",
      fill = "Player"
    ) +
    theme_minimal()
}

plot_pitcher_comparison <- function(data, player_1, player_2) {
  comparison <- make_pitcher_comparison(data, player_1, player_2)
  
  comparison_long <- comparison |>
    select(player_name, war, era, fip, whip, k_9, bb_9) |>
    pivot_longer(
      cols = -player_name,
      names_to = "metric",
      values_to = "value"
    )
  
  ggplot(comparison_long, aes(x = metric, y = value, fill = player_name)) +
    geom_col(position = "dodge") +
    labs(
      title = paste(player_1, "vs", player_2),
      x = NULL,
      y = "Value",
      fill = "Player"
    ) +
    theme_minimal()
}