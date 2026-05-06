get_percentile <- function(data, metric, player_value, higher_is_better = TRUE) {
  vals <- data[[metric]]
  vals <- vals[!is.na(vals)]
  
  if (length(vals) == 0 || is.na(player_value)) {
    return(NA_real_)
  }
  
  pct <- mean(vals <= player_value) * 100
  
  if (!higher_is_better) {
    pct <- 100 - pct
  }
  
  round(pct, 1)
}

make_hitter_scouting <- function(data, selected_player) {
  player <- data |> filter(player_name == selected_player) |> slice(1)
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
  ) |>
    filter(!is.na(Value), !is.na(Percentile))
}

make_pitcher_scouting <- function(data, selected_player) {
  player <- data |> filter(player_name == selected_player) |> slice(1)
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
  ) |>
    filter(!is.na(Value), !is.na(Percentile))
}

plot_percentiles <- function(scouting_df, selected_player, team_color = "#174A8B") {
  if (nrow(scouting_df) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No scouting data available.") +
        theme_void()
    )
  }
  
  scouting_df <- scouting_df |>
    arrange(Percentile) |>
    mutate(
      label = paste0(round(Percentile, 1), "%"),
      Metric = factor(Metric, levels = Metric)
    )
  
  ggplot(scouting_df, aes(x = Metric, y = Percentile)) +
    geom_col(fill = team_color, width = 0.65) +
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
      x = NULL,
      y = "Percentile"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.y = element_text(face = "bold"),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(20, 60, 20, 20)
    )
}