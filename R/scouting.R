get_percentile <- function(data, metric, player_value, higher_is_better = TRUE) {
  vals <- data[[metric]]
  vals <- vals[!is.na(vals)]
  
  pct <- mean(vals <= player_value) * 100
  
  if (!higher_is_better) {
    pct <- 100 - pct
  }
  
  round(pct, 1)
}

make_hitter_scouting <- function(data, selected_player) {
  player <- data |>
    filter(player_name == selected_player)
  
  if (nrow(player) == 0) return(NULL)
  
  tibble(
    Metric = c("WAR", "wRC+", "OPS", "OBP", "SLG"),
    Value = c(
      player$war,
      player$w_rc_plus,
      player$ops,
      player$obp,
      player$slg
    ),
    Percentile = c(
      get_percentile(data, "war", player$war),
      get_percentile(data, "w_rc_plus", player$w_rc_plus),
      get_percentile(data, "ops", player$ops),
      get_percentile(data, "obp", player$obp),
      get_percentile(data, "slg", player$slg)
    )
  )
}

make_pitcher_scouting <- function(data, selected_player) {
  player <- data |>
    filter(player_name == selected_player)
  
  if (nrow(player) == 0) return(NULL)
  
  tibble(
    Metric = c("WAR", "ERA", "FIP", "K/9", "BB/9"),
    Value = c(
      player$war,
      player$era,
      player$fip,
      player$k_9,
      player$bb_9
    ),
    Percentile = c(
      get_percentile(data, "war", player$war),
      get_percentile(data, "era", player$era, higher_is_better = FALSE),
      get_percentile(data, "fip", player$fip, higher_is_better = FALSE),
      get_percentile(data, "k_9", player$k_9),
      get_percentile(data, "bb_9", player$bb_9, higher_is_better = FALSE)
    )
  )
}

plot_percentiles <- function(scouting_df, selected_player, team_color = "#1f77b4") {
  scouting_df <- scouting_df |>
    mutate(label = paste0(Percentile, "%"))
  
  ggplot(scouting_df, aes(x = reorder(Metric, Percentile), y = Percentile)) +
    geom_col(fill = team_color) +
    geom_text(
      aes(label = label),
      hjust = 1.15,
      color = "white",
      fontface = "bold",
      size = 4
    ) +
    coord_flip() +
    scale_y_continuous(limits = c(0, 100)) +
    labs(
      title = paste("Percentile Profile:", selected_player),
      x = NULL,
      y = "Percentile"
    ) +
    theme_minimal()
}
