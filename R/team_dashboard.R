make_team_summary <- function(hitters, pitchers, selected_team) {
  team_hitters <- hitters |> filter(team == selected_team)
  team_pitchers <- pitchers |> filter(team == selected_team)
  
  tibble(
    Category = c(
      "Total Hitter WAR",
      "Team OPS",
      "Team wRC+",
      "Total Pitcher WAR",
      "Team ERA",
      "Team FIP"
    ),
    Value = c(
      round(sum(team_hitters$war, na.rm = TRUE), 1),
      round(mean(team_hitters$ops, na.rm = TRUE), 3),
      round(mean(team_hitters$w_rc_plus, na.rm = TRUE), 1),
      round(sum(team_pitchers$war, na.rm = TRUE), 1),
      round(mean(team_pitchers$era, na.rm = TRUE), 2),
      round(mean(team_pitchers$fip, na.rm = TRUE), 2)
    )
  )
}

make_top_team_hitters <- function(hitters, selected_team) {
  hitters |>
    filter(team == selected_team) |>
    arrange(desc(war)) |>
    select(player_name, pa, hr, rbi, ops, w_rc_plus, war, everything()) |>
    slice_head(n = 10)
}

make_top_team_pitchers <- function(pitchers, selected_team) {
  pitchers |>
    filter(team == selected_team) |>
    arrange(desc(war)) |>
    select(player_name, ip, era, fip, whip, k_9, bb_9, war, everything()) |>
    slice_head(n = 10)
}

plot_team_hitters <- function(hitters, selected_team) {
  top_hitters <- make_top_team_hitters(hitters, selected_team) |>
    mutate(label = round(war, 1))
  
  bar_color <- get_team_color(selected_team)
  
  ggplot(top_hitters, aes(x = reorder(player_name, war), y = war)) +
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
      title = paste(selected_team, "Top Hitters by WAR"),
      x = NULL,
      y = "WAR"
    ) +
    theme_minimal()
}

plot_team_pitchers <- function(pitchers, selected_team) {
  top_pitchers <- make_top_team_pitchers(pitchers, selected_team) |>
    mutate(label = round(war, 1))
  
  bar_color <- get_team_color(selected_team)
  
  ggplot(top_pitchers, aes(x = reorder(player_name, war), y = war)) +
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
      title = paste(selected_team, "Top Pitchers by WAR"),
      x = NULL,
      y = "WAR"
    ) +
    theme_minimal()
}
