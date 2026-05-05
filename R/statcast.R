make_batter_statcast_summary <- function(statcast_data) {
  if (nrow(statcast_data) == 0) {
    return(tibble())
  }
  
  statcast_data |>
    filter(!is.na(player_name)) |>
    group_by(player_name) |>
    summarise(
      batted_balls = sum(!is.na(launch_speed)),
      avg_exit_velocity = round(mean(launch_speed, na.rm = TRUE), 1),
      avg_launch_angle = round(mean(launch_angle, na.rm = TRUE), 1),
      hard_hit_rate = round(mean(launch_speed >= 95, na.rm = TRUE) * 100, 1),
      max_exit_velocity = round(max(launch_speed, na.rm = TRUE), 1),
      .groups = "drop"
    ) |>
    filter(batted_balls >= 5) |>
    arrange(desc(avg_exit_velocity))
}

plot_exit_velocity_leaders <- function(statcast_summary) {
  if (nrow(statcast_summary) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No Statcast data available.") +
        theme_void()
    )
  }
  
  statcast_summary |>
    slice_max(avg_exit_velocity, n = 15) |>
    ggplot(aes(x = reorder(player_name, avg_exit_velocity), y = avg_exit_velocity)) +
    geom_col() +
    coord_flip() +
    labs(
      title = "Top Average Exit Velocity Leaders",
      x = NULL,
      y = "Average Exit Velocity"
    ) +
    theme_minimal()
}

plot_launch_profile <- function(statcast_data, selected_player) {
  if (nrow(statcast_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No Statcast data available.") +
        theme_void()
    )
  }
  
  player_data <- statcast_data |>
    filter(player_name == selected_player) |>
    filter(!is.na(launch_speed), !is.na(launch_angle))
  
  if (nrow(player_data) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No batted-ball data for selected player.") +
        theme_void()
    )
  }
  
  player_data |>
    ggplot(aes(x = launch_angle, y = launch_speed)) +
    geom_point(alpha = 0.6) +
    labs(
      title = paste("Launch Profile:", selected_player),
      x = "Launch Angle",
      y = "Exit Velocity"
    ) +
    theme_minimal()
}