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
      barrel_like_rate = round(
        mean(launch_speed >= 98 & launch_angle >= 26 & launch_angle <= 30, na.rm = TRUE) * 100,
        1
      ),
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
  
  plot_data <- statcast_summary |>
    slice_max(avg_exit_velocity, n = 5) |>
    mutate(
      label = avg_exit_velocity,
      player_name = forcats::fct_reorder(player_name, avg_exit_velocity)
    )
  
  ggplot(plot_data, aes(x = player_name, y = avg_exit_velocity)) +
    geom_col(fill = "#174A8B", width = 0.55) +
    geom_text(
      aes(label = label),
      hjust = -0.2,
      fontface = "bold",
      size = 4.5
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(0, max(plot_data$avg_exit_velocity, na.rm = TRUE) * 1.18),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "Top Average Exit Velocity Leaders",
      x = NULL,
      y = "Average Exit Velocity"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(20, 70, 20, 20)
    )
}

plot_barrel_like_leaders <- function(statcast_summary) {
  if (nrow(statcast_summary) == 0) {
    return(
      ggplot() +
        annotate("text", x = 1, y = 1, label = "No Statcast data available.") +
        theme_void()
    )
  }
  
  plot_data <- statcast_summary |>
    slice_max(barrel_like_rate, n = 5) |>
    mutate(
      label = paste0(barrel_like_rate, "%"),
      player_name = forcats::fct_reorder(player_name, barrel_like_rate)
    )
  
  ggplot(plot_data, aes(x = player_name, y = barrel_like_rate)) +
    geom_col(fill = "#174A8B", width = 0.55) +
    geom_text(
      aes(label = label),
      hjust = -0.2,
      fontface = "bold",
      size = 4.5
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(
      limits = c(0, max(plot_data$barrel_like_rate, na.rm = TRUE) * 1.25),
      expand = expansion(mult = c(0, 0.05))
    ) +
    labs(
      title = "Barrel-Like Contact Rate Leaders",
      subtitle = "Approximation: EV ≥ 98 mph and LA between 26–30 degrees",
      x = NULL,
      y = "Barrel-Like Rate"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 11),
      panel.grid.major.y = element_blank(),
      plot.margin = margin(20, 70, 20, 20)
    )
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
    geom_rect(
      aes(xmin = 8, xmax = 32, ymin = 95, ymax = Inf),
      fill = "#174A8B",
      alpha = 0.08,
      inherit.aes = FALSE
    ) +
    geom_point(alpha = 0.75, size = 3, color = "#174A8B") +
    geom_hline(yintercept = 95, linetype = "dashed") +
    geom_vline(xintercept = c(8, 32), linetype = "dashed") +
    labs(
      title = paste("Launch Profile:", selected_player),
      subtitle = "Shaded zone approximates hard-hit ideal launch-angle contact",
      x = "Launch Angle",
      y = "Exit Velocity"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(20, 40, 20, 20)
    )
}