prepare_hitter_model_data <- function(hitters) {
  hitters |>
    select(player_name, team, war, hr, rbi, sb, avg, obp, slg, ops, w_rc_plus) |>
    filter(if_all(c(war, hr, rbi, sb, avg, obp, slg, ops, w_rc_plus), ~ !is.na(.x)))
}

build_hitter_war_model <- function(hitters) {
  model_data <- prepare_hitter_model_data(hitters)
  
  lm(
    war ~ hr + rbi + sb + avg + obp + slg + ops + w_rc_plus,
    data = model_data
  )
}

make_model_coefficients <- function(model) {
  broom::tidy(model) |>
    filter(term != "(Intercept)") |>
    mutate(
      estimate = round(estimate, 3),
      p.value = round(p.value, 4),
      term = recode(
        term,
        "hr" = "Home Runs",
        "rbi" = "RBI",
        "sb" = "Stolen Bases",
        "avg" = "AVG",
        "obp" = "OBP",
        "slg" = "SLG",
        "ops" = "OPS",
        "w_rc_plus" = "wRC+"
      )
    ) |>
    arrange(desc(abs(estimate)))
}

plot_model_coefficients <- function(model) {
  coef_df <- make_model_coefficients(model) |>
    filter(abs(estimate) < quantile(abs(estimate), 0.90, na.rm = TRUE)) |>
    mutate(
      label = round(estimate, 2),
      term = forcats::fct_reorder(term, estimate)
    )
  
  ggplot(coef_df, aes(x = term, y = estimate)) +
    geom_col(fill = "#174A8B", width = 0.65) +
    geom_text(
      aes(label = label),
      hjust = ifelse(coef_df$estimate >= 0, -0.15, 1.15),
      fontface = "bold",
      size = 4
    ) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0.20, 0.25))) +
    labs(
      title = "Which Hitting Stats Drive WAR?",
      subtitle = "Linear regression coefficients from qualified hitters; extreme outlier coefficients hidden for readability",
      x = NULL,
      y = "Model Coefficient"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.text.y = element_text(size = 11, face = "bold"),
      panel.grid.major.y = element_blank()
    )
}

make_model_summary_table <- function(model) {
  glance_df <- broom::glance(model)
  
  tibble(
    Metric = c("R-Squared", "Adjusted R-Squared", "Model RMSE", "Number of Players"),
    Value = c(
      round(glance_df$r.squared, 3),
      round(glance_df$adj.r.squared, 3),
      round(sqrt(mean(model$residuals^2)), 3),
      length(model$fitted.values)
    )
  )
}

make_top_model_residuals <- function(hitters, model) {
  model_data <- prepare_hitter_model_data(hitters)
  
  model_data |>
    mutate(
      predicted_war = round(predict(model, newdata = model_data), 2),
      actual_war = round(war, 2),
      difference = round(actual_war - predicted_war, 2)
    ) |>
    arrange(desc(abs(difference))) |>
    select(player_name, team, actual_war, predicted_war, difference) |>
    slice_head(n = 15)
}