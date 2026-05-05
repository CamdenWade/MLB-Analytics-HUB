source("R/00_packages.R")
source("R/data_prep.R")
source("R/ui_helpers.R")
source("R/plots.R")
source("R/scouting.R")
source("R/comparison.R")
source("R/team_dashboard.R")
source("R/statcast.R")
source("R/modeling.R")

mlb_data <- load_processed_data()

hitters <- mlb_data$hitters
pitchers <- mlb_data$pitchers
statcast_data <- mlb_data$statcast

team_choices <- sort(unique(c(hitters$team, pitchers$team)))
statcast_summary <- make_batter_statcast_summary(statcast_data)

statcast_player_choices <- if (nrow(statcast_summary) > 0) {
  sort(unique(statcast_summary$player_name))
} else {
  character(0)
}

hitter_war_model <- build_hitter_war_model(hitters)

make_player_kpi_card <- function(value, label) {
  div(
    class = "kpi-box",
    div(class = "kpi-value", value),
    div(class = "kpi-label", label)
  )
}

ui <- page_navbar(
  title = "MLB Analytics Hub",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  nav_panel(
    "Home",
    div(
      class = "hero-card",
      div(class = "hero-title", "MLB Analytics Hub"),
      div(
        class = "hero-subtitle",
        "An interactive baseball analytics dashboard built in R Shiny. Explore player leaderboards, scouting reports, player comparisons, team dashboards, Statcast batted-ball data, modeling insights, and downloadable reports."
      )
    ),
    
    layout_column_wrap(
      width = 1 / 3,
      card(class = "feature-card", card_body(div(class = "feature-title", "Player Leaderboards"), div(class = "feature-text", "Filter hitter and pitcher leaderboards by team and evaluate production using traditional and advanced metrics."))),
      card(class = "feature-card", card_body(div(class = "feature-title", "Scouting Reports"), div(class = "feature-text", "Generate percentile-based player scouting profiles and download polished HTML reports."))),
      card(class = "feature-card", card_body(div(class = "feature-title", "Player Comparison"), div(class = "feature-text", "Compare two hitters or pitchers side-by-side using tables and visualizations."))),
      card(class = "feature-card", card_body(div(class = "feature-title", "Team Dashboard"), div(class = "feature-text", "Analyze team-level strengths using WAR leaders, offensive production, and pitching performance."))),
      card(class = "feature-card", card_body(div(class = "feature-title", "Statcast Analysis"), div(class = "feature-text", "Explore exit velocity, launch angle, hard-hit rate, and barrel-like contact from Baseball Savant data."))),
      card(class = "feature-card", card_body(div(class = "feature-title", "Modeling Insights"), div(class = "feature-text", "Use linear regression to explore which offensive metrics are most associated with hitter WAR.")))
    )
  ),
  
  nav_panel(
    "Hitter Leaderboard",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("hitter_team", "Team", choices = c("All", sort(unique(hitters$team))), selected = "All")
      ),
      card(card_header("Hitter Leaderboard"), DTOutput("hitter_table"))
    )
  ),
  
  nav_panel(
    "Pitcher Leaderboard",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("pitcher_team", "Team", choices = c("All", sort(unique(pitchers$team))), selected = "All")
      ),
      card(card_header("Pitcher Leaderboard"), DTOutput("pitcher_table"))
    )
  ),
  
  nav_panel(
    "Player Profiles",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("profile_type", "Player Type", choices = c("Hitter", "Pitcher")),
        uiOutput("player_selector")
      ),
      
      uiOutput("profile_header"),
      
      div(
        class = "kpi-wrapper",
        layout_column_wrap(
          width = 1 / 5,
          uiOutput("profile_kpi_1"),
          uiOutput("profile_kpi_2"),
          uiOutput("profile_kpi_3"),
          uiOutput("profile_kpi_4"),
          uiOutput("profile_kpi_5")
        )
      ),
      
      card(
        card_header("Percentile Profile"),
        plotOutput("profile_plot", height = "540px")
      )
    )
  ),
  
  nav_panel(
    "Scouting Report",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("scout_type", "Player Type", choices = c("Hitter", "Pitcher")),
        uiOutput("scout_player_selector"),
        downloadButton("download_scouting_report", "Download Scouting Report")
      ),
      
      uiOutput("scout_header"),
      
      div(
        class = "kpi-wrapper",
        layout_column_wrap(
          width = 1 / 5,
          uiOutput("scout_kpi_1"),
          uiOutput("scout_kpi_2"),
          uiOutput("scout_kpi_3"),
          uiOutput("scout_kpi_4"),
          uiOutput("scout_kpi_5")
        )
      ),
      
      card(
        card_header("Scouting Percentile Chart"),
        plotOutput("scout_percentile_plot", height = "540px")
      ),
      
      card(
        card_header("Scouting Metrics"),
        DTOutput("scout_table")
      )
    )
  ),
  
  nav_panel(
    "Player Comparison",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("compare_type", "Player Type", choices = c("Hitter", "Pitcher")),
        uiOutput("compare_player_1_selector"),
        uiOutput("compare_player_2_selector")
      ),
      card(card_header("Comparison Chart"), plotlyOutput("comparison_plot", height = "500px")),
      card(card_header("Comparison Table"), DTOutput("comparison_table"))
    )
  ),
  
  nav_panel(
    "Team Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        selectInput("team_dashboard_team", "Team", choices = team_choices, selected = team_choices[1])
      ),
      layout_column_wrap(
        width = 1 / 3,
        card(card_header("Team Summary"), DTOutput("team_summary_table")),
        card(card_header("Top Hitters"), DTOutput("team_hitters_table")),
        card(card_header("Top Pitchers"), DTOutput("team_pitchers_table"))
      ),
      card(card_header("Hitter WAR Leaders"), plotOutput("team_hitter_plot", height = "600px")),
      card(card_header("Pitcher WAR Leaders"), plotOutput("team_pitcher_plot", height = "600px"))
    )
  ),
  
  nav_panel(
    "Statcast",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "statcast_player",
          "Player",
          choices = statcast_player_choices,
          selected = if (length(statcast_player_choices) > 0) statcast_player_choices[1] else NULL
        )
      ),
      
      uiOutput("statcast_player_header"),
      
      div(
        class = "kpi-wrapper",
        layout_column_wrap(
          width = 1 / 5,
          uiOutput("statcast_kpi_1"),
          uiOutput("statcast_kpi_2"),
          uiOutput("statcast_kpi_3"),
          uiOutput("statcast_kpi_4"),
          uiOutput("statcast_kpi_5")
        )
      ),
      
      card(
        card_header("Selected Player Batted-Ball Events"),
        DTOutput("selected_statcast_events_table")
      ),
      
      layout_column_wrap(
        width = 1 / 2,
        card(
          card_header("Exit Velocity Leaderboard"),
          DTOutput("exit_velocity_table")
        ),
        card(
          card_header("Barrel-Like Contact Leaderboard"),
          DTOutput("barrel_like_table")
        )
      ),
      
      card(
        card_header("Full Statcast Summary"),
        DTOutput("statcast_table")
      )
    )
  ),
  
  nav_panel(
    "Modeling Insights",
    layout_column_wrap(
      width = 1 / 2,
      card(card_header("WAR Model Summary"), DTOutput("model_summary_table")),
      card(card_header("Model Coefficients"), DTOutput("model_coefficients_table"))
    ),
    card(card_header("Which Hitting Stats Drive WAR?"), plotOutput("model_coefficients_plot", height = "600px")),
    card(card_header("Largest Actual WAR vs Predicted WAR Differences"), DTOutput("model_residuals_table"))
  ),
  
  nav_panel(
    "About / Data Dictionary",
    card(
      card_header("Project Overview"),
      card_body(
        p("MLB Analytics Hub is an interactive baseball analytics application built with R, Shiny, ggplot2, plotly, DT, Quarto, FanGraphs leaderboards, and Baseball Savant Statcast data."),
        p("The goal of this project is to demonstrate sports analytics, data visualization, app development, statistical modeling, and reporting skills in a portfolio-ready format.")
      )
    ),
    
    layout_column_wrap(
      width = 1 / 2,
      card(
        card_header("Key Metrics"),
        card_body(
          tags$ul(
            tags$li(strong("WAR:"), " Wins Above Replacement; estimates total player value."),
            tags$li(strong("wRC+:"), " Weighted Runs Created Plus; league-adjusted offensive production where 100 is average."),
            tags$li(strong("OPS:"), " On-base percentage plus slugging percentage."),
            tags$li(strong("FIP:"), " Fielding Independent Pitching; estimates pitcher performance using outcomes most controlled by the pitcher."),
            tags$li(strong("K/9:"), " Strikeouts per nine innings."),
            tags$li(strong("BB/9:"), " Walks per nine innings."),
            tags$li(strong("Exit Velocity:"), " Speed of the baseball off the bat."),
            tags$li(strong("Launch Angle:"), " Vertical angle of the batted ball.")
          )
        )
      ),
      card(
        card_header("Data Sources & Limitations"),
        card_body(
          tags$ul(
            tags$li("FanGraphs leaderboard data is used for hitter and pitcher season statistics."),
            tags$li("Baseball Savant / Statcast data is used for batted-ball quality metrics."),
            tags$li("The Statcast tab uses a sample date range to keep the app lightweight and easier to deploy."),
            tags$li("Modeling results are exploratory and should not be interpreted as causal."),
            tags$li("Player images depend on the availability of MLB player ID columns and MLB headshot URLs.")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_hitters <- reactive({
    if (input$hitter_team == "All") hitters else hitters |> filter(team == input$hitter_team)
  })
  
  filtered_pitchers <- reactive({
    if (input$pitcher_team == "All") pitchers else pitchers |> filter(team == input$pitcher_team)
  })
  
  selected_profile_player <- reactive({
    req(input$selected_player)
    
    if (input$profile_type == "Hitter") {
      hitters |> filter(player_name == input$selected_player) |> slice(1)
    } else {
      pitchers |> filter(player_name == input$selected_player) |> slice(1)
    }
  })
  
  selected_scout_player <- reactive({
    req(input$scout_player)
    
    if (input$scout_type == "Hitter") {
      hitters |> filter(player_name == input$scout_player) |> slice(1)
    } else {
      pitchers |> filter(player_name == input$scout_player) |> slice(1)
    }
  })
  
  selected_statcast_player <- reactive({
    req(input$statcast_player)
    
    statcast_summary |>
      filter(player_name == input$statcast_player) |>
      slice(1)
  })
  
  output$hitter_table <- renderDT({
    filtered_hitters() |>
      make_player_display() |>
      select(
        Player = player_display, Team = team, PA = pa, HR = hr, R = r, RBI = rbi,
        SB = sb, AVG = avg, OBP = obp, SLG = slg, OPS = ops, `wRC+` = w_rc_plus, WAR = war
      ) |>
      pretty_datatable(page_length = 25, escape = FALSE)
  })
  
  output$pitcher_table <- renderDT({
    filtered_pitchers() |>
      make_player_display() |>
      select(
        Player = player_display, Team = team, IP = ip, W = w, L = l, ERA = era,
        FIP = fip, WHIP = whip, `K/9` = k_9, `BB/9` = bb_9, `HR/9` = hr_9, WAR = war
      ) |>
      pretty_datatable(page_length = 25, escape = FALSE)
  })
  
  output$player_selector <- renderUI({
    if (input$profile_type == "Hitter") {
      selectInput("selected_player", "Player", choices = sort(unique(hitters$player_name)))
    } else {
      selectInput("selected_player", "Player", choices = sort(unique(pitchers$player_name)))
    }
  })
  
  output$profile_header <- renderUI({
    player <- selected_profile_player()
    team_color <- get_team_color(player$team[1])
    
    div(
      class = "player-summary-card",
      style = paste0("border-left: 8px solid ", team_color, ";"),
      div(class = "player-name-title", player$player_name[1]),
      div(class = "player-meta", paste("Team:", player$team[1], "| Type:", input$profile_type))
    )
  })
  
  output$profile_kpi_1 <- renderUI({
    player <- selected_profile_player()
    make_player_kpi_card(round(player$war[1], 1), "WAR")
  })
  
  output$profile_kpi_2 <- renderUI({
    player <- selected_profile_player()
    if (input$profile_type == "Hitter") {
      make_player_kpi_card(round(player$w_rc_plus[1], 0), "wRC+")
    } else {
      make_player_kpi_card(round(player$era[1], 2), "ERA")
    }
  })
  
  output$profile_kpi_3 <- renderUI({
    player <- selected_profile_player()
    if (input$profile_type == "Hitter") {
      make_player_kpi_card(round(player$ops[1], 3), "OPS")
    } else {
      make_player_kpi_card(round(player$fip[1], 2), "FIP")
    }
  })
  
  output$profile_kpi_4 <- renderUI({
    player <- selected_profile_player()
    if (input$profile_type == "Hitter") {
      make_player_kpi_card(round(player$hr[1], 0), "HR")
    } else {
      make_player_kpi_card(round(player$k_9[1], 2), "K/9")
    }
  })
  
  output$profile_kpi_5 <- renderUI({
    player <- selected_profile_player()
    if (input$profile_type == "Hitter") {
      make_player_kpi_card(round(player$rbi[1], 0), "RBI")
    } else {
      make_player_kpi_card(round(player$bb_9[1], 2), "BB/9")
    }
  })
  
  output$profile_plot <- renderPlot({
    req(input$selected_player)
    
    if (input$profile_type == "Hitter") {
      plot_hitter_profile(hitters, input$selected_player)
    } else {
      plot_pitcher_profile(pitchers, input$selected_player)
    }
  })
  
  output$scout_player_selector <- renderUI({
    if (input$scout_type == "Hitter") {
      selectInput("scout_player", "Player", choices = sort(unique(hitters$player_name)))
    } else {
      selectInput("scout_player", "Player", choices = sort(unique(pitchers$player_name)))
    }
  })
  
  output$scout_header <- renderUI({
    player <- selected_scout_player()
    team_color <- get_team_color(player$team[1])
    
    div(
      class = "player-summary-card",
      style = paste0("border-left: 8px solid ", team_color, ";"),
      div(class = "player-name-title", paste(player$player_name[1], "Scouting Report")),
      div(class = "player-meta", paste("Team:", player$team[1], "| Type:", input$scout_type))
    )
  })
  
  output$scout_kpi_1 <- renderUI({
    player <- selected_scout_player()
    make_player_kpi_card(round(player$war[1], 1), "WAR")
  })
  
  output$scout_kpi_2 <- renderUI({
    player <- selected_scout_player()
    if (input$scout_type == "Hitter") {
      make_player_kpi_card(round(player$w_rc_plus[1], 0), "wRC+")
    } else {
      make_player_kpi_card(round(player$era[1], 2), "ERA")
    }
  })
  
  output$scout_kpi_3 <- renderUI({
    player <- selected_scout_player()
    if (input$scout_type == "Hitter") {
      make_player_kpi_card(round(player$ops[1], 3), "OPS")
    } else {
      make_player_kpi_card(round(player$fip[1], 2), "FIP")
    }
  })
  
  output$scout_kpi_4 <- renderUI({
    player <- selected_scout_player()
    if (input$scout_type == "Hitter") {
      make_player_kpi_card(round(player$hr[1], 0), "HR")
    } else {
      make_player_kpi_card(round(player$k_9[1], 2), "K/9")
    }
  })
  
  output$scout_kpi_5 <- renderUI({
    player <- selected_scout_player()
    if (input$scout_type == "Hitter") {
      make_player_kpi_card(round(player$rbi[1], 0), "RBI")
    } else {
      make_player_kpi_card(round(player$bb_9[1], 2), "BB/9")
    }
  })
  
  scouting_data <- reactive({
    req(input$scout_player)
    
    if (input$scout_type == "Hitter") {
      make_hitter_scouting(hitters, input$scout_player)
    } else {
      make_pitcher_scouting(pitchers, input$scout_player)
    }
  })
  
  output$scout_table <- renderDT({
    scouting_data() |>
      pretty_datatable(page_length = 10, escape = FALSE)
  })
  
  output$scout_percentile_plot <- renderPlot({
    req(scouting_data())
    
    if (input$scout_type == "Hitter") {
      team_color <- hitters |> filter(player_name == input$scout_player) |> pull(team) |> first() |> get_team_color()
    } else {
      team_color <- pitchers |> filter(player_name == input$scout_player) |> pull(team) |> first() |> get_team_color()
    }
    
    plot_percentiles(scouting_data(), input$scout_player, team_color)
  })
  
  output$download_scouting_report <- downloadHandler(
    filename = function() {
      paste0(
        gsub(" ", "_", input$scout_player),
        "_scouting_report.html"
      )
    },
    content = function(file) {
      req(input$scout_player, input$scout_type)
      
      temp_dir <- tempdir()
      temp_report <- file.path(temp_dir, "player_report.qmd")
      
      file.copy(
        from = "report/player_report.qmd",
        to = temp_report,
        overwrite = TRUE
      )
      
      old_wd <- getwd()
      setwd(temp_dir)
      on.exit(setwd(old_wd), add = TRUE)
      
      output_name <- paste0(
        gsub(" ", "_", input$scout_player),
        "_scouting_report.html"
      )
      
      quarto::quarto_render(
        input = "player_report.qmd",
        output_file = output_name,
        execute_params = list(
          player_name = input$scout_player,
          player_type = input$scout_type,
          season = 2024
        )
      )
      
      file.copy(
        from = file.path(temp_dir, output_name),
        to = file,
        overwrite = TRUE
      )
    }
  )
  
  output$compare_player_1_selector <- renderUI({
    if (input$compare_type == "Hitter") {
      selectInput("compare_player_1", "Player 1", choices = sort(unique(hitters$player_name)))
    } else {
      selectInput("compare_player_1", "Player 1", choices = sort(unique(pitchers$player_name)))
    }
  })
  
  output$compare_player_2_selector <- renderUI({
    if (input$compare_type == "Hitter") {
      selectInput("compare_player_2", "Player 2", choices = sort(unique(hitters$player_name)), selected = sort(unique(hitters$player_name))[2])
    } else {
      selectInput("compare_player_2", "Player 2", choices = sort(unique(pitchers$player_name)), selected = sort(unique(pitchers$player_name))[2])
    }
  })
  
  comparison_data <- reactive({
    req(input$compare_player_1, input$compare_player_2)
    
    if (input$compare_type == "Hitter") {
      make_hitter_comparison(hitters, input$compare_player_1, input$compare_player_2)
    } else {
      make_pitcher_comparison(pitchers, input$compare_player_1, input$compare_player_2)
    }
  })
  
  output$comparison_table <- renderDT({
    comparison_data() |>
      make_player_display() |>
      rename(Player = player_display) |>
      select(Player, everything(), -player_name) |>
      pretty_datatable(page_length = 5, escape = FALSE)
  })
  
  output$comparison_plot <- renderPlotly({
    req(input$compare_player_1, input$compare_player_2)
    
    if (input$compare_type == "Hitter") {
      p <- plot_hitter_comparison(hitters, input$compare_player_1, input$compare_player_2)
    } else {
      p <- plot_pitcher_comparison(pitchers, input$compare_player_1, input$compare_player_2)
    }
    
    ggplotly(p) |> config(displayModeBar = FALSE)
  })
  
  output$team_summary_table <- renderDT({
    make_team_summary(hitters, pitchers, input$team_dashboard_team) |>
      pretty_datatable(page_length = 10, escape = FALSE)
  })
  
  output$team_hitters_table <- renderDT({
    make_top_team_hitters(hitters, input$team_dashboard_team) |>
      make_player_display() |>
      select(
        Player = player_display,
        PA = pa,
        HR = hr,
        RBI = rbi,
        OPS = ops,
        `wRC+` = w_rc_plus,
        WAR = war
      ) |>
      pretty_datatable(page_length = 10, escape = FALSE)
  })
  
  output$team_pitchers_table <- renderDT({
    make_top_team_pitchers(pitchers, input$team_dashboard_team) |>
      make_player_display() |>
      select(
        Player = player_display,
        IP = ip,
        ERA = era,
        FIP = fip,
        WHIP = whip,
        `K/9` = k_9,
        `BB/9` = bb_9,
        WAR = war
      ) |>
      pretty_datatable(page_length = 10, escape = FALSE)
  })
  
  output$team_hitter_plot <- renderPlot({
    plot_team_hitters(hitters, input$team_dashboard_team)
  })
  
  output$team_pitcher_plot <- renderPlot({
    plot_team_pitchers(pitchers, input$team_dashboard_team)
  })
  
  output$statcast_player_header <- renderUI({
    player <- selected_statcast_player()
    
    div(
      class = "player-summary-card",
      style = "border-left: 8px solid #174A8B;",
      div(class = "player-name-title", player$player_name[1]),
      div(class = "player-meta", "Statcast Batted-Ball Profile")
    )
  })
  
  output$statcast_kpi_1 <- renderUI({
    player <- selected_statcast_player()
    make_player_kpi_card(player$batted_balls[1], "Batted Balls")
  })
  
  output$statcast_kpi_2 <- renderUI({
    player <- selected_statcast_player()
    make_player_kpi_card(player$avg_exit_velocity[1], "Avg EV")
  })
  
  output$statcast_kpi_3 <- renderUI({
    player <- selected_statcast_player()
    make_player_kpi_card(player$max_exit_velocity[1], "Max EV")
  })
  
  output$statcast_kpi_4 <- renderUI({
    player <- selected_statcast_player()
    make_player_kpi_card(paste0(player$hard_hit_rate[1], "%"), "Hard-Hit Rate")
  })
  
  output$statcast_kpi_5 <- renderUI({
    player <- selected_statcast_player()
    make_player_kpi_card(paste0(player$barrel_like_rate[1], "%"), "Barrel-Like Rate")
  })
  
  output$selected_statcast_events_table <- renderDT({
    req(input$statcast_player)
    
    statcast_data |>
      filter(player_name == input$statcast_player) |>
      select(
        `Game Date` = game_date,
        Event = events,
        Description = description,
        `Exit Velocity` = launch_speed,
        `Launch Angle` = launch_angle,
        Distance = hit_distance_sc,
        `Batted Ball Type` = bb_type
      ) |>
      arrange(desc(`Exit Velocity`)) |>
      pretty_datatable(page_length = 10, escape = FALSE)
  })
  
  output$exit_velocity_table <- renderDT({
    statcast_summary |>
      arrange(desc(avg_exit_velocity)) |>
      select(
        Player = player_name,
        `Batted Balls` = batted_balls,
        `Avg Exit Velocity` = avg_exit_velocity,
        `Max Exit Velocity` = max_exit_velocity,
        `Hard-Hit Rate` = hard_hit_rate
      ) |>
      slice_head(n = 15) |>
      pretty_datatable(page_length = 15, escape = FALSE)
  })
  
  output$barrel_like_table <- renderDT({
    statcast_summary |>
      arrange(desc(barrel_like_rate)) |>
      select(
        Player = player_name,
        `Batted Balls` = batted_balls,
        `Barrel-Like Rate` = barrel_like_rate,
        `Avg Launch Angle` = avg_launch_angle,
        `Avg Exit Velocity` = avg_exit_velocity
      ) |>
      slice_head(n = 15) |>
      pretty_datatable(page_length = 15, escape = FALSE)
  })
  
  output$statcast_table <- renderDT({
    statcast_summary |>
      pretty_datatable(page_length = 15, escape = FALSE)
  })
  
  output$model_summary_table <- renderDT({
    make_model_summary_table(hitter_war_model) |>
      pretty_datatable(page_length = 10, escape = FALSE)
  })
  
  output$model_coefficients_table <- renderDT({
    make_model_coefficients(hitter_war_model) |>
      select(Metric = term, Estimate = estimate, `P-Value` = p.value) |>
      pretty_datatable(page_length = 10, escape = FALSE)
  })
  
  output$model_coefficients_plot <- renderPlot({
    plot_model_coefficients(hitter_war_model)
  })
  
  output$model_residuals_table <- renderDT({
    make_top_model_residuals(hitters, hitter_war_model) |>
      make_player_display() |>
      select(
        Player = player_display,
        Team = team,
        `Actual WAR` = actual_war,
        `Predicted WAR` = predicted_war,
        Difference = difference
      ) |>
      pretty_datatable(page_length = 15, escape = FALSE)
  })
}

shinyApp(ui, server)