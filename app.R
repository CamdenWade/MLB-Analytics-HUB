source("R/00_packages.R")
source("R/data_prep.R")
source("R/ui_helpers.R")
source("R/plots.R")
source("R/scouting.R")
source("R/comparison.R")
source("R/team_dashboard.R")
source("R/statcast.R")

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
        "An interactive baseball analytics dashboard built in R Shiny. Explore player leaderboards, scouting reports, player comparisons, team dashboards, Statcast batted-ball data, and downloadable reports."
      )
    ),
    
    layout_column_wrap(
      width = 1 / 3,
      
      card(
        class = "feature-card",
        card_body(
          div(class = "feature-title", "Player Leaderboards"),
          div(
            class = "feature-text",
            "Filter hitter and pitcher leaderboards by team and evaluate production using traditional and advanced metrics."
          )
        )
      ),
      
      card(
        class = "feature-card",
        card_body(
          div(class = "feature-title", "Scouting Reports"),
          div(
            class = "feature-text",
            "Generate percentile-based player scouting profiles and download polished HTML reports."
          )
        )
      ),
      
      card(
        class = "feature-card",
        card_body(
          div(class = "feature-title", "Player Comparison"),
          div(
            class = "feature-text",
            "Compare two hitters or pitchers side-by-side using tables and interactive visualizations."
          )
        )
      ),
      
      card(
        class = "feature-card",
        card_body(
          div(class = "feature-title", "Team Dashboard"),
          div(
            class = "feature-text",
            "Analyze team-level strengths using WAR leaders, offensive production, and pitching performance."
          )
        )
      ),
      
      card(
        class = "feature-card",
        card_body(
          div(class = "feature-title", "Statcast Analysis"),
          div(
            class = "feature-text",
            "Explore exit velocity, launch angle, hard-hit rate, and batted-ball profiles from Baseball Savant data."
          )
        )
      ),
      
      card(
        class = "feature-card",
        card_body(
          div(class = "feature-title", "Portfolio Skills"),
          div(class = "metric-pill", "R"),
          div(class = "metric-pill", "Shiny"),
          div(class = "metric-pill", "Quarto"),
          div(class = "metric-pill", "ggplot2"),
          div(class = "metric-pill", "Statcast")
        )
      )
    )
  ),
  
  nav_panel(
    "Hitter Leaderboard",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "hitter_team",
          "Team",
          choices = c("All", sort(unique(hitters$team))),
          selected = "All"
        )
      ),
      card(
        card_header("Hitter Leaderboard"),
        DTOutput("hitter_table")
      )
    )
  ),
  
  nav_panel(
    "Pitcher Leaderboard",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "pitcher_team",
          "Team",
          choices = c("All", sort(unique(pitchers$team))),
          selected = "All"
        )
      ),
      card(
        card_header("Pitcher Leaderboard"),
        DTOutput("pitcher_table")
      )
    )
  ),
  
  nav_panel(
    "Player Profiles",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "profile_type",
          "Player Type",
          choices = c("Hitter", "Pitcher")
        ),
        uiOutput("player_selector")
      ),
      card(
        card_header("Player Profile"),
        plotlyOutput("profile_plot")
      )
    )
  ),
  
  nav_panel(
    "Scouting Report",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "scout_type",
          "Player Type",
          choices = c("Hitter", "Pitcher")
        ),
        uiOutput("scout_player_selector"),
        downloadButton(
          "download_scouting_report",
          "Download Scouting Report"
        )
      ),
      layout_column_wrap(
        width = 1 / 2,
        card(
          card_header("Percentile Chart"),
          plotlyOutput("scout_percentile_plot")
        ),
        card(
          card_header("Scouting Metrics"),
          DTOutput("scout_table")
        )
      )
    )
  ),
  
  nav_panel(
    "Player Comparison",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "compare_type",
          "Player Type",
          choices = c("Hitter", "Pitcher")
        ),
        uiOutput("compare_player_1_selector"),
        uiOutput("compare_player_2_selector")
      ),
      layout_column_wrap(
        width = 1,
        card(
          card_header("Comparison Chart"),
          plotlyOutput("comparison_plot")
        ),
        card(
          card_header("Comparison Table"),
          DTOutput("comparison_table")
        )
      )
    )
  ),
  
  nav_panel(
    "Team Dashboard",
    layout_sidebar(
      sidebar = sidebar(
        selectInput(
          "team_dashboard_team",
          "Team",
          choices = team_choices,
          selected = team_choices[1]
        )
      ),
      layout_column_wrap(
        width = 1 / 3,
        card(card_header("Team Summary"), DTOutput("team_summary_table")),
        card(card_header("Top Hitters"), DTOutput("team_hitters_table")),
        card(card_header("Top Pitchers"), DTOutput("team_pitchers_table"))
      ),
      layout_column_wrap(
        width = 1 / 2,
        card(card_header("Hitter WAR Leaders"), plotlyOutput("team_hitter_plot")),
        card(card_header("Pitcher WAR Leaders"), plotlyOutput("team_pitcher_plot"))
      )
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
      layout_column_wrap(
        width = 1 / 2,
        card(
          card_header("Exit Velocity Leaders"),
          plotlyOutput("exit_velocity_plot")
        ),
        card(
          card_header("Selected Player Launch Profile"),
          plotlyOutput("launch_profile_plot")
        )
      ),
      card(
        card_header("Statcast Summary"),
        DTOutput("statcast_table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_hitters <- reactive({
    if (input$hitter_team == "All") {
      hitters
    } else {
      hitters |> filter(team == input$hitter_team)
    }
  })
  
  filtered_pitchers <- reactive({
    if (input$pitcher_team == "All") {
      pitchers
    } else {
      pitchers |> filter(team == input$pitcher_team)
    }
  })
  
  output$hitter_table <- renderDT({
    filtered_hitters() |>
      select(player_name, team, pa, hr, r, rbi, sb, avg, obp, slg, ops, w_rc_plus, war) |>
      datatable(options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$pitcher_table <- renderDT({
    filtered_pitchers() |>
      select(player_name, team, ip, w, l, era, fip, whip, k_9, bb_9, hr_9, war) |>
      datatable(options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })
  
  output$player_selector <- renderUI({
    if (input$profile_type == "Hitter") {
      selectInput("selected_player", "Player", choices = sort(unique(hitters$player_name)))
    } else {
      selectInput("selected_player", "Player", choices = sort(unique(pitchers$player_name)))
    }
  })
  
  output$profile_plot <- renderPlotly({
    req(input$selected_player)
    
    if (input$profile_type == "Hitter") {
      p <- plot_hitter_profile(hitters, input$selected_player)
    } else {
      p <- plot_pitcher_profile(pitchers, input$selected_player)
    }
    
    ggplotly(p)
  })
  
  output$scout_player_selector <- renderUI({
    if (input$scout_type == "Hitter") {
      selectInput("scout_player", "Player", choices = sort(unique(hitters$player_name)))
    } else {
      selectInput("scout_player", "Player", choices = sort(unique(pitchers$player_name)))
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
      datatable(
        options = list(pageLength = 10, searching = FALSE, paging = FALSE, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  output$scout_percentile_plot <- renderPlotly({
    req(scouting_data())
    ggplotly(plot_percentiles(scouting_data(), input$scout_player))
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
      
      quarto::quarto_render(
        input = temp_report,
        output_file = file,
        execute_params = list(
          player_name = input$scout_player,
          player_type = input$scout_type,
          season = 2024
        )
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
      selectInput(
        "compare_player_2",
        "Player 2",
        choices = sort(unique(hitters$player_name)),
        selected = sort(unique(hitters$player_name))[2]
      )
    } else {
      selectInput(
        "compare_player_2",
        "Player 2",
        choices = sort(unique(pitchers$player_name)),
        selected = sort(unique(pitchers$player_name))[2]
      )
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
      datatable(
        options = list(pageLength = 5, searching = FALSE, paging = FALSE, scrollX = TRUE),
        rownames = FALSE
      )
  })
  
  output$comparison_plot <- renderPlotly({
    req(input$compare_player_1, input$compare_player_2)
    
    if (input$compare_type == "Hitter") {
      p <- plot_hitter_comparison(hitters, input$compare_player_1, input$compare_player_2)
    } else {
      p <- plot_pitcher_comparison(pitchers, input$compare_player_1, input$compare_player_2)
    }
    
    ggplotly(p)
  })
  
  output$team_summary_table <- renderDT({
    make_team_summary(hitters, pitchers, input$team_dashboard_team) |>
      datatable(options = list(searching = FALSE, paging = FALSE, info = FALSE, scrollX = TRUE), rownames = FALSE)
  })
  
  output$team_hitters_table <- renderDT({
    make_top_team_hitters(hitters, input$team_dashboard_team) |>
      datatable(options = list(searching = FALSE, paging = FALSE, info = FALSE, scrollX = TRUE), rownames = FALSE)
  })
  
  output$team_pitchers_table <- renderDT({
    make_top_team_pitchers(pitchers, input$team_dashboard_team) |>
      datatable(options = list(searching = FALSE, paging = FALSE, info = FALSE, scrollX = TRUE), rownames = FALSE)
  })
  
  output$team_hitter_plot <- renderPlotly({
    ggplotly(plot_team_hitters(hitters, input$team_dashboard_team))
  })
  
  output$team_pitcher_plot <- renderPlotly({
    ggplotly(plot_team_pitchers(pitchers, input$team_dashboard_team))
  })
  
  output$exit_velocity_plot <- renderPlotly({
    ggplotly(plot_exit_velocity_leaders(statcast_summary))
  })
  
  output$launch_profile_plot <- renderPlotly({
    req(input$statcast_player)
    ggplotly(plot_launch_profile(statcast_data, input$statcast_player))
  })
  
  output$statcast_table <- renderDT({
    statcast_summary |>
      datatable(
        options = list(pageLength = 15, scrollX = TRUE),
        rownames = FALSE
      )
  })
}

shinyApp(ui, server)
