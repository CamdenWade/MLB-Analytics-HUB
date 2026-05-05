source("R/00_packages.R")

dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)

season <- 2025

clean_leaderboard <- function(df) {
  df <- df |> janitor::clean_names()
  
  if ("team_name" %in% names(df)) df <- df |> rename(team = team_name)
  if ("teamid" %in% names(df) && !"team" %in% names(df)) df <- df |> mutate(team = as.character(teamid))
  if ("team_abbrev" %in% names(df) && !"team" %in% names(df)) df <- df |> rename(team = team_abbrev)
  if ("x" %in% names(df) && !"player_name" %in% names(df)) df <- df |> rename(player_name = x)
  if ("player" %in% names(df) && !"player_name" %in% names(df)) df <- df |> rename(player_name = player)
  if ("name" %in% names(df) && !"player_name" %in% names(df)) df <- df |> rename(player_name = name)
  if (!"team" %in% names(df)) df <- df |> mutate(team = "Unknown")
  
  df
}

message("Pulling hitter leaderboard...")

hitters <- baseballr::fg_batter_leaders(
  startseason = season,
  endseason = season,
  qual = 100
) |>
  clean_leaderboard()

message("Pulling pitcher leaderboard...")

pitchers <- baseballr::fg_pitcher_leaders(
  startseason = season,
  endseason = season,
  qual = 50
) |>
  clean_leaderboard()

saveRDS(hitters, "data/processed/hitters.rds")
saveRDS(pitchers, "data/processed/pitchers.rds")

message("Saved hitter and pitcher data.")

message("Pulling Statcast sample directly from Baseball Savant...")

statcast_url <- paste0(
  "https://baseballsavant.mlb.com/statcast_search/csv?",
  "all=true",
  "&hfPT=",
  "&hfAB=",
  "&hfBBT=",
  "&hfPR=",
  "&hfZ=",
  "&stadium=",
  "&hfBBL=",
  "&hfNewZones=",
  "&hfGT=R%7CPO%7CS%7C",
  "&hfC=",
  "&hfSea=2024%7C",
  "&hfSit=",
  "&hfOuts=",
  "&opponent=",
  "&pitcher_throws=",
  "&batter_stands=",
  "&hfSA=",
  "&player_type=batter",
  "&hfInfield=",
  "&team=",
  "&position=",
  "&hfOutfield=",
  "&hfRO=",
  "&home_road=",
  "&game_date_gt=2024-04-01",
  "&game_date_lt=2024-04-03",
  "&hfFlag=",
  "&hfPull=",
  "&metric_1=",
  "&hfInn=",
  "&min_pitches=0",
  "&min_results=0",
  "&group_by=name",
  "&sort_col=pitches",
  "&player_event_sort=h_launch_speed",
  "&sort_order=desc",
  "&min_abs=0",
  "&type=details"
)

statcast_raw <- readr::read_csv(
  statcast_url,
  show_col_types = FALSE,
  progress = TRUE
) |>
  janitor::clean_names()

message("Statcast rows pulled: ", nrow(statcast_raw))
message("Statcast columns pulled: ", ncol(statcast_raw))

if (!"player_name" %in% names(statcast_raw)) {
  stop("Statcast data did not include player_name. Check names(statcast_raw).")
}

needed_cols <- c(
  "player_name",
  "events",
  "description",
  "launch_speed",
  "launch_angle",
  "hit_distance_sc",
  "estimated_ba_using_speedangle",
  "estimated_woba_using_speedangle",
  "bb_type",
  "game_date"
)

existing_cols <- intersect(needed_cols, names(statcast_raw))

statcast_clean <- statcast_raw |>
  select(all_of(existing_cols)) |>
  filter(!is.na(player_name))

saveRDS(statcast_clean, "data/processed/statcast_batter_sample.rds")

message("Saved Statcast data with ", nrow(statcast_clean), " rows.")
message("Data build complete.")