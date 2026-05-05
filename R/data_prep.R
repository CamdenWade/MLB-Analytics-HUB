load_processed_data <- function() {
  hitters_path <- "data/processed/hitters.rds"
  pitchers_path <- "data/processed/pitchers.rds"
  statcast_path <- "data/processed/statcast_batter_sample.rds"
  
  if (!file.exists(hitters_path)) {
    stop("Missing data/processed/hitters.rds. Run scripts/01_build_data.R first.")
  }
  
  if (!file.exists(pitchers_path)) {
    stop("Missing data/processed/pitchers.rds. Run scripts/01_build_data.R first.")
  }
  
  hitters <- readRDS(hitters_path)
  pitchers <- readRDS(pitchers_path)
  
  statcast <- if (file.exists(statcast_path)) {
    readRDS(statcast_path)
  } else {
    tibble()
  }
  
  list(
    hitters = hitters,
    pitchers = pitchers,
    statcast = statcast
  )
}