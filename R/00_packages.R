options(repos = c(CRAN = "https://cloud.r-project.org"))

required_packages <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "dplyr",
  "ggplot2",
  "DT",
  "gt",
  "broom",
  "forcats",
  "scales",
  "stringr",
  "purrr"
)

invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(
    library(pkg, character.only = TRUE)
  )
}))