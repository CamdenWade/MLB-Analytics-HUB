packages <- c(
  "shiny",
  "bslib",
  "tidyverse",
  "baseballr",
  "DT",
  "plotly",
  "scales",
  "janitor",
  "quarto",
  "gt",
  "broom"
)

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

invisible(lapply(packages, install_if_missing))
invisible(lapply(packages, library, character.only = TRUE))