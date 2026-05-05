team_colors <- c(
  "ARI" = "#A71930", "ATL" = "#CE1141", "BAL" = "#DF4601",
  "BOS" = "#BD3039", "CHC" = "#0E3386", "CWS" = "#27251F",
  "CIN" = "#C6011F", "CLE" = "#E31937", "COL" = "#33006F",
  "DET" = "#0C2340", "HOU" = "#EB6E1F", "KC" = "#004687",
  "LAA" = "#BA0021", "LAD" = "#005A9C", "MIA" = "#00A3E0",
  "MIL" = "#12284B", "MIN" = "#002B5C", "NYM" = "#002D72",
  "NYY" = "#003087", "OAK" = "#003831", "PHI" = "#E81828",
  "PIT" = "#FDB827", "SD" = "#2F241D", "SEA" = "#0C2C56",
  "SF" = "#FD5A1E", "STL" = "#C41E3A", "TB" = "#092C5C",
  "TEX" = "#003278", "TOR" = "#134A8E", "WSH" = "#AB0003"
)

get_team_color <- function(team) {
  color <- team_colors[[as.character(team)]]
  
  if (is.null(color)) {
    "#1f77b4"
  } else {
    color
  }
}

detect_player_id_col <- function(data) {
  possible_cols <- c(
    "mlbam_id",
    "mlbamid",
    "x_mlbamid",
    "player_id",
    "playerid",
    "key_mlbam",
    "mlb_id",
    "idfg"
  )
  
  found <- intersect(possible_cols, names(data))
  
  if (length(found) == 0) {
    return(NULL)
  }
  
  found[1]
}

make_headshot_url <- function(player_id) {
  ifelse(
    is.na(player_id) | player_id == "",
    "",
    paste0(
      "https://img.mlbstatic.com/mlb-photos/image/upload/w_80,q_auto:best/v1/people/",
      player_id,
      "/headshot/67/current"
    )
  )
}

make_player_display <- function(data) {
  id_col <- detect_player_id_col(data)
  
  if (is.null(id_col)) {
    data |>
      mutate(player_display = player_name)
  } else {
    data |>
      mutate(
        player_display = paste0(
          "<div style='display:flex; align-items:center; gap:10px;'>",
          "<img src='", make_headshot_url(.data[[id_col]]),
          "' style='width:38px; height:38px; border-radius:50%; object-fit:cover;' ",
          "onerror=\"this.style.display='none'\">",
          "<span style='font-weight:700;'>", player_name, "</span>",
          "</div>"
        )
      )
  }
}

pretty_datatable <- function(data, page_length = 15, escape = FALSE) {
  DT::datatable(
    data,
    escape = escape,
    rownames = FALSE,
    options = list(
      pageLength = page_length,
      scrollX = TRUE,
      autoWidth = TRUE,
      dom = "tip",
      columnDefs = list(
        list(className = "dt-center", targets = "_all")
      )
    ),
    class = "stripe hover compact"
  )
}
