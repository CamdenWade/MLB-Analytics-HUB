# MLB Analytics Hub

An interactive **baseball analytics dashboard** built with **R Shiny** that allows users to explore player performance, generate scouting reports, and analyze Statcast data.

---

## Live Demo

https://camden-wade.shinyapps.io/MLB-Analytics-HUB/

---

## Project Overview

The **MLB Analytics Hub** simulates a front-office analytics tool used by baseball teams to evaluate players, compare performance, and generate scouting insights.

This project demonstrates:
- End-to-end data pipeline (ingestion → cleaning → visualization)
- Interactive dashboard development with Shiny
- Sports analytics using real-world baseball data
- Automated reporting with Quarto
- Predictive modeling (WAR estimation)

---

## Features

### Player Leaderboards
- Filter hitters and pitchers by team
- View advanced metrics like:
  - WAR
  - wRC+
  - OPS
  - ERA / FIP
- Includes player headshots

---

### Player Profiles
- Individual player performance breakdown
- Clean KPI-style visualization
- Team-colored bar charts

---

### Scouting Reports
- Percentile-based player evaluation
- Key metrics ranked vs league
- Downloadable polished reports (Quarto)

---

### Player Comparison
- Side-by-side comparison of two players
- Visual + tabular analysis

---

### Team Dashboard
- Team-level summaries
- Top hitters and pitchers by WAR
- Offensive and pitching metrics

---

### Statcast Analysis
- Exit velocity leaders
- Barrel-like contact rate
- Launch angle vs exit velocity profile

---

### Modeling Insights
- Linear regression model predicting WAR
- Feature importance visualization
- Identify over/under-performing players

---

## Tech Stack

- **R**
- **Shiny**
- **ggplot2**
- **plotly**
- **dplyr / tidyverse**
- **DT (interactive tables)**
- **Quarto (report generation)**

---

## Data Sources

- [FanGraphs](https://www.fangraphs.com/)
- [Baseball Savant (Statcast)](https://baseballsavant.mlb.com/)

---

## Limitations

- Statcast data is a **sample subset** for performance
- WAR model is **simplified (linear regression)**
- Some advanced metrics are not included
- Player images depend on available IDs

---

## Screenshots

*(Highly recommended — add later)*

