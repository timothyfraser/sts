# ============================================================
# app.R — 26C Network Analytics Dashboard
# Stage 2: HTML Mockup → Shiny App
# ============================================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(igraph)
library(tidygraph)
library(ggraph)
library(purrr)
library(jsonlite)

# Source helper R files (same directory as app.R)
app_env <- environment()
source("fake_data.R", local = app_env)
source("placeholder_fns.R", local = app_env)
source("helpers.R", local = app_env)

# Load design metadata (baked-in colors/layout from Stage 1)
# Fall back to defaults when the JSON file is not present.
metadata <- if (file.exists("design_metadata.json")) {
  jsonlite::read_json("design_metadata.json")
} else {
  list(
    color_palette = list(
      primary = "#1a3a5c",
      secondary = "#2e6da4",
      accent = "#e8a020",
      accent2 = "#3cb4c8",
      background = "#f7f9fc",
      surface = "#ffffff",
      text = "#1f2d3d"
    ),
    typography = list(
      font_main = "'IBM Plex Sans', sans-serif",
      font_size_base = "16px"
    ),
    layout = list(
      sidebar_width = "280px",
      panel_spacing = "1rem"
    )
  )
}

# Source UI and server
source("ui.R", local = app_env)
source("server.R", local = app_env)

shinyApp(ui = ui, server = server)
