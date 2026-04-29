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

# Source helper R files
source("R/fake_data.R")
source("R/placeholder_fns.R")
source("R/helpers.R")

# Load design metadata (baked-in colors/layout from Stage 1)
metadata <- jsonlite::read_json("design_metadata.json")

# Source UI and server
source("ui.R")
source("server.R")

shinyApp(ui = ui, server = server)
