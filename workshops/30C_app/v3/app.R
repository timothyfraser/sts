# ============================================================
# app.R — 26C Network Analytics Dashboard
# Stage 3: Real Data Integration
# ============================================================

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(igraph)
library(tidygraph)
library(ggraph)
library(plotly)
library(purrr)
library(jsonlite)
library(glue)
library(tibble)

# Source helper R files (same directory as app.R)
app_env <- environment()
source("R/helpers.R",         local = app_env)
source("R/fake_data.R",       local = app_env)   # kept for reference, no longer called
source("R/placeholder_fns.R", local = app_env)   # kept for reference, no longer called
source("functions/coaffiliate.R")                # needed for FAKE_002/005 replacements
source("functions/graph_join_list.R")            # helper used to merge mapped subgraphs

# ============================================================
# LOAD REAL DATA (once, at startup)
# FAKE_001 / FAKE_002 / FAKE_003 / FAKE_004 / FAKE_005 all
# derive from this single RDS file.
# ============================================================
rds_candidates = c(
  "data/committees/graph_bipartite.rds",
  "../../../data/committees/graph_bipartite.rds"
)
rds_path = rds_candidates[file.exists(rds_candidates)][1]

# Fallback loader so the app remains runnable during setup/debugging.
build_synthetic_bipartite = function(n_committees = 30, n_members = 160) {
  set.seed(5460)
  geos = c("iwate", "miyagi", "fukushima", "national")
  committee_types = c("response", "recovery", "coordination", "finance")

  committees = tibble::tibble(
    name = paste0("committee_", seq_len(n_committees)),
    type = TRUE,
    geography = sample(geos, n_committees, replace = TRUE),
    committee_type = sample(committee_types, n_committees, replace = TRUE)
  )

  members = tibble::tibble(
    name = paste0("member_", seq_len(n_members)),
    type = FALSE,
    geography = sample(geos, n_members, replace = TRUE),
    committee_type = NA_character_
  )

  nodes = dplyr::bind_rows(committees, members)

  member_committee_links = purrr::map_dfr(seq_len(n_members), function(i) {
    n_links = sample(1:4, 1)
    chosen_committees = sample(seq_len(n_committees), n_links, replace = FALSE)
    tibble::tibble(
      from = n_committees + i,
      to = chosen_committees,
      weight = 1
    )
  })

  tidygraph::tbl_graph(nodes = nodes, edges = member_committee_links, directed = FALSE)
}

g = if (!is.na(rds_path) && file.exists(rds_path)) {
  readr::read_rds(rds_path)
} else {
  warning(paste0(
    "Data file not found in candidates: ", paste(rds_candidates, collapse = ", "),
    ". Using synthetic graph so the app can still run."
  ))
  build_synthetic_bipartite()
}

# ---- Build coaffiliation graph (gco) ----
# REAL DATA: replacing FAKE_002 basis
# type = FALSE → project to committee-committee graph
gco <- coaffiliate(graph = g, type = FALSE, names = TRUE,
                   weight = "weight", diag = FALSE)

# Reattach committee node metadata lost during projection.
committee_meta = g |>
  tidygraph::activate("nodes") |>
  dplyr::as_tibble() |>
  dplyr::filter(type == TRUE) |>
  dplyr::select(name, geography, committee_type)

gco = gco |>
  tidygraph::activate("nodes") |>
  dplyr::left_join(committee_meta, by = "name")

# ---- Compute centrality on gco (once) ----
# REAL DATA: replacing FAKE_003
gco <- gco |>
  tidygraph::activate("nodes") |>
  dplyr::mutate(
    deg  = tidygraph::centrality_degree(mode = "all"),
    wdeg = tidygraph::centrality_degree(mode = "all", weights = .E()$weight),
    betw = tidygraph::centrality_betweenness(directed = FALSE, weights = .E()$weight)
  )

# ---- Build gmem (iterative coaffiliation per geography) ----
# REAL DATA: replacing FAKE_005
# Adds from_geo to edges then morphs/splits by geography, runs coaffiliate
# per sub-graph (type=TRUE → member-member projection within geography),
# then reassembles without duplicate nodes.
gmem <- tryCatch({
  g |>
    tidygraph::activate("edges") |>
    dplyr::mutate(from_geo = .N()$geography[.E()$from]) |>
    tidygraph::morph(tidygraph::to_split, from_geo, split_by = "edges") |>
    purrr::map(~coaffiliate(graph = .x, type = TRUE, names = TRUE,
                             weight = "weight", diag = FALSE)) |>
    graph_join_list(by = "name", .id = "geography")
}, error = function(e) {
  warning("gmem build failed: ", conditionMessage(e),
          " — geo_stats will fall back to gco summaries.")
  NULL
})

# Load design metadata
metadata <- if (file.exists("design_metadata.json")) {
  jsonlite::read_json("design_metadata.json")
} else {
  list(
    color_palette = list(
      primary = "#1a3a5c", secondary = "#2e6da4",
      accent = "#e8a020", accent2 = "#3cb4c8",
      background = "#f7f9fc", surface = "#ffffff", text = "#1f2d3d"
    ),
    typography  = list(font_main = "'IBM Plex Sans', sans-serif", font_size_base = "16px"),
    layout      = list(sidebar_width = "280px", panel_spacing = "1rem")
  )
}

source("ui.R",     local = app_env)
source("server.R", local = app_env)

shinyApp(ui = ui, server = server)
