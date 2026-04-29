---
name: shiny-pipeline-stage3
description: Use this skill when a user wants to replace fake/synthetic data stubs in their Shiny app with real data from their workflow.R script. Triggers when users mention "stage 3 of the shiny pipeline", "replace fake data", "wire up real data", "swap stubs for real data", "integrate my workflow into shiny", or present a FAKEDATA_INDEX with items marked as still fake. Always use this skill before touching any fake_data.R or placeholder_fns.R stubs. Never silently replace fake data — every swap must be logged.
---

# Stage 3: Real Data Integration

## Overview

Replace all `FAKEDATA` stubs and `PLACEHOLDER` functions with real data calls from `workflow.R`. Every swap is explicit, logged, and tested. Nothing is silently changed.

---

## Inputs Required

1. `app/` folder from Stage 2
2. `workflow.R`
3. `FAKEDATA_INDEX.md` with current status

If any are missing, ask the user before proceeding.

---

## Step 1 — Audit the FAKEDATA_INDEX

Read every row of `FAKEDATA_INDEX.md`. For each item:

1. Find its `real_replacement` reference in `workflow.R`
2. Confirm the object name, type, and structure
3. Identify any dependencies (packages, file paths, external data) needed
4. Flag blockers: missing files, API keys, heavy computations that need caching

Show the user an updated audit table before touching any code:

| ID | Real Object | Type | Dependencies | Blocker? |
|----|------------|------|-------------|---------|
| FAKE_001 | `emissions_df` | data.frame | `data/emissions.csv` | None |
| PLACEHOLDER_001 | `counties_sf` | sf object | `sf`, `data/counties.gpkg` | Needs `leafletOutput` in UI |

Get user confirmation before proceeding.

---

## Step 2 — Integration Order

Replace in this order to minimize breakage:

1. **Simple data frames** (Tier A panels first)
2. **Aggregated/model summaries** (Tier B)
3. **Spatial outputs** (requires UI changes: swap placeholder div → `leafletOutput`)
4. **Network outputs** (requires UI changes: swap placeholder div → custom render)
5. **Statistical model outputs** (may require caching with `memoise` or `shinyCache`)

---

## Step 3 — Replacement Pattern

For each swap, follow this exact pattern:

### In `R/fake_data.R`

Comment out (do not delete) the fake function:

```r
# FAKEDATA REPLACED: FAKE_001
# Replaced in Stage 3 — keeping for reference
# get_fake_emissions <- function(...) { ... }
```

### In `server.R`

Replace the fake call with the real one, with a comment:

```r
# DATA SOURCE: emissions_df (REAL — FAKE_001 replaced Stage 3)
# From workflow.R lines 40-52: reads data/emissions.csv, cleans, aggregates
emissions_data <- reactive({
  req(input$year_filter)
  emissions_df |>
    dplyr::filter(year == input$year_filter)
})
```

### For Placeholders requiring UI changes

When a `PLACEHOLDER` panel needs a new Shiny output type (e.g., `leafletOutput`, `plotOutput`):

1. Update `ui.R` to replace the placeholder `div` with the correct output widget
2. Update `server.R` with the real render function
3. Add required packages to the top of `app.R`

Example for a leaflet map:

```r
# ui.R — replacing PLACEHOLDER_001
# Before: tags$div(class = "placeholder-panel", ...)
# After:
leaflet::leafletOutput("countyMap", width = "100%", height = "400px")

# server.R
output$countyMap <- leaflet::renderLeaflet({
  # REAL DATA: counties_sf (PLACEHOLDER_001 replaced Stage 3)
  counties_sf |>
    dplyr::left_join(emissions_data(), by = "county") |>
    leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addPolygons(fillColor = ~pal(nox_kg))
})
```

---

## Step 4 — Dependency Hygiene

After all swaps, audit the top of `app.R`:

```r
# Auto-generated dependency block — Stage 3
library(shiny)
library(dplyr)
library(ggplot2)
# Add real-data dependencies:
library(sf)         # for counties_sf
library(leaflet)    # for map panel
library(tidyr)
```

Remove any packages only needed for fake data generation (`purrr::map` for synthetic loops, etc.) if they're no longer used.

---

## Step 5 — Remove FAKE Badges from UI

The `[FAKE]` UI badges added in Stage 1 must be removed for each panel that now has real data. Search `www/styles.css` and `ui.R` for `.fake-badge` and remove them panel by panel as each is verified.

---

## Step 6 — Final FAKEDATA_INDEX

Mark every replaced item:

| ID | Description | Stage 3 Status |
|----|-------------|---------------|
| FAKE_001 | County emissions | ✅ Replaced with `emissions_df` |
| PLACEHOLDER_001 | County map | ✅ Replaced with `leafletOutput` + `counties_sf` |

If any items are still fake after this stage (e.g., blocked by missing data), mark them:

| ID | Description | Stage 3 Status |
|----|-------------|---------------|
| FAKE_003 | External API data | ❌ Blocked — API key not available |

---

## Step 7 — Smoke Test Checklist

Before declaring done, confirm:

- [ ] `shiny::runApp('app/')` launches without errors
- [ ] All `[FAKE]` badges removed from UI (or documented exceptions)
- [ ] `fake_data.R` has no active `get_fake_*` calls in `server.R`
- [ ] `FAKEDATA_INDEX.md` fully updated
- [ ] All required data files are present relative to `app/`

---

## Deliverables

1. Updated `app/` with real data integrations
2. Final `FAKEDATA_INDEX.md` showing all ✅ / ❌ statuses
3. Brief summary of any remaining stubs and why they're blocked
