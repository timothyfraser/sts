---
name: shiny-pipeline-stage2
description: Use this skill when a user wants to convert an approved HTML mockup dashboard into a real R Shiny app. Triggers when users mention "convert my mockup to Shiny", "stage 2 of the shiny pipeline", "turn the HTML into a Shiny app", "build the Shiny server", or paste a design_metadata.json. Always use this skill before writing any Shiny app code from an HTML mockup. This skill produces a raw HTML/JS Shiny UI (no bslib/shinydashboard) paired with a reactive R server.
---

# Stage 2: HTML Mockup → R Shiny App

## Overview

Convert the approved HTML mockup into a working R Shiny app. The UI is built from raw HTML/JS — no `bslib`, no `shinydashboard`. Complex analytics (spatial, network, models) remain stubbed with clearly labeled fake data. Only straightforward data visualizations are wired to reactive R.

---

## Inputs Required

1. `design_metadata.json` from Stage 1 (colors, layout, panel config, fakedata_index)
2. Original `workflow.R`
3. The `FAKEDATA_INDEX.md` from Stage 1

If any are missing, ask the user before proceeding.

---

## Step 1 — Classify Panels

Using `design_metadata.json` and `workflow.R`, classify every panel into one of three tiers:

| Tier | Type | Stage 2 Handling |
|------|------|-----------------|
| A | Simple reactive (filters → plot/table) | Wire to R reactive fully |
| B | Moderate reactive (model summary, aggregated stats) | Wire partially; flag complexity |
| C | Placeholder (spatial, network, heavy model) | Keep as stub with `FAKEDATA` |

Show this classification to the user and confirm before writing code.

---

## Step 2 — App Structure

Generate a standard Shiny app structure:

```
app/
├── app.R          # or ui.R + server.R
├── R/
│   ├── fake_data.R        # All FAKEDATA stubs live here
│   ├── placeholder_fns.R  # All PLACEHOLDER stubs live here
│   └── helpers.R          # Shared utility functions
├── www/
│   ├── styles.css         # Extracted from mockup, using same CSS variables
│   └── app.js             # Any custom JS (Chart.js configs, etc.)
└── FAKEDATA_INDEX.md      # Updated from Stage 1
```

### ui.R Pattern

Use `htmlTemplate()` or `tagList()` with raw HTML. Example:

```r
ui <- htmltools::htmlTemplate(
  "www/template.html",
  # Shiny output placeholders injected here
  chart_output = plotOutput("emissionsPlot"),
  table_output = tableOutput("summaryTable"),
  map_placeholder = div(class = "placeholder-panel",
    p("[PLACEHOLDER: county_map — spatial render in Stage 3]"))
)
```

Or inline with `tags$`:

```r
ui <- tags$div(
  class = "dashboard-container",
  style = htmltools::css(
    `--color-primary` = metadata$color_palette$primary,
    `--color-bg`      = metadata$color_palette$background
  ),
  # panels built from raw tags$div, tags$canvas, etc.
)
```

**Do not use** `fluidPage()`, `sidebarLayout()`, `bslib::page_*()`, or any layout helper. Pure HTML only.

---

## Step 3 — fake_data.R

All fake data from Stage 1 is extracted into `R/fake_data.R`. Each function is tagged:

```r
# FAKEDATA: FAKE_001
# Description: Synthetic county emissions data
# Columns: county, year, nox_kg
# Used in: emissionsPlot, summaryTable
# Real replacement: emissions_df (workflow.R ~line 45)
get_fake_emissions <- function(n_counties = 10, years = 2018:2024) {
  tidyr::expand_grid(
    county = paste("County", LETTERS[1:n_counties]),
    year   = years
  ) |>
    dplyr::mutate(nox_kg = runif(dplyr::n(), 100, 5000))
}
```

**Rule**: Every fake data function in this file starts with `get_fake_`. Every placeholder function starts with `get_placeholder_`.

---

## Step 4 — placeholder_fns.R

Complex outputs that cannot yet be rendered get stubs:

```r
# PLACEHOLDER: PLACEHOLDER_001
# Description: County choropleth map
# Real implementation: leaflet render using counties_sf (workflow.R ~line 78)
# Stage 3 action: Replace with renderLeaflet() / leafletOutput()
get_placeholder_map <- function() {
  # Returns NULL — UI shows gray placeholder box
  NULL
}
```

In the server, render these as static placeholder divs:

```r
output$countyMap <- renderUI({
  # PLACEHOLDER_001 — replace in Stage 3 with renderLeaflet
  tags$div(
    class = "placeholder-panel",
    tags$p("[PLACEHOLDER: County Map — will render in Stage 3]",
           style = "color: #999; font-style: italic;")
  )
})
```

---

## Step 5 — server.R

For Tier A/B panels, wire reactives normally using data from `fake_data.R`. Label each reactive:

```r
# DATA SOURCE: FAKE_001 (fake) — replace in Stage 3 with emissions_df
emissions_data <- reactive({
  get_fake_emissions()
})
```

Filters/inputs use Shiny inputs that match the mockup sidepanel controls. The design sidepanel itself is **removed** in Stage 2 — its selected values are now baked into the CSS.

---

## Step 6 — Update FAKEDATA_INDEX

Add a `stage2_status` column:

| ID | Description | Used In | Real Replacement | Stage 2 Status |
|----|-------------|---------|-----------------|---------------|
| FAKE_001 | County emissions | emissionsPlot | `emissions_df` | ⏳ Still fake |
| PLACEHOLDER_001 | County map | countyMap | `counties_sf` + leaflet | ⏳ Stub in UI |

---

## Deliverables

1. `app/` folder with full Shiny app code
2. Updated `FAKEDATA_INDEX.md`
3. Brief summary of which panels are Tier A/B (functional) vs. Tier C (stubbed)
4. Instructions: "Test the app with `shiny::runApp('app/')`. When ready for real data, paste your `workflow.R` and updated `FAKEDATA_INDEX.md` into a Stage 3 conversation."
