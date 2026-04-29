---
name: shiny-pipeline-stage1
description: Use this skill when a user wants to convert a workflow.R script into an interactive HTML/JS mockup dashboard with a design sidepanel. Triggers when users mention building a Shiny dashboard from an R script, want to prototype a dashboard UI before writing Shiny server code, want to visualize their R workflow as a dashboard, or say things like "turn my workflow into a dashboard", "mockup my shiny app", "design a dashboard from my R script", or "stage 1 of the shiny pipeline". Always use this skill before generating any HTML dashboard mockup for a Shiny app project.
---

# Stage 1: HTML Mockup with Design Sidepanel

## Overview

Convert a `workflow.R` linear script into a standalone HTML/JS/React mockup dashboard. All data is synthetic. A collapsible sidepanel lets the developer customize design and layout interactively. At the end, a `design_metadata.json` and `FAKEDATA_INDEX` are exported for Stage 2.

---

## Step 0 — Parse `workflow.R`

Before asking elicitation questions, extract the following from the script:

1. **Output objects**: What does the workflow produce? (data frames, model outputs, plots, summary stats, spatial objects, network objects)
2. **Plot types used**: ggplot2 geoms, tmap, igraph, etc.
3. **Complex outputs** (flag these — they cannot be rendered in HTML):
   - Spatial: `sf`, `tmap`, `leaflet`, `terra`
   - Network: `igraph`, `tidygraph`, `ggraph`
   - Statistical models: `lm`, `glm`, `lmer`, `brm`, `stan`, etc.
   - Large/slow computations
4. **Key variables and column names** that will drive the dashboard (filters, axes, groupings)

---

## Step 1 — Elicitation Questions

Read `references/elicitation-questions.md` and ask the user the Stage 1 questions. Do not skip this. Present as a numbered list with options where applicable.

---

## Step 2 — Generate the HTML Mockup

### Required Structure

The mockup is a single `.html` file with:

```
[Sidepanel] [Main Dashboard Area]
```

The **sidepanel** contains:
- Color theme picker (primary, secondary, accent, background, text)
- Font selector (sans-serif options)
- Layout toggle (e.g., 1-column vs. 2-column grid)
- Panel visibility toggles (show/hide each chart panel)
- Spacing/padding slider
- A button: **"Export Design JSON"** — copies `design_metadata.json` to clipboard

The **main area** contains dashboard panels driven by synthetic data.

### Fake Data Rules

See `references/fakedata-conventions.md` for full conventions. Summary:

- Every synthetic dataset is defined in a clearly labeled `// FAKEDATA` block at the top of the `<script>` section.
- Each fake dataset has an ID: `FAKE_001`, `FAKE_002`, etc.
- Every chart/table/stat that uses fake data displays a small `[FAKE]` badge in the UI (light gray, dismissible in final export).
- Complex outputs (spatial, network, model) get a **placeholder panel** — a gray box with text: `[PLACEHOLDER: {output_name} — will render in Shiny]`. These are tagged `PLACEHOLDER_001`, etc.

### Synthetic Data Generation

Generate plausible synthetic data that:
- Matches the column names and types from `workflow.R`
- Has realistic ranges (don't use `1,2,3` for a year column — use `2018:2024`)
- Includes the same grouping variables (e.g., county names, categories)

Use inline JavaScript arrays — no external data files.

### Chart Library

Default to **Chart.js** (CDN). For tables use plain HTML. For maps/network placeholders use a styled gray `<div>`.

### Design Tokens

Define all colors, fonts, spacing as CSS variables at the top:

```css
:root {
  --color-primary: #2c7bb6;
  --color-secondary: #abd9e9;
  --color-accent: #d7191c;
  --color-bg: #f8f9fa;
  --color-text: #212529;
  --font-main: 'Inter', sans-serif;
  --spacing-panel: 1.5rem;
  --border-radius: 8px;
}
```

The sidepanel JS updates these variables live.

---

## Step 3 — Generate `FAKEDATA_INDEX`

After generating the mockup, output a `FAKEDATA_INDEX` table:

| ID | Description | Columns | Used In | Real Replacement |
|----|-------------|---------|---------|-----------------|
| FAKE_001 | Synthetic county emissions data | county, year, nox_kg | Bar chart (Panel 2), Summary stats | `emissions_df` from `workflow.R` line ~45 |
| PLACEHOLDER_001 | County choropleth map | — | Map Panel | `sf` object `counties_sf`, rendered via leaflet in Shiny |

---

## Step 4 — Export `design_metadata.json`

The "Export Design JSON" button in the sidepanel copies this structure:

```json
{
  "color_palette": {
    "primary": "#2c7bb6",
    "secondary": "#abd9e9",
    "accent": "#d7191c",
    "background": "#f8f9fa",
    "text": "#212529"
  },
  "typography": {
    "font_main": "Inter",
    "font_size_base": "14px"
  },
  "layout": {
    "columns": 2,
    "sidebar_width": "280px",
    "panel_spacing": "1.5rem"
  },
  "panels": [
    {"id": "panel_summary", "visible": true, "title": "Summary Stats"},
    {"id": "panel_timeseries", "visible": true, "title": "Emissions Over Time"},
    {"id": "panel_map", "visible": true, "title": "County Map (Placeholder)"}
  ],
  "fakedata_index": [
    {
      "id": "FAKE_001",
      "description": "Synthetic county emissions data",
      "columns": ["county", "year", "nox_kg"],
      "used_in": ["panel_timeseries", "panel_summary"],
      "real_replacement": "emissions_df"
    }
  ]
}
```

---

## Deliverables

1. `dashboard_mockup.html` — single file, self-contained
2. `FAKEDATA_INDEX.md` — table of all fake data and placeholders
3. Instructions: "When you're happy with the design, click 'Export Design JSON' and paste it into your Stage 2 conversation along with your `workflow.R`."
