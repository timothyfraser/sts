# ============================================================
# R/helpers.R — Shared utility functions
# ============================================================

# Build CSS variable injection string from design_metadata.json
build_css_vars <- function(metadata) {
  cp <- metadata$color_palette
  ty <- metadata$typography
  lo <- metadata$layout
  glue::glue("
:root {{
  --color-primary:   {cp$primary};
  --color-secondary: {cp$secondary};
  --color-accent:    {cp$accent};
  --color-accent2:   {cp$accent2};
  --color-bg:        {cp$background};
  --color-surface:   {cp$surface};
  --color-text:      {cp$text};
  --font-main:       {ty$font_main};
  --font-size-base:  {ty$font_size_base};
  --sidebar-width:   {lo$sidebar_width};
  --spacing-panel:   {lo$panel_spacing};
}}
")
}

# Render a placeholder panel div (used for PLACEHOLDER_001–004)
placeholder_panel_ui <- function(label, note, placeholder_id) {
  tags$div(
    class = "placeholder-panel",
    tags$div(class = "placeholder-icon", "⬜"),
    tags$div(class = "placeholder-label", label),
    tags$div(class = "placeholder-note", note),
    tags$div(
      style = "margin-top:0.5rem;",
      tags$span(class = "placeholder-badge", placeholder_id)
    )
  )
}

# Render a [FAKE] badge span
fake_badge <- function(id) {
  tags$span(
    class = "fake-badge",
    title = "Synthetic data — replaced in Stage 3",
    id
  )
}

# Render a stat card
stat_card <- function(label, value, sub, fake_id = NULL, modifier = "") {
  tags$div(
    class = paste("stat-card", modifier),
    if (!is.null(fake_id)) tags$span(class = "fake-badge fake-badge-inline", fake_id),
    tags$span(class = "stat-label", label),
    tags$span(class = "stat-value", value),
    tags$span(class = "stat-sub",   sub)
  )
}

# Render a concept code box (for code display panels)
concept_box <- function(...) {
  tags$div(class = "concept-box", ...)
}

# Render a callout div
callout_box <- function(..., type = "info") {
  cls <- if (type == "warn") "callout warn" else "callout"
  tags$div(class = cls, ...)
}

# Geography choices for selectInput / filter pills
GEO_CHOICES <- c("All" = "all", "Iwate" = "iwate",
                 "Miyagi" = "miyagi", "Fukushima" = "fukushima",
                 "National" = "national")

# Focal node choices for distance chart
FOCAL_CHOICES <- c("committee_23","committee_1","committee_7","committee_15")
