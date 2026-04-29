# Raw HTML/JS Patterns for Shiny

## Core Principle

In this pipeline, the Shiny UI is built from raw HTML tags, not from layout helpers like `fluidPage()`, `sidebarLayout()`, or `bslib::page_*()`. This preserves exact visual fidelity to the Stage 1 HTML mockup.

---

## Injecting CSS Variables from design_metadata.json

Read `design_metadata.json` at app startup and inject as inline CSS:

```r
# In global.R or top of app.R
metadata <- jsonlite::read_json("design_metadata.json")

css_vars <- glue::glue("
:root {{
  --color-primary: {metadata$color_palette$primary};
  --color-secondary: {metadata$color_palette$secondary};
  --color-accent: {metadata$color_palette$accent};
  --color-bg: {metadata$color_palette$background};
  --color-text: {metadata$color_palette$text};
  --font-main: '{metadata$typography$font_main}', sans-serif;
  --spacing-panel: {metadata$layout$panel_spacing};
}}
")

# In ui.R
tags$head(
  tags$link(rel = "stylesheet", href = "styles.css"),
  tags$style(HTML(css_vars))
)
```

---

## Building Panels with `tags$`

```r
dashboard_panel <- function(id, title, content, fake_id = NULL) {
  badge <- if (!is.null(fake_id)) {
    tags$span(class = "fake-badge", title = "Synthetic data", "[FAKE]")
  }
  tags$div(
    class = "dashboard-panel",
    id = paste0("panel-", id),
    tags$div(class = "panel-header",
      tags$h3(title, badge)
    ),
    tags$div(class = "panel-body", content)
  )
}
```

---

## Chart.js in Shiny

Chart.js renders into a `<canvas>`. Use `renderUI` + `session$sendCustomMessage`:

```r
# ui.R
tags$canvas(id = "emissionsChart", width = "100%", height = "300")

# www/app.js
Shiny.addCustomMessageHandler("updateEmissionsChart", function(data) {
  const ctx = document.getElementById("emissionsChart").getContext("2d");
  if (window.emissionsChart) window.emissionsChart.destroy();
  window.emissionsChart = new Chart(ctx, {
    type: "bar",
    data: {
      labels: data.labels,
      datasets: [{ data: data.values, backgroundColor: getComputedStyle(document.documentElement).getPropertyValue("--color-primary") }]
    }
  });
});

# server.R
observe({
  df <- emissions_data()
  session$sendCustomMessage("updateEmissionsChart", list(
    labels = df$year,
    values = df$nox_kg
  ))
})
```

---

## Shiny Inputs as Styled HTML

Replace `selectInput()` with raw HTML + `shinyjs` or standard Shiny binding:

```r
# Raw select that Shiny can read as input$year_filter
tags$select(
  id = "year_filter",
  class = "custom-select",
  # Shiny picks this up automatically as input$year_filter
  tags$option(value = "2020", "2020"),
  tags$option(value = "2021", "2021")
)
```

For sliders and checkboxes, use standard Shiny input functions but wrap in custom `tags$div` for styling:

```r
tags$div(class = "filter-group",
  tags$label("Select Year", `for` = "year_filter"),
  sliderInput("year_filter", label = NULL, min = 2018, max = 2024, value = 2022, sep = "")
)
```

Then override default Shiny slider CSS in `www/styles.css` using `.irs-*` selectors.

---

## leafletOutput in Raw HTML Context

```r
# ui.R
tags$div(
  class = "dashboard-panel",
  tags$div(class = "panel-header", tags$h3("County Map")),
  tags$div(class = "panel-body", style = "padding: 0;",
    leaflet::leafletOutput("countyMap", width = "100%", height = "400px")
  )
)

# server.R
output$countyMap <- leaflet::renderLeaflet({ ... })
```

---

## Avoiding Common Pitfalls

| Pitfall | Fix |
|---------|-----|
| `fluidRow()` / `column()` breaks CSS grid | Use `tags$div(class = "grid-row")` with CSS grid |
| `shinydashboard` box shadow overwrites custom styles | Don't use shinydashboard at all |
| `bslib` injects Bootstrap variables that override `--color-*` | Don't load bslib |
| Shiny's default CSS for `selectInput` conflicts with custom styles | Use `tags$select` directly or override `.selectize-*` in CSS |
| Chart.js canvas gets wrong size on load | Call `chart.resize()` in a `$(window).on('shiny:connected')` handler |
