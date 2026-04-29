# Fake Data Conventions

## IDs

- Synthetic datasets: `FAKE_001`, `FAKE_002`, ...
- Placeholder panels: `PLACEHOLDER_001`, `PLACEHOLDER_002`, ...
- IDs are assigned sequentially and never reused.

## Code Tagging

### In JavaScript (Stage 1)

```javascript
// ============================================================
// FAKEDATA: FAKE_001
// Description: Synthetic county-year emissions data
// Columns: county (string), year (int), nox_kg (float)
// Used in: Bar chart (panel_timeseries), summary stats
// Real replacement: emissions_df from workflow.R ~line 45
// ============================================================
const FAKE_001_emissions = [
  { county: "Albany", year: 2020, nox_kg: 1234.5 },
  // ...
];
```

### In R (Stage 2+)

```r
# FAKEDATA: FAKE_001
# Description: Synthetic county-year emissions data
# Columns: county, year, nox_kg
# Used in: emissionsPlot, summaryTable
# Real replacement: emissions_df (workflow.R ~line 45)
get_fake_emissions <- function() { ... }
```

## UI Badges (Stage 1 and Stage 2)

Every chart/table/stat card that renders fake data must show a badge:

```html
<span class="fake-badge" title="Synthetic data — replaced in Stage 3">[FAKE]</span>
```

```css
.fake-badge {
  font-size: 0.65rem;
  background: #e9ecef;
  color: #6c757d;
  border-radius: 3px;
  padding: 1px 5px;
  margin-left: 6px;
  font-family: monospace;
}
```

## Placeholder Panels

Panels for spatial/network/model outputs that cannot render in HTML:

```html
<div class="placeholder-panel" data-placeholder-id="PLACEHOLDER_001">
  <span class="placeholder-icon">⬜</span>
  <p class="placeholder-label">County Choropleth Map</p>
  <p class="placeholder-note">Spatial render — available in Shiny (Stage 3)</p>
</div>
```

```css
.placeholder-panel {
  background: #f1f3f5;
  border: 2px dashed #ced4da;
  border-radius: 8px;
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: center;
  min-height: 200px;
  color: #868e96;
}
```

## What Counts as "Complex" (Must Be Placeholder in Stage 1)

- Any `sf`, `terra`, `tmap`, `leaflet` spatial output
- Any `igraph`, `tidygraph`, `ggraph` network output
- Any fitted model object (`lm`, `glm`, `lmer`, `brm`, `xgboost`, etc.)
- Any function call taking > ~1 second to run
- Any output requiring external files not bundled in the app

## FAKEDATA_INDEX Format

Maintain as a markdown table in `FAKEDATA_INDEX.md`:

```markdown
| ID | Description | Columns | Used In | Real Replacement | Stage 2 Status | Stage 3 Status |
|----|-------------|---------|---------|-----------------|---------------|---------------|
| FAKE_001 | County emissions | county, year, nox_kg | emissionsPlot, summaryTable | `emissions_df` (workflow.R ~45) | ⏳ Still fake | — |
| PLACEHOLDER_001 | County map | — | countyMap | `counties_sf` + leaflet | ⏳ Stub | — |
```
