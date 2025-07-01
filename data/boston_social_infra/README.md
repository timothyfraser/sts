# README `/boston_social_infra`

## 🏙️ Boston Social Infrastructure Dataset

This folder contains geospatial and demographic data for social infrastructure, neighborhoods, and transit lines across Boston, MA. 
The files are suitable for spatial and statistical analysis of urban infrastructure, equity, and community resources.

Sourced from Studies: 

- Fraser, T., Cherdchaiyapong, N., Tekle, W., Thomas, E., Zayas, J., Page-Tan, C., & Aldrich, D. P. (2022). Trust but verify: Validating new measures for mapping social infrastructure in cities. Urban Climate, 46, 101287.
- Fraser, T., Feeley, O., Ridge, A., Cervini, A., Rago, V., Gilmore, K., ... & Berliavsky, I. (2024). How far I’ll go: Social infrastructure accessibility and proximity in urban neighborhoods. Landscape and Urban Planning, 241, 104922.
- Fraser, T., Yabe, T., Aldrich, D. P., & Moro, E. (2024). The great equalizer? Mixed effects of social infrastructure on diverse encounters in cities. Computers, Environment and Urban Systems, 113, 102173.

---

## 📁 Contents

| File | Description |
|------|-------------|
| `boston_block_groups.geojson` | Spatial polygons for Boston block groups (used for census-level granularity) |
| `boston_grid.geojson`         | Regular spatial grid overlaying Boston, used for spatial aggregation |
| `boston_social_infra.geojson` | Point data of social infrastructure locations (e.g., parks, community centers) |
| `boston_train_lines.geojson`  | MBTA subway and light rail routes (LINESTRING geometries) |
| `boston_census_data.csv`      | Socio-demographic statistics matched to `boston_grid.geojson` cells |

---

## 🗺️ Spatial Files

### `boston_block_groups.geojson`
- **`geoid`**: 12-digit Census block group GEOID
- **`geometry`**: `POLYGON` boundary of each block group

### `boston_grid.geojson`
- **`cell`**: Unique grid cell identifier (e.g., `"Cell 108"`)
- **`geometry`**: `POLYGON Z` geometry for regular spatial grid

### `boston_social_infra.geojson`
- **`Name`**: Name of the infrastructure location
- **`id`**: Unique integer identifier
- **`group`**: Type/category (e.g., `"Parks"`, `"Community Spaces"`, `"Places of Worship"`, `"Social Businesses"`)
- **`geometry`**: `POINT Z` representing facility location

### `boston_train_lines.geojson`
- **`line`**: MBTA line name (e.g., `"RED"`, `"GREEN"`, `"BLUE"`, `"ORANGE"`)
- **`geometry`**: `LINESTRING` showing train line path

---

## 📊 Tabular Data

### `boston_census_data.csv`

Sociodemographic attributes for each spatial grid cell. Can be joined to `boston_grid.geojson` using the `cell` field.

| Variable | Description |
|----------|-------------|
| `cell`                        | Grid cell ID (e.g., `"Cell 108"`) |
| `neighborhood`               | Boston neighborhood name |
| `pop_density`               | People per square kilometer |
| `pop_women`                 | % female residents |
| `pop_white`, `pop_black`, `pop_natam`, `pop_asian`, `pop_pacific`, `pop_hisplat` | % of residents by racial/ethnic group |
| `pop_some_college`          | % of residents with some college education |
| `median_income`             | Median household income (USD) |
| `income_inequality`         | Gini coefficient (0 = perfect equality; 1 = complete inequality) |
| `pop_unemployed`            | % of labor force unemployed |
| `median_monthly_housing_cost` | Median cost of housing per month (USD) |
| `pop_age_65_plus`           | % of population aged 65 or older |

---

## 🧠 Example Uses

- Visualize access to community infrastructure by neighborhood
- Analyze demographic variation across Boston’s urban grid
- Overlay transit infrastructure with sociodemographic indicators
- Identify under-resourced areas based on spatial clustering

---

## 📌 Join Keys

| Dataset | Join Key |
|---------|----------|
| `boston_census_data.csv` + `boston_grid.geojson` | `cell` |
| `boston_social_infra.geojson` + `boston_grid.geojson` | Spatial join |
| `boston_block_groups.geojson` + external Census data | `geoid` |

---
