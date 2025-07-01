`# 📘 Hurricane Dorian Evacuation Dataset Codebook

**Source**: Fraser (2022), *Sustainability Science*  
**Description**: This dataset captures spatial and temporal evacuation patterns inferred from Facebook mobility data during Hurricane Dorian. It consists of three primary RDS files: `nodes.rds`, `edges.rds`, and `roads.rds`.

---

## 📍 `nodes.rds`: County Subdivision Characteristics

Each row represents a city or county subdivision node.

| Variable                         | Description |
|----------------------------------|-------------|
| `node`                           | Internal node ID (used to match with `from` and `to` in edges) |
| `geoid`                          | Census geography code for the subdivision |
| `social_capital`                | Composite index of social capital |
| `bonding`, `bridging`, `linking`| Dimensions of social capital |
| `svi`, `svi_*`                   | Social Vulnerability Index and subcomponents (socioeconomic, housing, etc.) |
| `pop`                            | Total population |
| `pop_women`, `pop_black`, `pop_white`, `pop_hisplat`, etc. | Demographic composition (percentages) |
| `median_income`                  | Median household income |
| `income_inequality`              | Gini coefficient of income inequality |
| `pop_labor_force`               | Civilian labor force count |
| `pop_unemployed`                | Percent of population unemployed |
| `median_monthly_housing_cost`   | Median monthly housing costs |
| `pop_age_65_plus`               | Percent of population age 65+ |
| `county`                         | 5-digit county FIPS code |
| `democrat_XXXX`, `republican_XXXX` | Presidential election vote shares by year (2000–2016) |
| `dem_percent_precinct`          | Democratic vote share in local precinct |
| `projects`, `properties`, `amount_paid`, `programs` | FEMA program statistics |
| `cert`, `trainings_per_year`, `classes`, `trainees` | Community emergency response training metrics |
| `rainfall_14days`               | Rainfall in the 14-day window around the storm |
| `evacuation_*`                  | Aggregated intra/inter-community evacuation metrics |

---

## 🔁 `edges.rds`: Movement Between City Pairs

Each row represents observed movement between a pair of city subdivisions at a given time.

| Variable       | Description |
|----------------|-------------|
| `from`         | Source node ID |
| `to`           | Destination node ID |
| `date_time`    | Timestamp (datetime format) of movement observation |
| `evacuation`   | Difference from baseline movement (positive = more movement than usual; negative = less); unit is 1 Facebook user. |
| `km`           | Distance between `from` and `to` subdivisions (in kilometers) |
| `from_geoid`   | Census geoid of source location |
| `to_geoid`     | Census geoid of destination location |

**Notes**:
- One row = one directed observation of change in usual movement from one city to another at a point in time.
- Used for temporal modeling (e.g., `evacuation ~ km + hour`).
- Be sure to filter this dataset! It probably doesn't make sense to compare increases in movement (evacuation) with decreases in movement (sheltering in place) simultaneously.

---

## 🛣️ `roads.rds`: Road Geometry and Metadata

Each row represents a segment of road.

| Variable         | Description |
|------------------|-------------|
| `linearid`       | Unique line segment ID |
| `fullname`       | Full name of the road |
| `rttyp`          | Route type (e.g., 'U' for US Highway, 'S' for State) |
| `mtfcc`          | Census feature classification code |
| `geoid`          | Census geoid linked to the road segment |
| `geoid_county`   | County FIPS code |
| `state`          | Two-letter state abbreviation |
| `geometry`       | Road geometry in LINESTRING (sf spatial format) |

---


## 🗺️ County Geometries (`counties.geojson`)

This spatial dataset contains U.S. county boundaries and land/water area measurements for 293 counties relevant to the Hurricane Dorian evacuation study.

- **File type**: GeoJSON (spatial)
- **Rows**: 293 counties
- **Geometry type**: `MULTIPOLYGON` (one or more polygons per county)
- **Projection**: Geographic coordinates (degrees longitude and latitude)

| Variable     | Description |
|--------------|-------------|
| `geoid`      | 5-digit FIPS code for each county (character) |
| `state`      | Two-letter state abbreviation (e.g. `FL`, `GA`) |
| `name`       | Full county name (e.g. `Geneva County`) |
| `area_land`  | Land area in square meters (numeric) |
| `area_water` | Water area in square meters (numeric) |
| `geometry`   | Spatial geometry (`MULTIPOLYGON`) representing county boundaries |

### 📝 Example Entry

| geoid | state | name           | area_land   | area_water | geometry     |
|-------|--------|----------------|-------------|-------------|---------------|
|01061 | AL     | Geneva County  | 1,487,908,432 | 11,567,409 | MULTIPOLYGON(...) |

---

## 💡 Usage Tips

- Filter `edges.rds` by hour (`lubridate::hour(date_time)`) for evacuation patterns.
- Join `edges.rds` to `nodes.rds` using `from` and `to` for origin-destination demographic context.
- Use `roads.rds` for spatial visualizations with `ggplot2::geom_sf()`.
- Merge with `nodes.rds` or `edges.rds` by county `geoid`
- Filter by state or region for focused spatial analysis

---
