<!-- seed/README.md: what the seed CSVs are and how to rebuild them. Used by
     sql/05_seed.sql (chapters 1-3, 5-11, 13 query what these load). -->

# Seed CSVs for the STS class database

Every `*.csv` here is built by `make_seed.R` from the course datasets in
`data/`, is at most 2 MB, and is loaded by `sql/05_seed.sql` with psql's
`\copy`. An empty unquoted field is NULL. Spatial seeds have a `wkt` column
(EPSG:4326, Z dropped, 7 decimal places) that `sql/06_geometry.sql` converts
with `ST_GeomFromText(wkt, 4326)`.

Rebuild (from `exemplars/supabase/`; needs dplyr, readr, sf, DBI, RSQLite):

```sh
Rscript seed/make_seed.R
```

Sampling uses `set.seed(5460)`, so a rebuild is byte-identical unless the
source data changed. The script stops if any CSV would exceed 2 MB.

| Seed | Rows | Source | Cut |
|---|---:|---|---|
| airlines, airports, planes | 16 / 1,458 / 3,322 | `data/*.csv` | full |
| flights | 18,000 | `data/flights.csv` | random sample |
| gapminder | 1,704 | `data/gapminder.csv` | full |
| jp_solar_farms_2018, jp_solar | 1,741 / 6,321 | `data/jp_solar*.csv` | full |
| japan_municipalities | 1,750 | `data/japan/municipalities.geojson` | simplified (`st_simplify`, 0.004 degrees), made valid |
| bsi_sites, bsi_grid, bsi_census | 1,049 / 73 / 73 | `data/boston_social_infra/` | full |
| bsi_block_groups | 680 | `boston_block_groups.geojson` | full; the 8 polygons sharing geoid 250259801011 are unioned |
| precincts, polling_places | 255 / 255 | `data/boston_voting/*.geojson` + `boston_votes.csv` | full |
| bluebikes_stations | 425 | `bluebikes.zip` -> `stationbg_dataset` (x, y) | 4 covariates kept |
| bluebikes_trips | 66,604 | `bluebikes.zip` -> `tally_rush_edges` | rush-hour trips, 2019, by station pair and month, pairs with >= 100 trips that year |
| evacuation_nodes | 427 | `data/evacuation/nodes.rds` | 13 of 56 columns |
| evacuation_edges | 15,000 | `data/evacuation/edges.rds` | random sample of 408,853 |
| committees, committee_members, committee_memberships | 39 / 656 / 749 | `data/committees/` | full rows, subset of member columns |
| air_quality_sites, air_quality_hourly | 56 / 8,395 | `data/air_quality/` | 2025-05-01 to 2025-05-07 UTC, PM2.5 in UG/M3 |

No student data is in any seed. Committee members are already anonymised in
the source (`name_1`, ...).
