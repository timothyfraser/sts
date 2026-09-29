# make_seed.R
# Builds the seed CSVs for the STS class database (exemplars/supabase/seed/*.csv)
# from the course datasets in data/. Every CSV stays under 2 MB. Spatial tables
# get a `wkt` text column (sf::st_as_text, EPSG:4326) that sql/05_seed.sql COPYs
# into a staging column and sql/06_geometry.sql converts with ST_GeomFromText().
#
# Used by: every chapter that queries the class database (1, 3, 5, 6, 7, 9, 10,
# 11, 13). Run from exemplars/supabase/:   Rscript seed/make_seed.R
# Sampling uses set.seed(5460), so re-running produces identical seeds.

library(dplyr)
library(readr)
library(sf)
library(DBI)
library(RSQLite)

data_dir = "../../data"
out_dir = "seed"
max_bytes = 2 * 1024^2

# Write a seed CSV: NA as an empty field (COPY's CSV NULL), and refuse to
# write anything over the 2 MB cap so a bad sample fails loudly here.
write_seed = function(df, name) {
  path = file.path(out_dir, paste0(name, ".csv"))
  write_csv(df, path, na = "")
  size = file.size(path)
  if (size > max_bytes) stop(sprintf("%s is %.2f MB, over the 2 MB cap", path, size / 1024^2))
  cat(sprintf("%-32s %7d rows  %6.0f KB\n", basename(path), nrow(df), size / 1024))
  invisible(path)
}

# Geometry to WKT in EPSG:4326, Z/M dropped, rounded to ~1 m.
to_wkt = function(geom) {
  geom %>% st_zm() %>% st_transform(4326) %>% st_as_text(digits = 7)
}

# ---- Chapters 1, 3, 5: nycflights ------------------------------------------
airlines = read_csv(file.path(data_dir, "airlines.csv"), show_col_types = FALSE)
airports = read_csv(file.path(data_dir, "airports.csv"), show_col_types = FALSE)
planes = read_csv(file.path(data_dir, "planes.csv"), show_col_types = FALSE)
flights = read_csv(file.path(data_dir, "flights.csv"), show_col_types = FALSE,
                   col_types = cols(time_hour = col_character()))

set.seed(5460)
flights_sample = flights %>%
  slice_sample(n = 18000) %>%
  arrange(year, month, day, sched_dep_time, carrier, flight)

write_seed(airlines, "airlines")
write_seed(airports, "airports")
write_seed(planes, "planes")
write_seed(flights_sample, "flights")

# ---- Chapter 2: gapminder --------------------------------------------------
gapminder = read_csv(file.path(data_dir, "gapminder.csv"), show_col_types = FALSE)
write_seed(gapminder, "gapminder")

# ---- Chapter 3: Japan solar ------------------------------------------------
jp_solar_farms = read_csv(file.path(data_dir, "jp_solar_farms_2018.csv"), show_col_types = FALSE,
                          col_types = cols(muni_code = col_character(), pref_code = col_character()))
jp_solar = read_csv(file.path(data_dir, "jp_solar.csv"), show_col_types = FALSE,
                    col_types = cols(muni_code = col_character()))
write_seed(jp_solar_farms, "jp_solar_farms_2018")
write_seed(jp_solar, "jp_solar")

# ---- Chapter 7: Japan municipalities (simplified until under 2 MB) ----------
sf_use_s2(FALSE)
munis_raw = read_sf(file.path(data_dir, "japan", "municipalities.geojson")) %>%
  st_make_valid()
for (tol in c(0.002, 0.004, 0.008, 0.016)) {
  munis = munis_raw %>%
    st_simplify(dTolerance = tol, preserveTopology = TRUE) %>%
    st_make_valid() %>%
    st_collection_extract("POLYGON") %>%
    group_by(across(-geometry)) %>%
    summarise(geometry = st_union(geometry), .groups = "drop") %>%
    st_cast("MULTIPOLYGON") %>%
    mutate(wkt = to_wkt(geometry)) %>%
    st_drop_geometry() %>%
    select(muni_code, muni, muni_type, muni_type_jp, muni_jp, pref, pref_code, region, wkt)
  tmp = tempfile(fileext = ".csv")
  write_csv(munis, tmp, na = "")
  if (file.size(tmp) <= max_bytes) break
}
cat(sprintf("japan_municipalities simplified at dTolerance = %s degrees\n", tol))
write_seed(munis, "japan_municipalities")

# ---- Chapters 6, 7, 8: Boston social infrastructure -------------------------
bsi = file.path(data_dir, "boston_social_infra")
sites = read_sf(file.path(bsi, "boston_social_infra.geojson")) %>%
  mutate(wkt = to_wkt(geometry)) %>%
  st_drop_geometry() %>%
  select(id, name = Name, site_group = group, wkt)
write_seed(sites, "bsi_sites")

# The source has several polygons for one geoid (250259801011); union them so
# geoid can be the primary key.
block_groups = read_sf(file.path(bsi, "boston_block_groups.geojson")) %>%
  st_make_valid() %>%
  group_by(geoid) %>%
  summarise(geometry = st_union(geometry), .groups = "drop") %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(wkt = to_wkt(geometry)) %>%
  st_drop_geometry() %>%
  select(geoid, wkt)
write_seed(block_groups, "bsi_block_groups")

grid = read_sf(file.path(bsi, "boston_grid.geojson")) %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(wkt = to_wkt(geometry)) %>%
  st_drop_geometry() %>%
  select(cell, wkt)
write_seed(grid, "bsi_grid")

census = read_csv(file.path(bsi, "boston_census_data.csv"), show_col_types = FALSE)
write_seed(census, "bsi_census")

# ---- Chapters 6, 7: Boston voting -------------------------------------------
bv = file.path(data_dir, "boston_voting")
votes = read_csv(file.path(bv, "boston_votes.csv"), show_col_types = FALSE,
                 col_types = cols(.default = col_character(),
                                  voted_biden = col_double(), voted_trump = col_double(),
                                  voter_turnout = col_double()))
precincts = read_sf(file.path(bv, "precincts.geojson")) %>%
  st_make_valid() %>%
  st_cast("MULTIPOLYGON") %>%
  mutate(wkt = to_wkt(geometry)) %>%
  st_drop_geometry() %>%
  select(ward_precinct, wkt) %>%
  left_join(by = "ward_precinct", y = votes %>% select(ward_precinct, ward, precinct, voted_biden, voted_trump, voter_turnout))
polling = read_sf(file.path(bv, "polling_places.geojson")) %>%
  mutate(wkt = to_wkt(geometry)) %>%
  st_drop_geometry() %>%
  select(id, ward_precinct, wkt)
write_seed(precincts, "precincts")
write_seed(polling, "polling_places")

# ---- Chapters 9, 10, 11: Bluebikes ------------------------------------------
# Station-pair trips are rush-hour rides (tally_rush_edges) aggregated to
# month for 2019, keeping pairs with >= min_trips rides that year so the
# seed stays under 2 MB. The full table is 5.2M rows; this is the teaching cut.
bb_dir = tempfile("bluebikes")
unzip(file.path(data_dir, "bluebikes", "bluebikes.zip"), exdir = bb_dir)
con = dbConnect(RSQLite::SQLite(), file.path(bb_dir, "our_data", "bluebikes.sqlite"))

stations = tbl(con, "stationbg_dataset") %>%
  select(code, geoid, x, y, pop_density_2020_smooth5, pop_some_college_smooth5,
         pop_white_2020_smooth5, pop_over_65_2019_smooth5) %>%
  collect() %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  mutate(wkt = to_wkt(geometry)) %>%
  st_drop_geometry()

pairs_2019 = tbl(con, "tally_rush_edges") %>%
  filter(day >= "2019-01-01", day <= "2019-12-31") %>%
  mutate(month = paste0(substr(day, 1, 7), "-01")) %>%
  group_by(start_code, end_code, month) %>%
  summarise(trips = sum(count, na.rm = TRUE), .groups = "drop") %>%
  collect()
dbDisconnect(con)

for (min_trips in c(100, 150, 200, 300, 400, 600)) {
  keep = pairs_2019 %>%
    group_by(start_code, end_code) %>%
    filter(sum(trips) >= min_trips) %>%
    ungroup() %>%
    filter(start_code %in% stations$code, end_code %in% stations$code) %>%
    arrange(start_code, end_code, month)
  tmp = tempfile(fileext = ".csv")
  write_csv(keep, tmp, na = "")
  if (file.size(tmp) <= max_bytes) break
}
cat(sprintf("bluebikes_trips keeps pairs with >= %d rush-hour trips in 2019\n", min_trips))
write_seed(stations, "bluebikes_stations")
write_seed(keep %>% select(start_code, end_code, month, trips), "bluebikes_trips")

# ---- Chapters 9, 11: Hurricane Dorian evacuation ----------------------------
nodes = read_rds(file.path(data_dir, "evacuation", "nodes.rds")) %>%
  select(node, geoid, county, pop, social_capital, bonding, bridging, linking,
         svi, median_income, pop_age_65_plus, evacuation_more, evacuation_less)
set.seed(5460)
edges = read_rds(file.path(data_dir, "evacuation", "edges.rds")) %>%
  filter(from %in% nodes$node, to %in% nodes$node) %>%
  slice_sample(n = 15000) %>%
  mutate(date_time = format(date_time, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")) %>%
  select(from_node = from, to_node = to, date_time, evacuation, km, from_geoid, to_geoid) %>%
  arrange(date_time, from_node, to_node)
write_seed(nodes, "evacuation_nodes")
write_seed(edges, "evacuation_edges")

# ---- Chapter 10: committees (bipartite) -------------------------------------
cm = file.path(data_dir, "committees")
committees = read_csv(file.path(cm, "committees.csv"), show_col_types = FALSE) %>%
  select(committee_id = name, committee_type, geography, level, town,
         committee_romaji, committee_japanese)
members = read_csv(file.path(cm, "members.csv"), show_col_types = FALSE) %>%
  select(member_id = name, role, age_group, gender, politician, govt, research,
         business, fisheries, media)
memberships = read_csv(file.path(cm, "edgelist.csv"), show_col_types = FALSE) %>%
  distinct(committee_id, member_id, .keep_all = TRUE)
write_seed(committees, "committees")
write_seed(members, "committee_members")
write_seed(memberships, "committee_memberships")

# ---- Chapters 7, 13: air quality (one week, PM2.5 in UG/M3) -----------------
aq_sites = read_rds(file.path(data_dir, "air_quality", "sites.rds")) %>%
  mutate(wkt = to_wkt(geometry)) %>%
  st_drop_geometry() %>%
  mutate(aqs_id_full = format(aqs_id_full, scientific = FALSE, trim = TRUE)) %>%
  select(aqs_id_full, aqs_id, site_name, nyc_id, dist_km = dist, wkt) %>%
  distinct(aqs_id_full, .keep_all = TRUE)

readings = read_csv(file.path(data_dir, "air_quality", "air_quality.csv"), show_col_types = FALSE,
                    col_types = cols(aqs_id_full = col_character(), datetime = col_character())) %>%
  filter(datetime >= "2025-05-01", datetime < "2025-05-08", unit == "UG/M3",
         aqs_id_full %in% aq_sites$aqs_id_full) %>%
  distinct(aqs_id_full, datetime, pollutant, unit, .keep_all = TRUE) %>%
  select(aqs_id_full, datetime, pollutant, value, unit) %>%
  arrange(datetime, aqs_id_full)
write_seed(aq_sites, "air_quality_sites")
write_seed(readings, "air_quality_hourly")

unlink(bb_dir, recursive = TRUE)
cat("done\n")
