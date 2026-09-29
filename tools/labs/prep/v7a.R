# v7a.R - prep script for lab v7a (gridding and interpolation over time).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v7a.R
# It writes:
#   docs-v3/labs/data/v7a_sites.csv      sensor sites in the core NYC metro, projected (UTM 18N, metres)
#   docs-v3/labs/data/v7a_readings.csv   hourly PM2.5 (ug/m3) per site for one week, complete sites only
#   tests/labs/golden/v7a.json           named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
# Built from tools/labs/prep/_template.R; the SQL twin is PostGIS (ST_SquareGrid), which has no
# local engine, so it is shown in the panel but not executed here (see block 5).

library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(jsonlite)
library(sf)
library(gstat)

# ---- 0. REPO ROOT [KEEP] -------------------------------------------------------------------
find_root = function(dir = getwd()) {
  dir = normalizePath(dir, mustWork = TRUE)
  while (!(dir.exists(file.path(dir, "data")) && dir.exists(file.path(dir, "docs-v3")))) {
    parent = dirname(dir)
    if (parent == dir) stop("prep: could not find the repo root (a folder with data/ and docs-v3/)")
    dir = parent
  }
  dir
}
root = find_root()

# ---- 1. CONFIG [CHANGE] --------------------------------------------------------------------
lab_id     = "v7a"
page       = "docs-v3/labs/v7a-grid-and-interpolate.html"
seed       = 5460                                   # no sampling here, kept for the house rule
week_start = "2025-05-05"                           # Monday; 168 hourly readings follow
week_end   = "2025-05-11"
bbox_ll    = c(xmin = -74.30, ymin = 40.50, xmax = -73.70, ymax = 40.95)   # core NYC metro
sites_out    = "docs-v3/labs/data/v7a_sites.csv"
readings_out = "docs-v3/labs/data/v7a_readings.csv"
golden_out   = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE ----------------------------------------------------------------------
set.seed(seed)
sites_raw = readRDS(file.path(root, "data/air_quality/sites.rds")) %>%
  st_as_sf() %>%
  st_set_agr("constant") %>%
  st_crop(st_bbox(bbox_ll, crs = st_crs(4326))) %>%
  # one sensor per location: co-located sensors would make leave-one-out trivially exact
  mutate(lon = round(st_coordinates(geometry)[, 1], 4), lat = round(st_coordinates(geometry)[, 2], 4)) %>%
  arrange(aqs_id_full) %>%
  distinct(lon, lat, .keep_all = TRUE)

readings_raw = read_csv(file.path(root, "data/air_quality/air_quality.csv"),
                        col_types = cols(aqs_id_full = "d", datetime = "c", value = "d", unit = "c", .default = "c"), col_select = c(aqs_id_full, datetime, value, unit)) %>%
  filter(unit == "UG/M3",
         substr(datetime, 1, 10) >= week_start, substr(datetime, 1, 10) <= week_end,
         aqs_id_full %in% sites_raw$aqs_id_full, !is.na(value))

complete_ids = readings_raw %>% count(aqs_id_full) %>% filter(n == 168) %>% pull(aqs_id_full)

sites_df = sites_raw %>%
  filter(aqs_id_full %in% complete_ids) %>%
  st_transform(crs = 32618) %>%
  mutate(x = round(st_coordinates(geometry)[, 1]), y = round(st_coordinates(geometry)[, 2])) %>%
  as_tibble() %>%
  arrange(aqs_id_full) %>%
  mutate(site_id = sprintf("s%02d", row_number()), name = coalesce(na_if(site_name, ""), paste("AQS", aqs_id))) %>%
  select(site_id, name, x, y, aqs_id_full)

hours = sort(unique(readings_raw$datetime))
stopifnot(length(hours) == 168)

readings_df = readings_raw %>%
  inner_join(by = "aqs_id_full", y = sites_df %>% select(aqs_id_full, site_id)) %>%
  mutate(hour = match(datetime, hours) - 1) %>%
  arrange(hour, site_id) %>%
  select(hour, datetime, site_id, value)

dir.create(dirname(file.path(root, sites_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(sites_df %>% select(site_id, name, x, y), file.path(root, sites_out), na = "")
write_csv(readings_df, file.path(root, readings_out), na = "")
stopifnot(file.size(file.path(root, sites_out)) + file.size(file.path(root, readings_out)) <= 500 * 1024)

# Re-read exactly the bytes the browser fetches.
sites = read_csv(file.path(root, sites_out), show_col_types = FALSE) %>%
  st_as_sf(coords = c("x", "y"), crs = 32618, remove = FALSE)
readings = read_csv(file.path(root, readings_out), show_col_types = FALSE, col_types = "dccd") %>%
  mutate(datetime = as.character(datetime))

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline = list(cell_km = 5,  agg = "mean",  method = "idw",     power = 2, hour = 0,  loo = TRUE),
  nearest  = list(cell_km = 5,  agg = "mean",  method = "nearest", power = 2, hour = 0,  loo = TRUE),
  idw6     = list(cell_km = 5,  agg = "mean",  method = "idw",     power = 6, hour = 0,  loo = TRUE),
  fine     = list(cell_km = 2,  agg = "mean",  method = "idw",     power = 2, hour = 0,  loo = TRUE),
  coarse   = list(cell_km = 10, agg = "max",   method = "none",    power = 2, hour = 0,  loo = FALSE),
  count    = list(cell_km = 10, agg = "count", method = "none",    power = 2, hour = 0,  loo = FALSE),
  evening  = list(cell_km = 5,  agg = "mean",  method = "idw",     power = 2, hour = 18, loo = TRUE)
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
agg_expr = function(agg) {
  switch(agg,
    mean  = "value = mean(value, na.rm = TRUE)",
    max   = "value = max(value, na.rm = TRUE)",
    count = "value = sum(!is.na(value))")
}
code_r = function(s) {
  cs = s$cell_km * 1000
  interp = if (s$method == "none") "" else paste0(
    "\n\n# Fill every cell from the site points: gstat::idw()",
    if (s$method == "nearest") " with nmax = 1 is nearest neighbour" else paste0(" with power idp = ", s$power),
    "\nsurface = gstat::idw(value ~ 1, locations = points,\n",
    "  newdata = st_centroid(grid), idp = ", s$power,
    if (s$method == "nearest") ", nmax = 1" else "", ", debug.level = 0)"
  )
  loo = if (s$method == "none" || !isTRUE(s$loo)) "" else paste0(
    "\n\n# Leave-one-out: predict each site from all the others\n",
    "loo = map_dfr(1:nrow(points), function(i) {\n",
    "  fit = gstat::idw(value ~ 1, locations = points[-i, ], newdata = points[i, ],\n",
    "    idp = ", s$power, if (s$method == "nearest") ", nmax = 1" else "", ", debug.level = 0)\n",
    "  tibble(site_id = points$site_id[i], obs = points$value[i], pred = fit$var1.pred)\n",
    "})\n",
    "loo %>% summarize(rmse = sqrt(mean((pred - obs)^2)), mae = mean(abs(pred - obs)))"
  )
  paste0(
    "# one hour of PM2.5 at the sites (hour ", s$hour, " = ", readings$datetime[readings$hour == s$hour][1], ")\n",
    "points = sites %>%\n",
    "  inner_join(by = \"site_id\", y = readings %>% filter(hour == ", s$hour, "))\n\n",
    "grid = sites %>%\n",
    "  st_make_grid(cellsize = c(", cs, ", ", cs, "), square = TRUE) %>%\n",
    "  st_as_sf() %>% rename(geometry = x) %>%\n",
    "  mutate(cell = 1:n())\n\n",
    "cells = grid %>%\n",
    "  st_join(points %>% select(site_id, value)) %>%\n",
    "  as_tibble() %>%\n",
    "  group_by(cell) %>%\n",
    "  summarize(", agg_expr(s$agg), ", n_sites = sum(!is.na(site_id)))",
    interp, loo
  )
}
code_sql = function(s) {
  cs = s$cell_km * 1000
  agg = switch(s$agg, mean = "AVG(r.value)", max = "MAX(r.value)", count = "COUNT(r.value)")
  paste0(
    "-- PostGIS: square grid over the site extent (metres, UTM 18N)\n",
    "SELECT g.i, g.j, ", agg, " AS value, COUNT(s.site_id) AS n_sites\n",
    "FROM ST_SquareGrid(", cs, ", (SELECT ST_SetSRID(ST_Extent(geom), 32618) FROM sites)) AS g\n",
    "LEFT JOIN sites s ON ST_Intersects(g.geom, s.geom)\n",
    "LEFT JOIN readings r ON r.site_id = s.site_id AND r.hour = ", s$hour, "\n",
    "GROUP BY g.i, g.j;"
  )
}

# ---- 5. RUN + CHECK EACH STATE -------------------------------------------------------------
# The panel's R runs for real; its objects give the readouts. The PostGIS SQL is displayed only.
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$sites = sites
  env$readings = readings
  # Known, harmless warnings silenced: st_centroid() notes attributes are constant per cell, and
  # max() over a cell with no site returns -Inf (those cells are dropped as empty below).
  suppressWarnings(eval(parse(text = code_r(s)), envir = env))
  cells = env$cells
  occupied = cells %>% filter(n_sites > 0)
  r = list(
    n_cells     = nrow(cells),
    empty_cells = sum(cells$n_sites == 0),
    n_sites     = nrow(env$points),
    mean_value  = mean(occupied$value)
  )
  if (s$method != "none") r$surface_mean = mean(env$surface$var1.pred)
  if (s$method != "none" && isTRUE(s$loo)) {
    loo = env$loo
    worst = loo %>% mutate(err = abs(pred - obs)) %>% arrange(desc(err), site_id) %>% slice(1)
    r$loo_rmse   = sqrt(mean((loo$pred - loo$obs)^2))
    r$loo_mae    = mean(abs(loo$pred - loo$obs))
    r$worst_site = worst$site_id
    r$worst_obs  = worst$obs
    r$worst_pred = worst$pred
  }
  list(name = name, state = s, readouts = r, code = list(r = code_r(s), sql = code_sql(s)))
})

# ---- 6. WRITE GOLDEN [KEEP] ----------------------------------------------------------------
golden = list(
  lab          = lab_id,
  page         = page,
  generated_by = paste0("tools/labs/prep/", lab_id, ".R"),
  tolerance    = 1e-6,
  states       = unname(golden_states),
  ack          = ack
)
dir.create(dirname(file.path(root, golden_out)), recursive = TRUE, showWarnings = FALSE)
write_json(golden, file.path(root, golden_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
message("prep: wrote ", sites_out, " + ", readings_out, " (",
        file.size(file.path(root, sites_out)) + file.size(file.path(root, readings_out)), " B) and ", golden_out)
