# v6b.R - prep script for the coordinate reference system lab (docs-v3/labs/v6b-crs-lab.html).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v6b.R
# It writes:
#   docs-v3/labs/data/v6b.json      polling places in three CRSs + simplified precinct outlines
#   tests/labs/golden/v6b.json      named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
# Built from tools/labs/prep/_template.R; the [KEEP] blocks are unchanged in spirit.

library(dplyr)
library(purrr)
library(jsonlite)
library(sf)

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
lab_id     = "v6b"
page       = "docs-v3/labs/v6b-crs-lab.html"
seed       = 5460
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack        = list()
crs_list   = c(4326, 3857, 26986)
pair       = c(1, 120)   # the two marked polling places (ids), fixed on first publish

# ---- 2. LOAD + REDUCE [CHANGE] -------------------------------------------------------------
set.seed(seed)
polling_places = read_sf(file.path(root, "data/boston_voting/polling_places.geojson"))
precincts      = read_sf(file.path(root, "data/boston_voting/precincts.geojson"))
stopifnot(st_crs(polling_places)$epsg == 4326, st_crs(precincts)$epsg == 4326)

# Outlines only for drawing: simplify in metres (state plane), keep lon/lat at 5 decimals.
outline = precincts %>%
  st_transform(crs = 26986) %>%
  st_simplify(dTolerance = 40, preserveTopology = TRUE) %>%
  st_transform(crs = 4326)
rings = map(st_geometry(outline), function(g) {
  map(st_cast(st_geometry(st_sfc(g, crs = 4326)), "POLYGON"), function(p) {
    xy = st_coordinates(p)[, 1:2]
    unname(split(round(t(xy), 5), rep(seq_len(nrow(xy)), each = 2)))
  })
})

pts_xy = map(crs_list, function(code) {
  xy = polling_places %>% st_transform(crs = code) %>% st_coordinates()
  xy   # full precision: the page recomputes distances from these exact numbers
})
names(pts_xy) = paste0("epsg", crs_list)
page_data = list(
  n_places  = nrow(polling_places),
  pair      = pair,
  places    = map(seq_len(nrow(polling_places)), function(i) {
    c(list(id = polling_places$id[i], wp = polling_places$ward_precinct[i]),
      map(pts_xy, function(xy) unname(xy[i, ])))
  }),
  precincts = rings
)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(page_data, file.path(root, data_out), auto_unbox = TRUE, digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline  = list(crs = 26986, buffer = "metres"),
  wgs84     = list(crs = 4326,  buffer = "metres"),
  mercator  = list(crs = 3857,  buffer = "metres"),
  degrees   = list(crs = 4326,  buffer = "degrees")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
code_r = function(s) {
  buf = if (s$buffer == "degrees") "st_buffer(dist = 0.01)   # planar: 0.01 of a DEGREE" else
    if (s$crs == 4326) "st_buffer(dist = 1000)   # s2: metres on the sphere" else
      "st_buffer(dist = 1000)   # 1 km in the CRS unit (metres)"
  paste0(
    if (s$buffer == "degrees") "sf_use_s2(FALSE)   # treat lon/lat as flat x/y\n\n" else "",
    "pts = polling_places %>%\n",
    "  filter(id %in% c(", pair[1], ", ", pair[2], ")) %>%\n",
    "  st_transform(crs = ", s$crs, ")\n",
    "\n",
    "st_crs(pts)$units_gdal\n",
    "d = st_distance(pts)[1, 2]\n",
    "\n",
    "zone = pts %>%\n",
    "  ", buf
  )
}
code_sql = function(s) {
  paste0(
    "SELECT ST_Distance(ST_Transform(a.geom, ", s$crs, "),\n",
    "                   ST_Transform(b.geom, ", s$crs, ")) AS d_crs,\n",
    "       ST_Distance(a.geom::geography, b.geom::geography) AS d_geography\n",
    "FROM polling_places a, polling_places b\n",
    "WHERE a.id = ", pair[1], " AND b.id = ", pair[2], ";"
  )
}

# Truth = state plane (EPSG:26986) distance in metres.
truth = polling_places %>% filter(id %in% pair) %>% st_transform(crs = 26986) %>% st_distance()
truth = as.numeric(truth[1, 2])
readouts_from = function(s, env) {
  d = round(as.numeric(env$d), 2)   # centimetres: what the sidebar shows
  t = round(truth, 2)
  # Buffer reach in TRUE metres (east-west vs north-south) from the first marked place:
  # 0.01 degree of latitude is R * 0.01 * pi / 180; of longitude, that times cos(latitude).
  reach = if (s$buffer == "degrees") 0.01 * pi / 180 * 6371010 else 1000
  lat = pts_xy$epsg4326[polling_places$id == pair[1], 2]
  list(crs = paste0("EPSG:", s$crs), dist_m = d, truth_m = t,
       pct_err = round(100 * (d - t) / t, 3),
       ew_m = round(if (s$buffer == "degrees") reach * cos(lat * pi / 180) else reach, 1),
       ns_m = round(reach, 1))
}

# ---- 5. RUN EACH STATE [KEEP] --------------------------------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$polling_places = polling_places
  suppressWarnings(suppressMessages(eval(parse(text = code_r(s)), envir = env)))
  sf_use_s2(TRUE)
  list(name = name, state = s, readouts = readouts_from(s, env),
       code = list(r = code_r(s), sql = code_sql(s)))
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
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)
