# v8a.R - prep for lab v8a "Where should each step run? A spatial pipeline".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v8a.R
# It writes:
#   docs-v3/labs/data/v8a.json      stage outputs (row counts + bytes) per neighbourhood, cost model
#   tests/labs/golden/v8a.json      named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# The pipeline: points + grid cells -> JOIN (st_join) -> FILTER (neighbourhood) -> AGGREGATE (count)
# -> OUTPUT. Each of join / filter / aggregate is placed in the database (PostGIS), the API
# (Plumber/R) or the browser. Real stage outputs are computed here with sf on the course data;
# latency is a stated model (constants in `model` below), not a measurement.

library(dplyr)
library(readr)
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
lab_id     = "v8a"
page       = "docs-v3/labs/v8a-where-to-compute.html"
src_dir    = "data/boston_social_infra"
seed       = 5460
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# The cost model (stated on the page). ms of compute per stage and tier; network speeds.
model = list(
  compute = list(
    join      = list(db = 40, api = 220, browser = 900, view = 1),
    filter    = list(db = 2,  api = 6,   browser = 4),
    aggregate = list(db = 3,  api = 8,   browser = 6)
  ),
  rtt_ms            = 80,       # one browser <-> server round trip
  browser_bytes_ms  = 1250,     # bytes per ms into a phone (about 10 Mbit/s)
  internal_bytes_ms = 125000,   # bytes per ms from database to API (about 1 Gbit/s)
  internal_ms       = 2         # one database <-> API hop
)

# ---- 2. LOAD + STAGE OUTPUTS [CHANGE] -----------------------------------------------------
set.seed(seed)
path = function(f) file.path(root, src_dir, f)
points = read_sf(path("boston_social_infra.geojson")) %>% select(id, group)
census = read_csv(path("boston_census_data.csv"), show_col_types = FALSE)
cells = read_sf(path("boston_grid.geojson")) %>%
  left_join(census %>% select(cell, neighborhood), by = "cell")

# The bytes a stage hands on = the JSON a Plumber endpoint would serialise for that table.
json_bytes = function(df) nchar(as.character(toJSON(df, dataframe = "rows")), type = "bytes")

sf_use_s2(FALSE)
joined = st_join(points, cells, join = st_within) %>%
  st_drop_geometry() %>%
  filter(!is.na(cell))

raw_bytes = sum(file.size(path(c("boston_social_infra.geojson", "boston_grid.geojson", "boston_census_data.csv"))))
hoods = sort(unique(census$neighborhood))
by_hood = map(set_names(hoods), function(h) {
  kept = joined %>% filter(neighborhood == h)
  counts = kept %>% count(cell, group)
  list(sites = nrow(kept), rows_out = nrow(counts),
       filter_bytes = json_bytes(kept), aggregate_bytes = json_bytes(counts))
})

page_data = list(
  source = list(points = nrow(points), cells = nrow(cells), joined = nrow(joined),
                raw_bytes = raw_bytes, join_bytes = json_bytes(joined)),
  hoods = by_hood,
  model = model
)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(page_data, file.path(root, data_out), auto_unbox = TRUE, digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)
pd = fromJSON(file.path(root, data_out), simplifyVector = FALSE)   # compute on the bytes the page sees

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
st = function(join, filter, aggregate, cache = FALSE, hood = "Roxbury", updates = "daily")
  list(join = join, filter = filter, aggregate = aggregate, cache = cache, hood = hood, updates = updates)
states = list(
  baseline   = st("browser", "browser", "browser"),
  join_view  = st("db", "browser", "browser", cache = TRUE),
  api_middle = st("db", "api", "api"),
  all_db     = st("db", "db", "db", cache = TRUE, hood = "Dorchester"),
  stale      = st("db", "db", "db", cache = TRUE, updates = "click")
)

# ---- 4. MODEL + PANEL CODE [CHANGE] --------------------------------------------------------
stages = c("join", "filter", "aggregate")
readouts_from = function(s) {
  h = pd$hoods[[s$hood]]
  out_bytes = c(join = pd$source$join_bytes, filter = h$filter_bytes, aggregate = h$aggregate_bytes)
  tiers = unlist(s[stages])
  server = stages[tiers != "browser"]
  # bytes into the browser = output of the last server stage, or the raw files if none
  to_browser = if (length(server) == 0) pd$source$raw_bytes else out_bytes[[tail(server, 1)]]
  compute = sum(map_dbl(stages, function(g) {
    tier = if (g == "join" && isTRUE(s$cache) && s$join != "browser") "view" else s[[g]]
    pd$model$compute[[g]][[tier]]
  }))
  db_to_api = any(tiers == "db") && any(tiers == "api")
  internal = if (db_to_api) pd$model$internal_ms + out_bytes[[tail(stages[tiers == "db"], 1)]] / pd$model$internal_bytes_ms else 0
  latency = pd$model$rtt_ms + to_browser / pd$model$browser_bytes_ms + compute + internal
  # on a neighbourhood change: browser stages downstream of the filter re-run; if the filter is
  # on a server, a request fires and every server stage re-runs except a cached join
  request = s$filter != "browser"
  reruns = sum(map_lgl(stages, function(g) {
    if (g == "join") return(request && s$join != "browser" && !isTRUE(s$cache))
    TRUE
  }))
  list(bytes_first_load = to_browser, latency_ms = round(latency, 1), reruns = reruns,
       refetch_bytes = if (request) out_bytes[[tail(server, 1)]] else 0,
       sites = h$sites, stale = if (isTRUE(s$cache) && s$join != "browser" && s$updates == "click") "stale" else "fresh")
}

where = function(tier) c(db = "PostGIS", api = "Plumber (R)", browser = "browser")[[tier]]
code_r = function(s) {
  api = stages[unlist(s[stages]) == "api"]
  lines = c(
    "library(plumber)", "library(dplyr)", "library(sf)", "",
    "#* @get /sites",
    "function(hood = \"" %>% paste0(s$hood, "\") {")
  )
  src = if (s$join == "db") paste0("  tbl(con, \"", if (isTRUE(s$cache)) "joined_view" else "joined_q", "\")")
        else "  points"
  if (s$filter == "db") src = paste0("  tbl(con, \"", if (s$aggregate == "db") "hood_counts" else "hood_sites", "\") %>%\n    filter(neighborhood == hood)")
  body = if (length(api) == 0 && s$filter == "browser") {
    "  # nothing to do here: the browser gets the rows and does the work\n  read_sf(\"boston_social_infra.geojson\")"
  } else {
    paste0(src,
      if ("filter" %in% api) " %>%\n    filter(neighborhood == hood)" else "",
      if ("aggregate" %in% api) " %>%\n    count(cell, group)" else "",
      if (s$join != "browser" || s$filter != "browser") " %>%\n    collect()" else "")
  }
  paste(c(lines, body, "}"), collapse = "\n")
}
code_sql = function(s) {
  if (s$join != "db") return("-- PostGIS only stores the raw tables here;\n-- the spatial join runs in the browser.\nSELECT id, grp, geom FROM points;")
  view = if (isTRUE(s$cache)) "CREATE MATERIALIZED VIEW joined_view AS" else "CREATE VIEW joined_q AS"
  paste0(view, "\nSELECT p.id, p.grp, c.cell, c.neighborhood\nFROM points p\nJOIN cells c ON ST_Within(p.geom, c.geom);",
    if (isTRUE(s$cache)) "\n\n-- refresh nightly, not on every click\nREFRESH MATERIALIZED VIEW joined_view;" else "")
}

code_js = function(s) {
  if (s$join == "browser" && s$filter == "browser")
    return("// every stage runs here: fetch the raw tables once\nfetch(\"boston_social_infra.geojson\")\n  .then(r => r.json())\n  .then(drawMap);")
  if (s$filter == "browser")
    return("// the joined rows arrive once; filter and count run here\nfetch(\"/sites\")\n  .then(r => r.json())\n  .then(drawMap);")
  paste0("// a new request on every neighbourhood change\nfetch(\"/sites?hood=\" + encodeURIComponent(\"", s$hood, "\"))\n  .then(r => r.json())\n  .then(drawMap);")
}

# ---- 5. RUN EACH STATE [KEEP the shape] ----------------------------------------------------
# Readouts come from the model over the stage outputs written above; the panel code is the
# endpoint / view for the placement (it needs a live PostGIS, so it is shown, not run here).
golden_states = imap(states, function(s, name) {
  list(name = name, state = s, readouts = readouts_from(s), code = list(r = code_r(s), sql = code_sql(s), js = code_js(s)))
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
