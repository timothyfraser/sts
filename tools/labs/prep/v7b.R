# v7b.R - prep script for lab v7b "Nearest facility at scale" (copied from _template.R).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v7b.R
# It writes:
#   docs-v3/labs/data/v7b.json           precinct centroids + polling places (metres, EPSG-free Albers)
#   tests/labs/golden/v7b.json           named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# The SQL tab is PostGIS; PostGIS is not available to this script, so the SQL text is golden
# (compared exactly by the gate) but not executed. Every readout comes from running the R tab.

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

# ---- 1. CONFIG -----------------------------------------------------------------------------
lab_id     = "v7b"
page       = "docs-v3/labs/v7b-nearest-at-scale.html"
seed       = 5460
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE ----------------------------------------------------------------------
set.seed(seed)
polls_raw = readRDS(file.path(root, "data/boston_voting/polling_places.rds"))
precincts_raw = readRDS(file.path(root, "data/boston_voting/precincts.rds"))
crs_m = st_crs(polls_raw)   # projected, metres

xy = function(g) { cc = st_coordinates(g); data.frame(x = round(cc[, 1]), y = round(cc[, 2])) }
polls_df = bind_cols(tibble(id = paste0("p", polls_raw$id)), xy(polls_raw))
cent = suppressWarnings(st_point_on_surface(st_geometry(precincts_raw)))
origins_df = bind_cols(tibble(id = paste0("c", precincts_raw$ward_precinct)), xy(cent)) %>%
  arrange(id)
polls_df = polls_df %>% arrange(id)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(list(origins = origins_df, polls = polls_df),
           file.path(root, data_out), dataframe = "columns", digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# Re-read the exact bytes the browser fetches.
raw = read_json(file.path(root, data_out), simplifyVector = TRUE)
centroids = as_tibble(raw$origins) %>% st_as_sf(coords = c("x", "y"), crs = crs_m, remove = FALSE)
polls = as_tibble(raw$polls) %>% st_as_sf(coords = c("x", "y"), crs = crs_m, remove = FALSE)

# ---- 3. STATES -----------------------------------------------------------------------------
states = list(
  baseline   = list(d = 500,  index = FALSE, scale = 1,   algo = "brute"),
  bbox10     = list(d = 500,  index = FALSE, scale = 10,  algo = "bbox"),
  noindex100 = list(d = 500,  index = FALSE, scale = 100, algo = "index"),
  index100   = list(d = 500,  index = TRUE,  scale = 100, algo = "index"),
  tight      = list(d = 250,  index = TRUE,  scale = 1,   algo = "index"),
  wide       = list(d = 1000, index = TRUE,  scale = 1,   algo = "index")
)

# ---- 4. PANEL CODE -------------------------------------------------------------------------
uses_index = function(s) s$algo == "index" && isTRUE(s$index)
plan_of = function(s) if (uses_index(s)) "Index Scan using polls_geom_idx on polls" else "Seq Scan on polls"

code_r = function(s) {
  cost = if (s$algo == "brute") {
    "    comparisons = n * m,                 # brute force: every origin x every poll\n    distance_calcs = n * m"
  } else if (s$algo == "bbox") {
    "    comparisons = n * m,                 # every pair gets a box test; exact distance only inside it\n    distance_calcs = candidates"
  } else if (uses_index(s)) {
    "    comparisons = candidates,            # the GiST index hands back candidates only\n    distance_calcs = candidates"
  } else {
    "    comparisons = n * m,                 # no index to use: falls back to a full scan\n    distance_calcs = candidates"
  }
  paste0(
    "library(dplyr)\nlibrary(sf)\n\n",
    "# ", s$scale, "x synthetic scale-up: copy each precinct centroid ", s$scale, " time", if (s$scale == 1) "" else "s", "\n",
    "origins = centroids %>% slice(rep(1:n(), times = ", s$scale, "))\n\n",
    "# pre-filter: which polls sit within ", s$d, " m of each origin?\n",
    "within = st_is_within_distance(origins, polls, dist = ", s$d, ")\n",
    "# the true nearest poll, found without any radius\n",
    "nearest = st_nearest_feature(origins, polls)\n",
    "gap_m = st_distance(origins, polls[nearest, ], by_element = TRUE) %>% as.numeric()\n\n",
    "tibble(n = nrow(origins), m = nrow(polls),\n",
    "       candidates = sum(lengths(within)),\n",
    "       found = sum(lengths(within) > 0),\n",
    "       missed = sum(lengths(within) == 0),\n",
    "       mean_nearest_m = mean(gap_m)) %>%\n",
    "  mutate(\n", cost, ",\n",
    "    plan = \"", plan_of(s), "\")"
  )
}
code_sql = function(s) {
  if (s$algo == "brute") return(paste0(
    "-- brute force: measure every origin against every poll\n",
    "SELECT o.id, MIN(ST_Distance(o.geom, p.geom)) AS nearest_m\n",
    "FROM origins o CROSS JOIN polls p\n",
    "GROUP BY o.id;"))
  paste0(
    if (isTRUE(s$index)) "CREATE INDEX polls_geom_idx ON polls USING GIST (geom);\n\n" else "-- no spatial index on polls.geom\n\n",
    "SELECT o.id, near.id AS poll_id, near.dist_m\n",
    "FROM origins o\n",
    "CROSS JOIN LATERAL (\n",
    "  SELECT p.id, ST_Distance(o.geom, p.geom) AS dist_m\n",
    "  FROM polls p\n",
    "  WHERE ST_DWithin(o.geom, p.geom, ", s$d, ")\n",
    if (s$algo == "index") "  ORDER BY o.geom <-> p.geom\n" else "  ORDER BY dist_m\n",
    "  LIMIT 1\n",
    ") near;")
}

readouts_from = function(result) {
  r = as.list(result)
  list(comparisons = r$comparisons, distance_calcs = r$distance_calcs, candidates = r$candidates,
       n = r$n, found = r$found, missed = r$missed, mean_nearest_m = r$mean_nearest_m, plan = r$plan)
}

# ---- 5. RUN EACH STATE ---------------------------------------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$centroids = centroids
  env$polls = polls
  result = eval(parse(text = code_r(s)), envir = env)       # the panel's R, run for real
  ro = readouts_from(result)
  ro = map(ro, function(v) if (is.numeric(v) && v == round(v) && abs(v) < 2^31) as.integer(v) else v)
  list(name = name, state = s, readouts = ro, code = list(r = code_r(s), sql = code_sql(s)))
})

# ---- 6. WRITE GOLDEN -----------------------------------------------------------------------
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
