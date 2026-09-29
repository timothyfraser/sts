# v6a.R - prep script for lab v6a (spatial joins, geometry by geometry).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v6a.R
# It writes:
#   docs-v3/labs/data/v6a_bg.geojson     Boston block groups, simplified (the polygons)
#   docs-v3/labs/data/v6a_sites.csv      social-infrastructure sites (the points)
#   docs-v3/labs/data/v6a_keys.json      which polygon each point matches under each predicate,
#                                        computed by sf on exactly the two files above
#   tests/labs/golden/v6a.json           named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
# Copied from _template.R; only the [CHANGE] blocks differ.

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
lab_id     = "v6a"
page       = "docs-v3/labs/v6a-spatial-join.html"
src_points = "data/boston_social_infra/boston_social_infra.geojson"
src_polys  = "data/boston_social_infra/boston_block_groups.geojson"
seed       = 5460                       # nothing is sampled; kept so the template shape holds
bg_out     = "docs-v3/labs/data/v6a_bg.geojson"
sites_out  = "docs-v3/labs/data/v6a_sites.csv"
keys_out   = "docs-v3/labs/data/v6a_keys.json"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE [CHANGE the pipeline, KEEP write + re-read] --------------------------
set.seed(seed)
dir.create(file.path(root, "docs-v3/labs/data"), recursive = TRUE, showWarnings = FALSE)

# Polygons: simplify 20 m in a metre CRS (Massachusetts State Plane) so the page stays small.
bg_small = read_sf(file.path(root, src_polys)) %>%
  select(geoid) %>%
  arrange(geoid) %>%
  st_transform(26986) %>%
  st_simplify(dTolerance = 20, preserveTopology = TRUE) %>%
  st_transform(4326)
if (file.exists(file.path(root, bg_out))) file.remove(file.path(root, bg_out))
st_write(bg_small, file.path(root, bg_out), quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=5", "RFC7946=YES", "WRITE_NAME=NO"))

# Points: id, type and lon/lat rounded to 5 decimals (about 1 m).
sites_small = read_sf(file.path(root, src_points)) %>%
  st_zm() %>%
  mutate(lon = round(st_coordinates(geometry)[, 1], 5),
         lat = round(st_coordinates(geometry)[, 2], 5)) %>%
  as_tibble() %>%
  transmute(id = as.integer(id), type = group, lon, lat) %>%
  arrange(id)
write_csv(sites_small, file.path(root, sites_out), na = "")

# Re-read the files the page fetches, so R computes on exactly the bytes the browser sees.
bg = read_sf(file.path(root, bg_out)) %>% select(geoid)
sites = read_csv(file.path(root, sites_out), show_col_types = FALSE) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) %>%
  select(id, type)

# The geometry half of the join, precomputed: for every point, the polygon(s) it matches under
# each predicate; for every polygon, the point(s) it matches. The page does the relational half.
# Indices are 0-based positions in v6a_bg.geojson features / v6a_sites.csv rows (JavaScript order).
idx0 = function(hits) map(hits, function(i) as.integer(i) - 1L)
keys = list(
  geoid      = bg$geoid,
  site_id    = sites$id,
  intersects = idx0(st_intersects(sites, bg)),
  within     = idx0(st_within(sites, bg)),
  nearest    = st_nearest_feature(sites, bg) - 1L,
  bg_within  = idx0(st_within(bg, sites)),
  bg_nearest = st_nearest_feature(bg, sites) - 1L
)
write_json(keys, file.path(root, keys_out), auto_unbox = FALSE, digits = NA)
stopifnot(sum(file.size(file.path(root, c(bg_out, sites_out, keys_out)))) <= 500 * 1024)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline     = list(pred = "intersects", dir = "p2g", how = "left",  agg = "count"),
  inner        = list(pred = "intersects", dir = "p2g", how = "inner", agg = "count"),
  poly_left    = list(pred = "intersects", dir = "g2p", how = "left",  agg = "count"),
  poly_inner   = list(pred = "intersects", dir = "g2p", how = "inner", agg = "count"),
  within       = list(pred = "within",     dir = "p2g", how = "left",  agg = "count"),
  within_poly  = list(pred = "within",     dir = "g2p", how = "left",  agg = "count"),
  nearest      = list(pred = "nearest",    dir = "p2g", how = "left",  agg = "count"),
  nearest_poly = list(pred = "nearest",    dir = "g2p", how = "left",  agg = "count"),
  by_type      = list(pred = "intersects", dir = "p2g", how = "left",  agg = "type")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
join_fn = c(intersects = "st_intersects", within = "st_within", nearest = "st_nearest_feature")
empty_line = c(
  intersects = "sum(lengths(st_intersects(bg, sites)) == 0)",
  within     = "sum(lengths(st_contains(bg, sites)) == 0)",
  nearest    = "sum(!bg$geoid %in% bg$geoid[st_nearest_feature(sites, bg)])"
)
code_r = function(s) {
  x = if (s$dir == "p2g") "sites" else "bg"
  y = if (s$dir == "p2g") "bg" else "sites"
  paste0(
    "# the join: one row per match (left = TRUE keeps the unmatched rows too)\n",
    "joined = ", x, " %>%\n",
    "  st_join(", y, ", join = ", join_fn[[s$pred]], ", left = ", if (s$how == "left") "TRUE" else "FALSE", ")\n",
    "\n",
    "# the aggregate: sites per block group", if (s$agg == "type") " and type" else "", "\n",
    "counts = joined %>%\n",
    "  as_tibble() %>%\n",
    "  group_by(geoid", if (s$agg == "type") ", type" else "", ") %>%\n",
    "  summarize(count = sum(!is.na(id)), .groups = \"drop\")\n",
    "\n",
    "# block groups with no site at all\n",
    "empty = ", empty_line[[s$pred]]
  )
}
sql_on = function(s) {
  a = if (s$dir == "p2g") "s.geom, b.geom" else "b.geom, s.geom"
  if (s$pred == "intersects") paste0("ST_Intersects(", a, ")") else paste0("ST_Within(", a, ")")
}
code_sql = function(s) {
  from = if (s$dir == "p2g") "sites s" else "bg b"
  kind = if (s$how == "left") "LEFT JOIN" else "JOIN"
  sel  = paste0("SELECT b.geoid", if (s$agg == "type") ", s.type" else "", ", COUNT(s.id) AS count\n")
  grp  = paste0("GROUP BY b.geoid", if (s$agg == "type") ", s.type" else "", ";")
  if (s$pred == "nearest") {
    if (s$dir == "p2g") {
      join = paste0("FROM sites s\n", kind, " LATERAL (\n",
                    "  SELECT geoid, geom FROM bg ORDER BY bg.geom <-> s.geom LIMIT 1\n",
                    ") b ON TRUE\n")
    } else {
      join = paste0("FROM bg b\n", kind, " LATERAL (\n",
                    "  SELECT id, type, geom FROM sites ORDER BY sites.geom <-> b.geom LIMIT 1\n",
                    ") s ON TRUE\n")
    }
  } else {
    other = if (s$dir == "p2g") "bg b" else "sites s"
    join = paste0("FROM ", from, "\n", kind, " ", other, "\n  ON ", sql_on(s), "\n")
  }
  paste0("-- PostGIS: the join and the aggregate in one query\n", sel, join, grp)
}

# The readouts the gate compares (same keys as the page's readouts()).
readouts_from = function(env, s) {
  joined = env$joined
  counts = env$counts
  unmatched = if (s$dir == "p2g") sum(is.na(joined$geoid)) else sum(is.na(joined$id))
  per_bg = counts %>% filter(!is.na(geoid)) %>% group_by(geoid) %>% summarize(n = sum(count))
  list(
    rows_out   = nrow(joined),
    unmatched  = unmatched,
    agg_rows   = nrow(counts),
    zero_rows  = sum(counts$count == 0),
    max_per_bg = if (nrow(per_bg)) max(per_bg$n) else 0,
    empty_bg   = env$empty
  )
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$sites = sites
  env$bg = bg
  suppressMessages(eval(parse(text = code_r(s)), envir = env))   # the panel's R, run for real
  code = list(r = code_r(s), sql = code_sql(s))                  # PostGIS twin: shown, not run here
  list(name = name, state = s, readouts = readouts_from(env, s), code = code)
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
message("prep: wrote ", bg_out, ", ", sites_out, ", ", keys_out, " (",
        sum(file.size(file.path(root, c(bg_out, sites_out, keys_out)))), " B) and ", golden_out)
