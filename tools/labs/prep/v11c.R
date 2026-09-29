# v11c.R - prep for the origin-destination flows lab (chapter 11).
# Built from tools/labs/prep/_template.R; only the [CHANGE] blocks differ.
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v11c.R
# It writes:
#   docs-v3/labs/data/v11c-flows.csv        county-to-county evacuation flows (increases only)
#   docs-v3/labs/data/v11c-counties.geojson simplified county outlines + centroids
#   tests/labs/golden/v11c.json             named states, readouts, code, acks
# Running it twice must leave `git status` unchanged (byte-for-byte output).

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)
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
lab_id     = "v11c"
page       = "docs-v3/labs/v11c-od-flows.html"
source     = "data/evacuation/edges.rds"
source_geo = "data/evacuation/counties.geojson"
seed       = 5460
data_out   = paste0("docs-v3/labs/data/", lab_id, "-flows.csv")
geo_out    = paste0("docs-v3/labs/data/", lab_id, "-counties.geojson")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
# One row per county pair: the summed INCREASE in movement over the storm (evacuation > 0),
# because mixing increases with decreases (sheltering in place) cancels two different things.
set.seed(seed)
flows_df = readRDS(file.path(root, source)) %>%
  filter(evacuation > 0) %>%
  mutate(origin = substr(from_geoid, 1, 5), dest = substr(to_geoid, 1, 5)) %>%
  group_by(origin, dest) %>%
  summarise(flow = round(sum(evacuation), 2), .groups = "drop") %>%
  filter(flow > 0)

counties = st_read(file.path(root, source_geo), quiet = TRUE) %>%
  filter(geoid %in% c(flows_df$origin, flows_df$dest))
flows_df = flows_df %>%
  inner_join(counties %>% st_drop_geometry() %>% select(origin = geoid, origin_state = state), by = "origin") %>%
  inner_join(counties %>% st_drop_geometry() %>% select(dest = geoid, dest_state = state), by = "dest") %>%
  select(origin, dest, origin_state, dest_state, flow) %>%
  arrange(origin, dest)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(flows_df, file.path(root, data_out), na = "")

sf_use_s2(FALSE)
cent = suppressWarnings(st_coordinates(st_point_on_surface(counties)))
geo = counties %>%
  mutate(cx = round(cent[, 1], 4), cy = round(cent[, 2], 4)) %>%
  select(geoid, state, name, cx, cy) %>%
  st_simplify(dTolerance = 0.02, preserveTopology = TRUE) %>% suppressWarnings() %>%
  arrange(geoid)
if (file.exists(file.path(root, geo_out))) file.remove(file.path(root, geo_out))
st_write(geo, file.path(root, geo_out), driver = "GeoJSON", quiet = TRUE,
         layer_options = c("COORDINATE_PRECISION=3", "RFC7946=YES"))
stopifnot(file.size(file.path(root, data_out)) + file.size(file.path(root, geo_out)) <= 500 * 1024)

# Re-read the file the page will fetch. The panel code calls it `flows`.
flows = read_csv(file.path(root, data_out), show_col_types = FALSE,
                 col_types = cols(origin = "c", dest = "c", .default = "?"))

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline   = list(zone = "county", min_flow = 100, direction = "gross", top_n = 20),
  net        = list(zone = "county", min_flow = 100, direction = "net",   top_n = 20),
  region     = list(zone = "state",  min_flow = 100, direction = "gross", top_n = 20),
  region_net = list(zone = "state",  min_flow = 100, direction = "net",   top_n = 20),
  strict     = list(zone = "county", min_flow = 1000, direction = "gross", top_n = 10)
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
zone_cols = function(s) if (s$zone == "state") c("origin_state", "dest_state") else c("origin", "dest")
code_r = function(s) {
  z = zone_cols(s)
  paste0(
    "od = flows %>%\n",
    "  group_by(origin_zone = ", z[1], ", dest_zone = ", z[2], ") %>%\n",
    "  summarise(flow = sum(flow), .groups = \"drop\") %>%\n",
    "  filter(origin_zone != dest_zone)",
    if (s$direction == "net") paste0(
      "\n\n# net flow: subtract the reverse corridor (a self-join)\n",
      "od = od %>%\n",
      "  left_join(od, by = c(\"origin_zone\" = \"dest_zone\", \"dest_zone\" = \"origin_zone\"),\n",
      "            suffix = c(\"\", \"_back\")) %>%\n",
      "  mutate(flow = flow - coalesce(flow_back, 0)) %>%\n",
      "  filter(flow > 0) %>%\n",
      "  select(origin_zone, dest_zone, flow)") else "",
    "\n\n",
    "kept = od %>%\n",
    "  filter(flow >= ", s$min_flow, ") %>%\n",
    "  arrange(desc(flow))\n\n",
    "kept %>%\n",
    "  summarise(pairs_kept = n(), flow_kept = sum(flow), share_kept = sum(flow) / sum(od$flow))\n\n",
    "kept %>% slice_head(n = ", s$top_n, ")  # the corridors drawn on the map"
  )
}
code_sql = function(s) {
  z = zone_cols(s)
  paste0(
    "WITH od AS (\n",
    "  SELECT ", z[1], " AS origin_zone, ", z[2], " AS dest_zone, SUM(flow) AS flow\n",
    "  FROM flows\n",
    "  WHERE ", z[1], " <> ", z[2], "\n",
    "  GROUP BY ", z[1], ", ", z[2], "\n",
    ")",
    if (s$direction == "net") paste0(
      ",\nnet AS (\n",
      "  SELECT a.origin_zone, a.dest_zone, a.flow - COALESCE(b.flow, 0) AS flow\n",
      "  FROM od a\n",
      "  LEFT JOIN od b ON a.origin_zone = b.dest_zone AND a.dest_zone = b.origin_zone\n",
      "  WHERE a.flow - COALESCE(b.flow, 0) > 0\n",
      ")") else "",
    "\nSELECT origin_zone, dest_zone, flow\n",
    "FROM ", if (s$direction == "net") "net" else "od", "\n",
    "WHERE flow >= ", s$min_flow, "\n",
    "ORDER BY flow DESC;"
  )
}

readouts_from = function(result, env, s) {
  out = as.list(result)
  top = env$kept %>% slice_head(n = 1)
  out$top_corridor = if (nrow(top)) paste0(top$origin_zone, " -> ", top$dest_zone) else "none"
  out
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "flows", as.data.frame(flows))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$flows = flows
  result = eval(parse(text = code_r(s)), envir = env)
  result = env$kept %>%
    summarise(pairs_kept = n(), flow_kept = sum(flow), share_kept = sum(flow) / sum(env$od$flow))
  code = list(r = code_r(s), sql = code_sql(s))
  via_sql = dbGetQuery(con, code$sql)
  kept = env$kept
  if (nrow(via_sql) != nrow(kept) || !isTRUE(all.equal(via_sql$flow, kept$flow)))
    stop("prep: SQL and R disagree for state '", name, "'")
  list(name = name, state = s, readouts = readouts_from(result, env, s), code = code)
})
dbDisconnect(con)

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
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B), ",
        geo_out, " (", file.size(file.path(root, geo_out)), " B) and ", golden_out)
