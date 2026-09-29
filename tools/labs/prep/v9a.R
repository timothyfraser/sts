# v9a.R - prep for lab v9a "A network is two tables: edges, nodes and the join that builds them".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v9a.R
# Built from tools/labs/prep/_template.R (the [KEEP] blocks are unchanged in intent).
#
# SOURCE: data/bluebikes/bluebikes.zip -> bluebikes.sqlite (unzipped into tempdir(), never into data/),
#   tables tally_rush_edges (start_code, end_code, day, rush, count) and stationbg_dataset (code, x, y).
#   Sample: July 2021, AM + PM rush hour, the 25 stations with the most rush-hour trips that month
#   (trips counted where the station is the start OR the end), keeping only trips between two of them.
#   No randomness: the station choice is a deterministic top-25 (ties broken by code).
#
# OUTPUTS (all read by the page; v9a_nodes.csv + v9a_edges.csv are also the input for later network labs)
#
#   docs-v3/labs/data/v9a_trips.csv   the raw tally rows: one row per (start, end, day, rush period)
#     from   chr  station code where the rides started (e.g. "A32010")
#     to     chr  station code where the rides ended
#     day    chr  "YYYY-MM-DD"
#     rush   chr  "am" | "pm"
#     count  int  rides on that day in that rush period between from and to (>= 1)
#
#   docs-v3/labs/data/v9a_nodes.csv   the NODE table: one row per station, stable order
#     id        int  1..25, stable node id (row order = matrix row/column order)
#     code      chr  Bluebikes station code, the key edges join on (the data has no station names;
#                    the code IS the station's name in this dataset)
#     lon, lat  dbl  WGS84 longitude / latitude (from stationbg_dataset x / y)
#     trips_out int  rides starting here (AM + PM, July 2021, within the 25-station sample)
#     trips_in  int  rides ending here (same scope)
#     degree_out, degree_in  int  distinct directed neighbours (weight >= 1, AM + PM), self-loop counts once
#
#   docs-v3/labs/data/v9a_edges.csv   the EDGE table: directed, weighted, AM + PM, weight >= 1
#     from, to  chr  station codes (join to nodes$code)
#     from_id, to_id int  the matching nodes$id
#     weight    int  rides from -> to in July 2021 rush hours (sum of count)
#
#   tests/labs/golden/v9a.json   named states, readouts, code (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(igraph)

Sys.setlocale("LC_COLLATE", "C")   # pmin()/pmax()/arrange() on codes sort like the browser does

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
lab_id     = "v9a"
page       = "docs-v3/labs/v9a-network-as-tables.html"
source     = "data/bluebikes/bluebikes.zip"
seed       = 5460
n_stations = 25
month      = "2021-07"
trips_out  = "docs-v3/labs/data/v9a_trips.csv"
nodes_out  = "docs-v3/labs/data/v9a_nodes.csv"
edges_out  = "docs-v3/labs/data/v9a_edges.csv"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + SAMPLE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
set.seed(seed)
tmp = file.path(tempdir(), "v9a-bluebikes")
dir.create(tmp, showWarnings = FALSE)
sqlite = file.path(tmp, "bluebikes.sqlite")
if (!file.exists(sqlite)) unzip(file.path(root, source), junkpaths = TRUE, exdir = tmp)
db = dbConnect(SQLite(), sqlite)

month_rows = db %>%
  tbl("tally_rush_edges") %>%
  filter(day >= !!paste0(month, "-01"), day <= !!paste0(month, "-31")) %>%
  collect()

top = bind_rows(
    month_rows %>% select(code = start_code, count),
    month_rows %>% select(code = end_code, count)) %>%
  group_by(code) %>%
  summarize(total = sum(count), .groups = "drop") %>%
  arrange(desc(total), code) %>%
  slice_head(n = n_stations)

stations = db %>% tbl("stationbg_dataset") %>% select(code, x, y) %>% collect()
dbDisconnect(db)

trips_df = month_rows %>%
  filter(start_code %in% top$code, end_code %in% top$code) %>%
  transmute(from = start_code, to = end_code, day, rush, count = as.integer(count)) %>%
  arrange(from, to, day, rush)

edges_df = trips_df %>%
  count(from, to, wt = count, name = "weight")

nodes_df = top %>%
  select(code) %>%
  arrange(code) %>%
  mutate(id = row_number()) %>%
  left_join(stations, by = "code") %>%
  transmute(id, code, lon = round(x, 5), lat = round(y, 5)) %>%
  left_join(edges_df %>% group_by(code = from) %>% summarize(trips_out = sum(weight), degree_out = n()), by = "code") %>%
  left_join(edges_df %>% group_by(code = to) %>% summarize(trips_in = sum(weight), degree_in = n()), by = "code") %>%
  mutate(across(c(trips_out, degree_out, trips_in, degree_in), ~ as.integer(coalesce(.x, 0L)))) %>%
  select(id, code, lon, lat, trips_out, trips_in, degree_out, degree_in)
stopifnot(!any(is.na(nodes_df$lon)))

edges_file = edges_df %>%
  left_join(nodes_df %>% select(from = code, from_id = id), by = "from") %>%
  left_join(nodes_df %>% select(to = code, to_id = id), by = "to") %>%
  select(from, to, from_id, to_id, weight) %>%
  mutate(weight = as.integer(weight))

dir.create(file.path(root, "docs-v3/labs/data"), recursive = TRUE, showWarnings = FALSE)
write_csv(trips_df, file.path(root, trips_out), na = "")
write_csv(nodes_df, file.path(root, nodes_out), na = "")
write_csv(edges_file, file.path(root, edges_out), na = "")
total_bytes = sum(file.size(file.path(root, c(trips_out, nodes_out, edges_out))))
stopifnot(total_bytes <= 500 * 1024)

# Re-read the files the page will fetch. The panel code calls them `trips` and `nodes`.
trips = read_csv(file.path(root, trips_out), show_col_types = FALSE, col_types = "cccci")
nodes = read_csv(file.path(root, nodes_out), show_col_types = FALSE) %>% select(code, id, lon, lat)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
# Same names and fields as `states` in the page's Lab.create(...). baseline comes first.
states = list(
  baseline   = list(directed = TRUE,  min_w = 1, window = "all", view = "edges"),
  undirected = list(directed = FALSE, min_w = 1, window = "all", view = "edges"),
  strong     = list(directed = TRUE,  min_w = 5, window = "all", view = "edges"),
  am_only    = list(directed = TRUE,  min_w = 1, window = "am",  view = "edges"),
  matrix     = list(directed = TRUE,  min_w = 1, window = "all", view = "matrix")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
rush_of = function(w) if (w == "all") 'c("am", "pm")' else paste0('c("', w, '")')
rush_sql = function(w) if (w == "all") "('am', 'pm')" else paste0("('", w, "')")

code_r = function(s) {
  paste0(
    "edges = trips %>%\n",
    "  filter(rush %in% ", rush_of(s$window), ")",
    if (!s$directed) " %>%\n  mutate(a = pmin(from, to), b = pmax(from, to), from = a, to = b)" else "",
    " %>%\n",
    "  count(from, to, wt = count, name = \"weight\") %>%\n",
    "  filter(weight >= ", s$min_w, ")\n",
    "\n",
    "g = graph_from_data_frame(edges, directed = ", if (s$directed) "TRUE" else "FALSE", ", vertices = nodes)\n",
    "# tidygraph: tbl_graph(nodes = nodes, edges = edges, node_key = \"code\", directed = ", if (s$directed) "TRUE" else "FALSE", ")\n",
    "adj = as_adjacency_matrix(g, attr = \"weight\", sparse = FALSE)\n",
    "\n",
    "tibble(tally_rows = nrow(filter(trips, rush %in% ", rush_of(s$window), ")),\n",
    "       trips = sum(edges$weight), edges = gsize(g), nodes = gorder(g),\n",
    "       cells_filled = sum(adj > 0), cells_total = length(adj),\n",
    "       matrix_bytes = 8 * length(adj), edgelist_bytes = 8 * 3 * gsize(g))"
  )
}
code_sql = function(s) {
  a = if (s$directed) "start_code" else "MIN(start_code, end_code)"
  b = if (s$directed) "end_code" else "MAX(start_code, end_code)"
  paste0(
    "SELECT ", a, " AS \"from\", ", b, " AS \"to\", SUM(count) AS weight\n",
    "FROM tally_rush_edges\n",
    "WHERE day BETWEEN '2021-07-01' AND '2021-07-31'\n",
    "  AND rush IN ", rush_sql(s$window), "\n",
    "  AND start_code IN (SELECT code FROM nodes)\n",
    "  AND end_code IN (SELECT code FROM nodes)\n",
    "GROUP BY 1, 2\n",
    "HAVING SUM(count) >= ", s$min_w, ";"
  )
}
readouts_from = function(result) as.list(result)

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "tally_rush_edges", as.data.frame(trips %>% rename(start_code = from, end_code = to)))
dbWriteTable(con, "nodes", as.data.frame(nodes))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$trips = trips
  env$nodes = nodes
  result = eval(parse(text = code_r(s)), envir = env)       # the panel's R, run for real
  code = list(r = code_r(s), sql = code_sql(s))
  via_sql = dbGetQuery(con, code$sql)                       # the panel's SQL, run for real
  if (nrow(via_sql) != nrow(env$edges) || sum(via_sql$weight) != sum(env$edges$weight))
    stop("prep: SQL and R disagree for state '", name, "'")
  r = readouts_from(result)
  r = map(r, function(v) if (is.numeric(v)) as.numeric(v) else v)
  list(name = name, state = s, readouts = r, code = code)
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
message("prep: wrote ", trips_out, ", ", nodes_out, ", ", edges_out, " (", total_bytes, " B) and ", golden_out)
