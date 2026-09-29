# v10b.R - prep for lab v10b "Counting neighbours: k hops as repeated joins".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v10b.R
# Built from tools/labs/prep/_template.R (the [KEEP] blocks are unchanged in intent).
#
# SOURCE: the station network written by tools/labs/prep/v9a.R (read only, never rewritten here):
#   docs-v3/labs/data/v9a_edges.csv  (from, to, from_id, to_id, weight: directed rides, July 2021 rush hours)
#   docs-v3/labs/data/v9a_nodes.csv  (id, code, lon, lat, ...: the 25 stations)
#   No randomness: every step is deterministic.
#
# OUTPUT (read by the page)
#
#   docs-v3/labs/data/v10b_edges.csv  the UNDIRECTED link table, stored in both directions so a join
#                                     on `from` walks either way; self-loops dropped
#     from, to  chr  station codes (join to v9a_nodes.csv code)
#     weight    int  rides between the two stations, both directions summed (>= 1)
#
#   tests/labs/golden/v10b.json   named states, readouts, code (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(igraph)

Sys.setlocale("LC_COLLATE", "C")   # arrange() on codes sorts like the browser does

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
lab_id     = "v10b"
page       = "docs-v3/labs/v10b-k-hop-neighbours.html"
edges_in   = "docs-v3/labs/data/v9a_edges.csv"
nodes_in   = "docs-v3/labs/data/v9a_nodes.csv"
seed       = 5460
edges_out  = "docs-v3/labs/data/v10b_edges.csv"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + SAMPLE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
set.seed(seed)
directed = read_csv(file.path(root, edges_in), show_col_types = FALSE, col_types = "cciii")

pairs = directed %>%
  filter(from != to) %>%
  mutate(a = pmin(from, to), b = pmax(from, to)) %>%
  count(a, b, wt = weight, name = "weight")

links_df = bind_rows(
    pairs %>% select(from = a, to = b, weight),
    pairs %>% select(from = b, to = a, weight)) %>%
  mutate(weight = as.integer(weight)) %>%
  arrange(from, to)

dir.create(file.path(root, "docs-v3/labs/data"), recursive = TRUE, showWarnings = FALSE)
write_csv(links_df, file.path(root, edges_out), na = "")
total_bytes = file.size(file.path(root, edges_out))
stopifnot(total_bytes <= 500 * 1024)

# Re-read the files the page will fetch. The panel code calls them `edges` and `nodes`.
edges = read_csv(file.path(root, edges_out), show_col_types = FALSE, col_types = "cci")
nodes = read_csv(file.path(root, nodes_in), show_col_types = FALSE) %>% select(code, lon, lat)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
# Same names and fields as `states` in the page's Lab.create(...). baseline comes first.
states = list(
  baseline    = list(seed = "B32005", k = 2, dedupe = TRUE,  min_w = 80),
  one_hop     = list(seed = "B32005", k = 1, dedupe = TRUE,  min_w = 80),
  k3_dedupe   = list(seed = "B32005", k = 3, dedupe = TRUE,  min_w = 80),
  k3_raw      = list(seed = "B32005", k = 3, dedupe = FALSE, min_w = 80),
  k4_raw      = list(seed = "B32005", k = 4, dedupe = FALSE, min_w = 80),
  dense_raw   = list(seed = "B32005", k = 4, dedupe = FALSE, min_w = 20),
  other_seed  = list(seed = "D32016", k = 2, dedupe = TRUE,  min_w = 80)
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
code_r = function(s) {
  paste0(
    "links = edges %>%\n",
    "  filter(weight >= ", s$min_w, ")\n",
    "\n",
    "frontier = tibble(node = \"", s$seed, "\")\n",
    "reached = tibble(node = character(), hop = integer())\n",
    "hop_rows = integer()\n",
    "hop_kept = integer()\n",
    "for (hop in 1:", s$k, ") {\n",
    "  frontier = frontier %>%\n",
    "    inner_join(links, by = c(\"node\" = \"from\"), relationship = \"many-to-many\") %>%\n",
    "    select(node = to)\n",
    "  hop_rows[hop] = nrow(frontier)\n",
    if (s$dedupe) "  frontier = frontier %>% distinct(node)\n" else "  # dedupe off: every walk is kept, repeats and all\n",
    "  hop_kept[hop] = nrow(frontier)\n",
    "  reached = bind_rows(reached, tibble(node = frontier$node, hop = hop))\n",
    "}\n",
    "\n",
    "g = graph_from_data_frame(links, directed = FALSE, vertices = nodes)\n",
    "# tidygraph: morph(to_local_neighborhood, node = \"", s$seed, "\", order = ", s$k, ")\n",
    "rings = reached %>%\n",
    "  filter(node != \"", s$seed, "\") %>%\n",
    "  group_by(node) %>%\n",
    "  summarize(hop = min(hop)) %>%\n",
    "  count(hop)\n",
    "ring = function(h) sum(rings$n[rings$hop == h])\n",
    "\n",
    "tibble(neighbours = ego_size(g, order = ", s$k, ", nodes = \"", s$seed, "\", mindist = 1),\n",
    "       join_rows = sum(hop_rows), kept_rows = sum(hop_kept),\n",
    "       ring_1 = ring(1), ring_2 = ring(2), ring_3 = ring(3), ring_4 = ring(4),\n",
    "       rows_1 = hop_rows[1], rows_2 = coalesce(hop_rows[2], 0L),\n",
    "       rows_3 = coalesce(hop_rows[3], 0L), rows_4 = coalesce(hop_rows[4], 0L))"
  )
}
code_sql = function(s) {
  paste0(
    "WITH RECURSIVE walk(node, hop) AS (\n",
    "  SELECT '", s$seed, "', 0\n",
    if (s$dedupe) "  UNION\n" else "  UNION ALL\n",
    "  SELECT e.\"to\", w.hop + 1\n",
    "  FROM walk w\n",
    "  JOIN edges e ON e.\"from\" = w.node\n",
    "  WHERE w.hop < ", s$k, " AND e.weight >= ", s$min_w, "\n",
    ")\n",
    "SELECT COUNT(*) - 1 AS kept_rows,\n",
    "       COUNT(DISTINCT CASE WHEN node <> '", s$seed, "' THEN node END) AS neighbours\n",
    "FROM walk;"
  )
}
readouts_from = function(result) as.list(result)

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "edges", as.data.frame(edges))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$edges = edges
  env$nodes = nodes
  result = eval(parse(text = code_r(s)), envir = env)       # the panel's R, run for real
  code = list(r = code_r(s), sql = code_sql(s))
  via_sql = dbGetQuery(con, code$sql)                       # the panel's SQL, run for real
  if (via_sql$kept_rows != result$kept_rows || via_sql$neighbours != result$neighbours)
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
message("prep: wrote ", edges_out, " (", total_bytes, " B) and ", golden_out)
