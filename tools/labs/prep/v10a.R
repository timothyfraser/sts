# v10a.R - prep for lab v10a "Centrality at scale: who matters, by which measure".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v10a.R
# Built from tools/labs/prep/_template.R (the [KEEP] blocks are unchanged in intent).
#
# SOURCE: the network-as-tables lab's outputs (read only, never rewritten here):
#   docs-v3/labs/data/v9a_nodes.csv  (id, code, lon, lat, ...)   25 Bluebikes stations
#   docs-v3/labs/data/v9a_edges.csv  (from, to, from_id, to_id, weight)  directed rush-hour rides, July 2021
# Reduction: drop self-loops, fold both directions into one undirected edge (station codes sorted
#   with pmin/pmax, weight = rides both ways), keep edges with at least `min_rides` rides. No randomness.
#
# OUTPUTS
#   docs-v3/labs/data/v10a_nodes.csv   code chr (the vertex name), lon dbl, lat dbl
#   docs-v3/labs/data/v10a_edges.csv   from chr, to chr (from < to), weight int (rides both ways)
#   tests/labs/golden/v10a.json        named states, readouts, code (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# The code panel uses igraph. tidygraph's centrality_degree(), centrality_betweenness() and
# centrality_closeness() call these same igraph functions; tidygraph is not installed on the build
# machine, so the panel shows (and this script runs) the igraph calls directly.

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(igraph)

invisible(Sys.setlocale("LC_COLLATE", "C"))   # arrange() on codes sorts like the browser does

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
lab_id     = "v10a"
page       = "docs-v3/labs/v10a-centrality-lab.html"
min_rides  = 30                                            # keep undirected edges with >= 30 rides
nodes_out  = "docs-v3/labs/data/v10a_nodes.csv"
edges_out  = "docs-v3/labs/data/v10a_edges.csv"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE [CHANGE the pipeline, KEEP write + re-read] --------------------------
nodes_src = read_csv(file.path(root, "docs-v3/labs/data/v9a_nodes.csv"), show_col_types = FALSE)
edges_src = read_csv(file.path(root, "docs-v3/labs/data/v9a_edges.csv"), show_col_types = FALSE)

nodes_df = nodes_src %>%
  select(code, lon, lat) %>%
  arrange(code)
edges_df = edges_src %>%
  filter(from != to) %>%
  mutate(a = pmin(from, to), b = pmax(from, to)) %>%
  group_by(a, b) %>%
  summarize(weight = sum(weight), .groups = "drop") %>%
  filter(weight >= min_rides) %>%
  select(from = a, to = b, weight) %>%
  arrange(from, to)

write_csv(nodes_df, file.path(root, nodes_out), na = "")
write_csv(edges_df, file.path(root, edges_out), na = "")
stopifnot(file.size(file.path(root, nodes_out)) + file.size(file.path(root, edges_out)) <= 500 * 1024)

# Re-read the files the page will fetch, so R computes on exactly the bytes the browser sees.
nodes = read_csv(file.path(root, nodes_out), show_col_types = FALSE)
edges = read_csv(file.path(root, edges_out), show_col_types = FALSE)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
build = function(remove) {
  g = graph_from_data_frame(edges, directed = FALSE, vertices = nodes)
  if (remove != "none") g = delete_vertices(g, remove)
  g
}
score_of = function(g, measure, weighted) {
  switch(measure,
    degree      = if (weighted) strength(g, weights = E(g)$weight) else degree(g),
    betweenness = betweenness(g, weights = if (weighted) 1 / E(g)$weight else NA),
    closeness   = closeness(g, weights = if (weighted) 1 / E(g)$weight else NA))
}
b0 = betweenness(build("none"), weights = NA)
top_btw = names(b0)[order(-b0, names(b0))][1]

states = list(
  baseline    = list(measure = "betweenness", weighted = FALSE, remove = "none",  k = 5),
  removed     = list(measure = "betweenness", weighted = FALSE, remove = top_btw, k = 5),
  degree      = list(measure = "degree",      weighted = FALSE, remove = "none",  k = 5),
  strength    = list(measure = "degree",      weighted = TRUE,  remove = "none",  k = 5),
  closeness_w = list(measure = "closeness",   weighted = TRUE,  remove = "none",  k = 10),
  removed_w   = list(measure = "betweenness", weighted = TRUE,  remove = top_btw, k = 5)
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
score_line = function(s) {
  w = if (s$weighted) "1 / E(g)$weight" else "NA"
  switch(s$measure,
    degree      = if (s$weighted) "scores = strength(g, weights = E(g)$weight)" else "scores = degree(g)",
    betweenness = paste0("scores = betweenness(g, weights = ", w, ")"),
    closeness   = paste0("scores = closeness(g, weights = ", w, ")"))
}
code_r = function(s) {
  paste0(
    "g = graph_from_data_frame(edges, directed = FALSE, vertices = nodes)\n",
    if (s$remove != "none") paste0("g = delete_vertices(g, \"", s$remove, "\")\n") else "",
    score_line(s), "\n\n",
    "top = tibble(code = names(scores), score = scores) %>%\n",
    "  arrange(desc(score), code) %>%\n",
    "  slice_head(n = ", s$k, ")\n\n",
    "apl = mean_distance(g, weights = NA)"
  )
}
code_sql = function(s) {
  where = if (s$remove != "none") paste0("  WHERE \"from\" <> '", s$remove, "' AND \"to\" <> '", s$remove, "'\n") else ""
  paste0(
    "SELECT code, COUNT(*) AS degree, SUM(weight) AS strength\n",
    "FROM (\n",
    "  SELECT \"from\" AS code, weight FROM edges\n", where,
    "  UNION ALL\n",
    "  SELECT \"to\" AS code, weight FROM edges\n", where,
    ") AS ends\n",
    "GROUP BY code\n",
    "ORDER BY ", if (s$weighted) "strength" else "degree", " DESC, code\n",
    "LIMIT ", s$k, ";"
  )
}

# Readouts: the panel's top and apl, plus the betweenness change caused by the removal
# (same weighting as the state; betweenness after removal minus betweenness before, per station).
readouts_from = function(env, s) {
  rise_code = "none"; rise_value = 0
  if (s$remove != "none") {
    w_full = if (s$weighted) 1 / E(build("none"))$weight else NA
    before = betweenness(build("none"), weights = w_full)
    g1 = build(s$remove)
    after = betweenness(g1, weights = if (s$weighted) 1 / E(g1)$weight else NA)
    d = after - before[names(after)]
    i = order(-d, names(d))[1]
    rise_code = names(d)[i]; rise_value = unname(d[i])
  }
  list(
    top_code   = env$top$code[1],
    top_score  = env$top$score[1],
    k_codes    = paste(env$top$code, collapse = " "),
    apl        = env$apl,
    n_nodes    = vcount(env$g),
    n_edges    = ecount(env$g),
    rise_code  = rise_code,
    rise_value = rise_value
  )
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "edges", as.data.frame(edges))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$edges = edges; env$nodes = nodes
  eval(parse(text = code_r(s)), envir = env)                  # the panel's R, run for real
  code = list(r = code_r(s), sql = code_sql(s))
  via_sql = dbGetQuery(con, code$sql)                          # the panel's SQL, run for real
  g = env$g
  want = if (s$weighted) strength(g, weights = E(g)$weight) else degree(g)
  if (!isTRUE(all.equal(unname(want[via_sql$code]), as.numeric(if (s$weighted) via_sql$strength else via_sql$degree))))
    stop("prep: SQL and R disagree for state '", name, "'")
  list(name = name, state = s, readouts = readouts_from(env, s), code = code)
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
message("prep: wrote ", nodes_out, ", ", edges_out, " and ", golden_out)
