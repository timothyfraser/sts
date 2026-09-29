# v9c.R - prep for the lab "Sampling a network: what your sample hides".
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v9c.R
# It writes:
#   docs-v3/labs/data/v9c.json          nodes (x, y, degree), edges, and every precomputed sample
#   tests/labs/golden/v9c.json          named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
# Built from tools/labs/prep/_template.R; only the [CHANGE] blocks differ.

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(sf)
library(igraph)

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
lab_id     = "v9c"
page       = "docs-v3/labs/v9c-sampling-networks.html"
seed       = 5460                                          # not used for samples; each sample sets its own seed
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack        = list()
fractions  = c(0.1, 0.2, 0.3, 0.5)
seeds      = c(1, 2, 3)
methods    = c("node", "edge", "snowball")
slice_time = "2019-08-30 08:00:00"                           # one time slice, as in the workshop

# ---- 2. LOAD [CHANGE] ----------------------------------------------------------------------
# The workshop's network: evacuation flows (evacuation > 0) in one time slice.
# For degree and density we treat it as undirected and simple: one edge per county pair.
points = read_sf(file.path(root, "data/evacuation/county_subdivisions.geojson")) %>%
  mutate(geometry %>% st_centroid() %>% st_coordinates() %>%
           as_tibble() %>% select(x = 1, y = 2)) %>%
  as_tibble() %>%
  select(geoid, x, y)

nodes = read_rds(file.path(root, "data/evacuation/nodes.rds")) %>%
  select(node, geoid) %>%
  left_join(by = "geoid", y = points) %>%
  mutate(x = round(x, 4), y = round(y, 4)) %>%
  arrange(node)

edges = read_rds(file.path(root, "data/evacuation/edges.rds")) %>%
  select(from, to, date_time, evacuation) %>%
  filter(evacuation > 0) %>%
  filter(date_time == as.POSIXct(slice_time, tz = "UTC")) %>%
  filter(from != to) %>%
  mutate(a = pmin(from, to), b = pmax(from, to)) %>%
  distinct(a, b) %>%
  select(from = a, to = b) %>%
  arrange(from, to)

g_pop = graph_from_data_frame(edges, directed = FALSE, vertices = nodes %>% select(node))
nodes = nodes %>% mutate(degree = as.numeric(degree(g_pop)))

# The marked county for snowball sampling: the median-degree node with the lowest id among
# connected nodes, so the snowball starts from an ordinary county, not a hub.
marked = nodes %>% filter(degree > 0) %>%
  filter(degree == sort(degree)[ceiling(n() / 2)]) %>%
  slice_head(n = 1) %>% pull(node)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline = list(method = "node",     frac = 0.2, seed = 1),
  edge     = list(method = "edge",     frac = 0.2, seed = 1),
  snowball = list(method = "snowball", frac = 0.2, seed = 1)
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
summary_code = paste0(
  "\n\n",
  "stats = tibble(n_nodes = nrow(sample_nodes), n_edges = nrow(sample_edges)) %>%\n",
  "  mutate(mean_degree = 2 * n_edges / n_nodes,\n",
  "         density = 2 * n_edges / (n_nodes * (n_nodes - 1)))"
)
code_r = function(s) {
  body = if (s$method == "node") {
    paste0(
      "set.seed(", s$seed, ")\n",
      "sample_nodes = nodes %>%\n",
      "  slice_sample(prop = ", s$frac, ")\n",
      "sample_edges = edges %>%\n",
      "  filter(from %in% sample_nodes$node, to %in% sample_nodes$node)"
    )
  } else if (s$method == "edge") {
    paste0(
      "set.seed(", s$seed, ")\n",
      "sample_edges = edges %>%\n",
      "  slice_sample(prop = ", s$frac, ")\n",
      "sample_nodes = nodes %>%\n",
      "  filter(node %in% c(sample_edges$from, sample_edges$to))"
    )
  } else {
    paste0(
      "g = graph_from_data_frame(edges, directed = FALSE, vertices = nodes)\n",
      "wave = ego(g, order = 427, nodes = \"", marked, "\")[[1]]\n",
      "sample_nodes = tibble(node = as.numeric(names(wave))) %>%\n",
      "  slice_head(n = round(", s$frac, " * nrow(nodes)))\n",
      "sample_edges = edges %>%\n",
      "  filter(from %in% sample_nodes$node, to %in% sample_nodes$node)"
    )
  }
  paste0(body, summary_code)
}
code_sql = NULL

run_code = function(s) {
  env = new.env(parent = globalenv())
  env$nodes = nodes %>% select(node, geoid)
  env$edges = edges
  eval(parse(text = code_r(s)), envir = env)
  env
}

pop_mean_degree = 2 * nrow(edges) / nrow(nodes)
pop_density = 2 * nrow(edges) / (nrow(nodes) * (nrow(nodes) - 1))
readouts_from = function(env) {
  st = env$stats
  list(
    n_nodes = st$n_nodes, n_edges = st$n_edges,
    mean_degree = st$mean_degree, density = st$density,
    pop_mean_degree = pop_mean_degree, pop_density = pop_density,
    bias_degree_pct = 100 * (st$mean_degree - pop_mean_degree) / pop_mean_degree,
    bias_density_pct = 100 * (st$density - pop_density) / pop_density
  )
}

# ---- 5. DATA FILE: every sample the controls can reach [CHANGE] ----------------------------
edge_key = paste(edges$from, edges$to)
grid = expand.grid(seed = seeds, frac = fractions, method = methods, stringsAsFactors = FALSE) %>%
  filter(method != "snowball" | seed == 1)
samples = pmap(grid, function(seed, frac, method) {
  env = run_code(list(method = method, frac = frac, seed = seed))
  list(key = paste(method, frac, seed, sep = "|"),
       nodes = as.integer(env$sample_nodes$node),
       edges = as.integer(match(paste(env$sample_edges$from, env$sample_edges$to), edge_key) - 1))
})
out = list(
  marked = marked, slice = slice_time,
  nodes = nodes %>% select(node, x, y, degree),
  edges = edges,
  samples = samples
)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
# One county subdivision has no polygon, so no centroid: it stays in the network (it has a degree)
# and is written with null coordinates; the page simply does not draw it.
write_json(out, file.path(root, data_out), auto_unbox = TRUE, digits = NA, dataframe = "columns", na = "null")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# ---- 6. RUN EACH STATE + WRITE GOLDEN [KEEP] -----------------------------------------------
golden_states = imap(states, function(s, name) {
  env = run_code(s)
  list(name = name, state = s, readouts = readouts_from(env), code = list(r = code_r(s)))
})
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
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out,
        "; marked = ", marked, "; pop mean degree = ", pop_mean_degree)
