# v11a.R - Lab v11a: straight line vs street network - what can people reach?
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v11a.R
# Built from tools/labs/prep/_template.R. Running it twice leaves `git status` unchanged.
#
# Study area: Lower/Midtown Manhattan, the East River and the Brooklyn/Queens waterfront
# (lon -74.02 to -73.93, lat 40.69 to 40.75). Roads: data/congestion_pricing/roads.rds
# (TIGER classes S1100, S1200, S1400, S1630, S1640). Block groups: data/congestion_pricing/bg.geojson,
# population: data/social_infra/bg_data.rds (year 2022).
#
# It writes (all under docs-v3/labs/data/, <= 500 KB together):
#
#   v11a-nodes.csv   THE ROAD GRAPH, NODES. One row per intersection or dead end (degree != 2
#                    after merging shared TIGER vertices; chains of degree-2 vertices are contracted).
#                    node     integer, stable id 1..n (sorted by lon, then lat)
#                    lon, lat WGS84 degrees, 5 decimals
#
#   v11a-edges.csv   THE ROAD GRAPH, EDGES (undirected: TIGER carries no one-way flag).
#                    edge      integer, stable id 1..m (sorted by from, to, length)
#                    from, to  node ids (from < to)
#                    length_m  street length along the full TIGER geometry, metres (0.1 m)
#                    min_25kmh travel time in minutes at 25 km/h (= length_m / (25000 / 60))
#                    For another speed v km/h: minutes = length_m / (v * 1000 / 60).
#
#   v11a-bg.json     block groups in the study area: id, geoid, pop, area_km2, x, y (centroid,
#                    UTM 18N), node (nearest graph node), snap_m (centroid to that node, metres),
#                    net_m (network metres from each site's node + snap_m, keyed by site),
#                    and a simplified polygon ring (lon/lat, 4 decimals) for the map.
#   v11a-sites.json  the five preset origins (site, label, lon, lat, x, y, node). The page runs
#                    Dijkstra on nodes/edges itself for the motion (edges light up in distance order).

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

# ---- 1. CONFIG -----------------------------------------------------------------------------
lab_id     = "v11a"
page       = "docs-v3/labs/v11a-isochrones.html"
seed       = 5460
out_dir    = "docs-v3/labs/data/"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack        = list()
bbox       = c(xmin = -74.02, ymin = 40.69, xmax = -73.93, ymax = 40.75)
classes    = c("S1100", "S1200", "S1400", "S1630", "S1640")
sites_def  = tibble(
  site  = c("union_sq", "city_hall", "borough_hall", "williamsburg", "lic"),
  label = c("Union Square", "City Hall", "Borough Hall (Brooklyn)", "Williamsburg waterfront", "Long Island City"),
  lon   = c(-73.9903, -74.0060, -73.9903, -73.9640, -73.9570),
  lat   = c(40.7359, 40.7127, 40.6928, 40.7210, 40.7440)
)
set.seed(seed)
sf_use_s2(FALSE)

# ---- 2. ROAD GRAPH -------------------------------------------------------------------------
area = st_as_sfc(st_bbox(bbox, crs = 4326))
roads = readRDS(file.path(root, "data/congestion_pricing/roads.rds")) %>%
  filter(mtfcc %in% classes) %>%
  st_transform(4326)
roads = roads[st_intersects(roads, area, sparse = FALSE)[, 1], ] %>%
  arrange(linearid) %>%
  st_transform(32618)

# every vertex, keyed to 0.5 m so shared TIGER vertices become one node
v = st_coordinates(roads) %>% as_tibble() %>%
  transmute(x = X, y = Y, line = L1) %>%
  mutate(key = paste(round(x * 2), round(y * 2)))
vid = match(v$key, unique(v$key))
v$vid = vid
seg = v %>% group_by(line) %>%
  mutate(to_vid = lead(vid), to_x = lead(x), to_y = lead(y)) %>% ungroup() %>%
  filter(!is.na(to_vid), vid != to_vid) %>%
  mutate(len = sqrt((to_x - x)^2 + (to_y - y)^2), mtfcc = roads$mtfcc[line])
vxy = v %>% distinct(vid, .keep_all = TRUE) %>% arrange(vid)

gv = graph_from_data_frame(seg %>% transmute(from = vid, to = to_vid, len, mtfcc),
                           directed = FALSE, vertices = tibble(name = vxy$vid))
gv = simplify(gv, remove.multiple = TRUE, remove.loops = TRUE, edge.attr.comb = list(len = "min", mtfcc = "first"))
# keep the largest connected piece so every node is reachable
comp = components(gv)
gv = induced_subgraph(gv, which(comp$membership == which.max(comp$csize)))
deg = degree(gv)
key = which(deg != 2)

# contract chains of degree-2 vertices into single edges
adj = as_adj_list(gv, mode = "all")
inc = as_adj_edge_list(gv, mode = "all")
elen = E(gv)$len; ecls = E(gv)$mtfcc
seen = rep(FALSE, ecount(gv))
chains = list()
for (s in key) {
  for (k in seq_along(adj[[s]])) {
    e = inc[[s]][k]
    if (seen[e]) next
    seen[e] = TRUE; len = elen[e]; cls = ecls[e]; prev = s; cur = as.integer(adj[[s]][k])
    while (deg[cur] == 2) {
      nb = as.integer(adj[[cur]]); ed = as.integer(inc[[cur]])
      j = if (nb[1] == prev && ed[1] != e) 2 else if (nb[2] == prev && ed[2] != e) 1 else if (ed[1] == e) 2 else 1
      e = ed[j]; if (seen[e]) break
      seen[e] = TRUE; len = len + elen[e]; prev = cur; cur = nb[j]
    }
    chains[[length(chains) + 1]] = c(s, cur, len, match(cls, classes))
  }
}
ch = do.call(rbind, chains)
kxy = vxy[as.integer(V(gv)$name), ]
nodes = tibble(vg = key, x = kxy$x[key], y = kxy$y[key])
pts = st_as_sf(nodes, coords = c("x", "y"), crs = 32618) %>% st_transform(4326) %>% st_coordinates()
nodes = nodes %>% mutate(lon = round(pts[, 1], 5), lat = round(pts[, 2], 5), x = round(x, 1), y = round(y, 1)) %>%
  arrange(lon, lat) %>% mutate(node = row_number())
edges = tibble(a = nodes$node[match(ch[, 1], nodes$vg)], b = nodes$node[match(ch[, 2], nodes$vg)],
               length_m = round(ch[, 3], 1), mtfcc = classes[ch[, 4]]) %>%
  filter(a != b) %>%
  transmute(from = pmin(a, b), to = pmax(a, b), length_m, mtfcc) %>%
  arrange(from, to, length_m) %>% distinct(from, to, .keep_all = TRUE) %>%
  mutate(edge = row_number(), min_25kmh = round(length_m / (25000 / 60), 3)) %>%
  select(edge, from, to, length_m, min_25kmh)
node_xy = nodes %>% select(node, x, y)   # projected copy, used in R only

write_csv(nodes %>% select(node, lon, lat), file.path(root, out_dir, "v11a-nodes.csv"))
write_csv(edges, file.path(root, out_dir, "v11a-edges.csv"))
# re-read exactly what the page fetches
nodes = read_csv(file.path(root, out_dir, "v11a-nodes.csv"), show_col_types = FALSE) %>%
  left_join(node_xy, by = "node")
edges = read_csv(file.path(root, out_dir, "v11a-edges.csv"), show_col_types = FALSE)
g = graph_from_data_frame(edges %>% select(from, to, length_m), directed = FALSE,
                          vertices = nodes %>% transmute(name = node))

# ---- 3. SITES + BLOCK GROUPS ---------------------------------------------------------------
nearest_node = function(x, y) map2_int(x, y, function(a, b) nodes$node[which.min((nodes$x - a)^2 + (nodes$y - b)^2)])
sxy = st_as_sf(sites_def, coords = c("lon", "lat"), crs = 4326) %>% st_transform(32618) %>% st_coordinates()
sites = sites_def %>% mutate(x = round(sxy[, 1], 1), y = round(sxy[, 2], 1), node = nearest_node(x, y))

pop = readRDS(file.path(root, "data/social_infra/bg_data.rds")) %>%
  filter(year == 2022) %>% select(geoid, pop)
bg = st_read(file.path(root, "data/congestion_pricing/bg.geojson"), quiet = TRUE) %>%
  st_transform(4326)
bg = bg[st_intersects(st_centroid(bg), area, sparse = FALSE)[, 1], ] %>%
  inner_join(pop, by = "geoid") %>% filter(!is.na(pop), pop > 0) %>% arrange(geoid)
cen = bg %>% st_transform(32618) %>% st_centroid() %>% st_coordinates()
bg_tab = bg %>% st_drop_geometry() %>%
  mutate(id = row_number(), area_km2 = round(area_land / 1e6, 4),
         x = round(cen[, 1], 1), y = round(cen[, 2], 1), node = nearest_node(x, y))
bg_tab = bg_tab %>% mutate(snap_m = round(sqrt((x - nodes$x[node])^2 + (y - nodes$y[node])^2), 1))

net_all = distances(g, v = as.character(sites$node), to = V(g), weights = E(g)$length_m)
net_bg = map(seq_len(nrow(sites)), function(i) round(net_all[i, bg_tab$node] + bg_tab$snap_m, 1)) %>%
  set_names(sites$site)
shape = bg %>% st_transform(32618) %>% st_simplify(dTolerance = 20) %>% st_transform(4326)
rings = map(st_geometry(shape), function(gm) {
  cc = st_coordinates(gm); first = cc[cc[, "L1"] == 1 & cc[, ncol(cc) - 1] == 1 & cc[, ncol(cc)] == 1, 1:2, drop = FALSE]
  if (nrow(first) == 0) first = cc[, 1:2, drop = FALSE]
  round(unname(first), 4)
})
bg_out = map(seq_len(nrow(bg_tab)), function(i) {
  r = bg_tab[i, ]
  list(id = r$id, geoid = r$geoid, pop = r$pop, area_km2 = r$area_km2, x = r$x, y = r$y,
       node = r$node, snap_m = r$snap_m, net_m = map(net_bg, i), ring = rings[[i]])
})
write_json(bg_out, file.path(root, out_dir, "v11a-bg.json"), auto_unbox = TRUE, digits = NA)
write_json(sites, file.path(root, out_dir, "v11a-sites.json"), dataframe = "rows", auto_unbox = TRUE, digits = NA)
total = sum(file.size(file.path(root, out_dir, c("v11a-nodes.csv", "v11a-edges.csv", "v11a-bg.json", "v11a-sites.json"))))
message("prep: data total ", total, " B; nodes ", nrow(nodes), " edges ", nrow(edges), " bg ", nrow(bg_tab))
stopifnot(total <= 500 * 1024)

# ---- 4. STATES -----------------------------------------------------------------------------
# Same names and fields as `states` in the page's Lab.create(...). baseline comes first.
states = list(
  baseline = list(site = "union_sq",     mode = "network", budget = 10, speed = 25),
  buffer   = list(site = "union_sq",     mode = "buffer",  budget = 10, speed = 25),
  river    = list(site = "williamsburg", mode = "network", budget = 10, speed = 25),
  budget15 = list(site = "union_sq",     mode = "network", budget = 15, speed = 25),
  speed40  = list(site = "union_sq",     mode = "network", budget = 10, speed = 40)
)

# ---- 5. PANEL CODE -------------------------------------------------------------------------
# The EXACT text the page's code panel shows. It runs below on the published files, so the
# readouts can never disagree with the code a student copies. x, y are UTM 18N metres.
code_r = function(s) {
  paste0(
    "library(dplyr)\n",
    "library(igraph)\n\n",
    "site = sites %>% filter(site == \"", s$site, "\")\n",
    "reach_m = ", s$speed, " * 1000 / 60 * ", s$budget, "   # km/h to metres per minute, times minutes\n\n",
    "# Euclidean buffer: straight-line metres from the origin\n",
    "bg = bg %>% mutate(buffer_m = sqrt((x - site$x)^2 + (y - site$y)^2))\n\n",
    "# Network isochrone: shortest path along the streets, plus the walk to the nearest node\n",
    "g = graph_from_data_frame(edges %>% select(from, to, length_m), directed = FALSE,\n",
    "                          vertices = nodes %>% select(node))\n",
    "d = distances(g, v = as.character(site$node), weights = E(g)$length_m)\n",
    "bg = bg %>% mutate(network_m = round(d[1, as.character(node)] + snap_m, 1))\n\n",
    "reached = bg %>% filter(", if (s$mode == "buffer") "buffer_m" else "network_m", " <= reach_m)\n\n",
    "bg %>% summarize(\n",
    "  reach_m = reach_m,\n",
    "  pop_buffer = sum(pop[buffer_m <= reach_m]),\n",
    "  pop_network = sum(pop[network_m <= reach_m]),\n",
    "  overstated_pct = round(100 * (pop_buffer - pop_network) / pop_network, 1),\n",
    "  area_buffer_km2 = round(sum(area_km2[buffer_m <= reach_m]), 2),\n",
    "  area_network_km2 = round(sum(area_km2[network_m <= reach_m]), 2),\n",
    "  bg_reached = nrow(reached),\n",
    "  pop_reached = sum(reached$pop))"
  )
}
code_sql = NULL

readouts_from = function(result) as.list(result)

# ---- 6. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
# Re-read the published files, exactly as the page fetches them.
bg_pub = fromJSON(file.path(root, out_dir, "v11a-bg.json"))
bg_pub = tibble(id = bg_pub$id, pop = bg_pub$pop, area_km2 = bg_pub$area_km2, x = bg_pub$x, y = bg_pub$y,
                node = bg_pub$node, snap_m = bg_pub$snap_m)
sites_pub = as_tibble(fromJSON(file.path(root, out_dir, "v11a-sites.json")))
nodes_pub = read_csv(file.path(root, out_dir, "v11a-nodes.csv"), show_col_types = FALSE)
edges_pub = read_csv(file.path(root, out_dir, "v11a-edges.csv"), show_col_types = FALSE)

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$bg = bg_pub; env$sites = sites_pub; env$nodes = nodes_pub; env$edges = edges_pub
  result = eval(parse(text = code_r(s)), envir = env)
  list(name = name, state = s, readouts = readouts_from(result), code = list(r = code_r(s)))
})

# ---- 7. WRITE GOLDEN [KEEP] ----------------------------------------------------------------
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
message("prep: wrote ", golden_out)
