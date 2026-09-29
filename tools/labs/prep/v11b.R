# v11b.R - Lab v11b: catchment areas - nearest by straight line (Voronoi) or by travel time
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v11b.R
# Built from tools/labs/prep/_template.R. Running it twice leaves `git status` unchanged.
#
# Reads the street-network lab's published files (never rewrites them):
#   docs-v3/labs/data/v11a-bg.json     531 block groups: id, pop, x, y (UTM 18N centroid), net_m (network
#                                      metres to each site, incl. the snap to the nearest node), ring (map)
#   docs-v3/labs/data/v11a-sites.json  the five facilities: site, label, lon, lat, x, y, node
#
# It writes (under docs-v3/labs/data/):
#   v11b-tt.csv   THE TRAVEL-TIME MATRIX. One row per block group (id), one column per facility,
#                 minutes along the streets at 25 km/h (= net_m / (25000 / 60)), 2 decimals.
#   tests/labs/golden/v11b.json   named states, readouts, code.

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

# ---- 1. CONFIG -----------------------------------------------------------------------------
lab_id     = "v11b"
page       = "docs-v3/labs/v11b-catchments.html"
seed       = 5460
out_dir    = "docs-v3/labs/data/"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack        = list()
speed_kmh  = 25
cap        = 160000   # people one facility can serve when the capacity cap is on

# ---- 2. READ THE DONOR FILES ---------------------------------------------------------------
set.seed(seed)
bg_raw = fromJSON(file.path(root, out_dir, "v11a-bg.json"))
sites_raw = as_tibble(fromJSON(file.path(root, out_dir, "v11a-sites.json")))

# ---- 3. WRITE THE TRAVEL-TIME MATRIX -------------------------------------------------------
tt_out = bind_cols(tibble(id = bg_raw$id),
                   as_tibble(round(as.matrix(bg_raw$net_m[, sites_raw$site]) / (speed_kmh * 1000 / 60), 2))) %>%
  arrange(id)
write_csv(tt_out, file.path(root, out_dir, "v11b-tt.csv"))
total = file.size(file.path(root, out_dir, "v11b-tt.csv"))
message("prep: v11b-tt.csv ", total, " B; bg ", nrow(tt_out))
stopifnot(total <= 500 * 1024)

# ---- 4. STATES -----------------------------------------------------------------------------
# Same names and fields as `states` in the page's Lab.create(...). baseline comes first.
states = list(
  baseline      = list(rule = "voronoi", closed = "none",      cap = "off"),
  travel        = list(rule = "travel",  closed = "none",      cap = "off"),
  closed        = list(rule = "voronoi", closed = "city_hall", cap = "off"),
  closed_travel = list(rule = "travel",  closed = "city_hall", cap = "off"),
  cap           = list(rule = "voronoi", closed = "none",      cap = "on"),
  cap_travel    = list(rule = "travel",  closed = "city_hall", cap = "on")
)

# ---- 5. PANEL CODE -------------------------------------------------------------------------
# The EXACT text the page's code panel shows; it runs below on the published files.
code_r = function(s) {
  cost = if (s$rule == "voronoi") "voronoi_m" else "travel_min"
  paste0(
    "library(dplyr)\n",
    "library(sf)\n\n",
    "open = sites %>% filter(site != \"", s$closed, "\")   # facilities still open\n",
    "bg_sf = st_as_sf(bg, coords = c(\"x\", \"y\"), crs = 32618)\n",
    "open_sf = st_as_sf(open, coords = c(\"x\", \"y\"), crs = 32618)\n\n",
    "# Straight line (Voronoi): the nearest facility as the crow flies\n",
    "near = st_nearest_feature(bg_sf, open_sf)\n",
    "bg$voronoi = open$site[near]\n",
    "bg$voronoi_m = as.numeric(st_distance(bg_sf, open_sf[near, ], by_element = TRUE))\n\n",
    "# Travel time: the smallest minutes in each row of the travel-time matrix\n",
    "m = as.matrix(tt[, open$site])\n",
    "bg$travel = open$site[apply(m, 1, which.min)]\n",
    "bg$travel_min = apply(m, 1, min)\n\n",
    "bg = bg %>%\n",
    "  mutate(site = ", if (s$rule == "voronoi") "voronoi" else "travel", ", cost = ", cost, ") %>%\n",
    "  group_by(site) %>%\n",
    "  arrange(cost, id, .by_group = TRUE) %>%\n",
    "  mutate(served = ", if (s$cap == "on") paste0("cumsum(pop) <= ", format(cap, scientific = FALSE)) else "TRUE",
    ") %>%   # nearest demand first\n",
    "  ungroup()\n\n",
    "loads = bg %>% filter(served) %>% count(site, wt = pop, name = \"load\") %>% arrange(desc(load))\n",
    "list(\n",
    "  top_site = loads$site[1],\n",
    "  top_load = loads$load[1],\n",
    "  bg_changed = sum(bg$voronoi != bg$travel),\n",
    "  pop_changed = sum(bg$pop[bg$voronoi != bg$travel]),\n",
    "  unserved = sum(bg$pop[!bg$served]),\n",
    "  open_sites = nrow(open))"
  )
}
code_sql = NULL

readouts_from = function(result) as.list(result)

# ---- 6. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
bg_pub = fromJSON(file.path(root, out_dir, "v11a-bg.json"))
bg_pub = tibble(id = bg_pub$id, pop = bg_pub$pop, x = bg_pub$x, y = bg_pub$y) %>% arrange(id)
sites_pub = as_tibble(fromJSON(file.path(root, out_dir, "v11a-sites.json")))
tt_pub = read_csv(file.path(root, out_dir, "v11b-tt.csv"), show_col_types = FALSE) %>% arrange(id)
stopifnot(all(tt_pub$id == bg_pub$id))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$bg = bg_pub; env$sites = sites_pub; env$tt = tt_pub
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
