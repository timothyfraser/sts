# v7c.R - prep script for lab v7c: neighbours, spatial lag and Moran's I.
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v7c.R
# It writes:
#   docs-v3/labs/data/v7c.json        Gunma's 35 municipalities (simplified outlines), two
#                                     variables, the neighbour list for every rule, and the
#                                     seeded permutation band for every rule x variable
#   tests/labs/golden/v7c.json        named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# Built from tools/labs/prep/_template.R. Neighbour lists are computed on the FULL-resolution
# outlines (the panel code runs on those); the page draws simplified outlines only.

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(sf)
library(spdep)

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
lab_id     = "v7c"
page       = "docs-v3/labs/v7c-neighbours-and-spatial-lag.html"
source_geo = "data/japan/municipalities.geojson"
source_csv = "data/jp_solar_farms_2018.csv"
pref_code  = "10"                                          # Gunma: landlocked, no islands
seed       = 5460                                          # never change after first publish
nsim       = 199
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD [CHANGE] ----------------------------------------------------------------------
solar = read_csv(file.path(root, source_csv), show_col_types = FALSE) %>%
  select(muni_code, sp, pv_output_2018)

gunma = read_sf(file.path(root, source_geo)) %>%
  filter(pref_code == !!pref_code) %>%
  select(muni_code, muni) %>%
  left_join(by = "muni_code", y = solar) %>%
  arrange(muni_code)

# Centroids (lon/lat) for k-nearest and distance-band rules, rounded so the page and R agree.
cent = suppressWarnings(st_centroid(st_geometry(gunma))) %>% st_coordinates()
gunma = gunma %>% mutate(lon = round(cent[, 1], 4), lat = round(cent[, 2], 4))
stopifnot(!anyNA(gunma$sp), !anyNA(gunma$pv_output_2018))

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
rules = c("queen", "rook", "knn3", "knn6", "d20")
vars  = c("sp", "pv_output_2018")
states = list(
  baseline = list(rule = "queen", var = "sp", std = FALSE, sel = "10201"),
  rook     = list(rule = "rook",  var = "sp", std = FALSE, sel = "10201"),
  knn3     = list(rule = "knn3",  var = "sp", std = FALSE, sel = "10201"),
  knn6     = list(rule = "knn6",  var = "sp", std = FALSE, sel = "10201"),
  d20      = list(rule = "d20",   var = "sp", std = FALSE, sel = "10201"),
  pv       = list(rule = "queen", var = "pv_output_2018", std = FALSE, sel = "10201"),
  std      = list(rule = "queen", var = "sp", std = TRUE,  sel = "10201"),
  other    = list(rule = "queen", var = "sp", std = FALSE, sel = "10204")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
nb_line = function(rule) {
  switch(rule,
    queen = "nb = poly2nb(gunma, queen = TRUE)",
    rook  = "nb = poly2nb(gunma, queen = FALSE)",
    knn3  = "nb = knearneigh(coords, k = 3, longlat = TRUE) %>% knn2nb()",
    knn6  = "nb = knearneigh(coords, k = 6, longlat = TRUE) %>% knn2nb()",
    d20   = "nb = dnearneigh(coords, d1 = 0, d2 = 20, longlat = TRUE)"
  )
}
code_r = function(s) {
  xexpr = if (isTRUE(s$std)) paste0("as.numeric(scale(", s$var, "))") else s$var
  paste0(
    "coords = cbind(gunma$lon, gunma$lat)\n",
    nb_line(s$rule), "\n",
    "w = nb2listw(nb, style = \"W\")\n\n",
    "gunma = gunma %>%\n",
    "  mutate(x = ", xexpr, ",\n",
    "         adj_sp = lag.listw(w, x))\n\n",
    "mt = moran.test(gunma$x, w)\n",
    "set.seed(", seed, ")\n",
    "mc = moran.mc(gunma$x, w, nsim = ", nsim, ")\n",
    "band = quantile(mc$res[1:", nsim, "], probs = c(0.025, 0.975))\n\n",
    "gunma %>%\n",
    "  filter(muni_code == \"", s$sel, "\") %>%\n",
    "  select(muni, x, adj_sp)"
  )
}

# The readouts the gate compares, taken from the environment the panel code ran in.
readouts_from = function(env, s) {
  i = which(env$gunma$muni_code == s$sel)
  list(
    n_neighbours = length(env$nb[[i]][env$nb[[i]] > 0]),
    x_sel        = env$gunma$x[i],
    lag_sel      = env$gunma$adj_sp[i],
    morans_i     = unname(env$mt$estimate[1]),
    band_lo      = unname(env$band[1]),
    band_hi      = unname(env$band[2]),
    mean_links   = mean(card(env$nb))
  )
}

# ---- 5. RUN + CHECK EACH STATE [KEEP, adapted: env-based readouts] -------------------------
run_state = function(s) {
  env = new.env(parent = globalenv())
  env$gunma = gunma
  eval(parse(text = code_r(s)), envir = env)
  env
}
golden_states = imap(states, function(s, name) {
  env = run_state(s)
  list(name = name, state = s, readouts = readouts_from(env, s), code = list(r = code_r(s)))
})

# ---- 6. PAGE DATA [CHANGE] -----------------------------------------------------------------
# One neighbour list per rule (0-based indices) and one band per rule x variable, from the SAME
# code the panel shows (std is irrelevant to I and to its permutation band).
nbs = map(set_names(rules), function(r) {
  env = run_state(list(rule = r, var = "sp", std = FALSE, sel = "10201"))
  map(env$nb, function(v) I(as.integer(v[v > 0] - 1L)))   # I(): keep 1-neighbour lists as arrays
})
bands = map(set_names(rules), function(r) map(set_names(vars), function(v) {
  env = run_state(list(rule = r, var = v, std = FALSE, sel = "10201"))
  round(unname(as.numeric(env$band)), 10)
}))

outline = gunma %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 250) %>%
  st_geometry()
features = map(seq_len(nrow(gunma)), function(i) {
  g = st_coordinates(outline[[i]])
  cols = intersect(c("L1", "L2", "L3"), colnames(g))
  key = interaction(as.data.frame(g[, cols, drop = FALSE]), drop = TRUE, lex.order = TRUE)
  rings = unname(split(as.data.frame(round(g[, c("X", "Y")], 4)), key))
  rings = map(rings, function(r) unname(as.matrix(r)))
  list(type = "Feature",
       properties = list(id = gunma$muni_code[i], muni = gunma$muni[i]),
       geometry = list(type = "MultiPolygon", coordinates = map(rings, function(r) list(r))))
})
page_data = list(
  units = map(seq_len(nrow(gunma)), function(i) list(
    muni_code = gunma$muni_code[i], muni = gunma$muni[i],
    sp = gunma$sp[i], pv_output_2018 = gunma$pv_output_2018[i],
    lon = gunma$lon[i], lat = gunma$lat[i])),
  nb = nbs, band = bands,
  geo = list(type = "FeatureCollection", features = features)
)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(page_data, file.path(root, data_out), auto_unbox = TRUE, digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

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
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)
