# v8b.R - prep for lab v8b "Small numbers, big swings" (built from _template.R).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v8b.R
# It writes:
#   docs-v3/labs/data/v8b.csv            one row per grid cell: cell, neighborhood, pop, sites
#   docs-v3/labs/data/v8b-grid.geojson   the 73 grid-cell polygons (5-decimal coordinates)
#   tests/labs/golden/v8b.json           named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
#
# Unit of analysis: the 1 km grid cells of boston_grid.geojson. The census table is matched to
# those cells (the block-group file carries no population), so the rate is social-infrastructure
# sites per 1,000 residents per cell, with residents = pop_density (per km2) x cell area (km2).

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
lab_id     = "v8b"
page       = "docs-v3/labs/v8b-small-numbers.html"
src_dir    = "data/boston_social_infra"
seed       = 5460                                          # no sampling here; kept for the template
data_out   = paste0("docs-v3/labs/data/", lab_id, ".csv")
grid_out   = paste0("docs-v3/labs/data/", lab_id, "-grid.geojson")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + REDUCE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
set.seed(seed)
sf_use_s2(FALSE)
grid   = read_sf(file.path(root, src_dir, "boston_grid.geojson")) %>% st_zm()
sites  = read_sf(file.path(root, src_dir, "boston_social_infra.geojson")) %>% st_zm()
census = read_csv(file.path(root, src_dir, "boston_census_data.csv"), show_col_types = FALSE)

cells = grid %>%
  mutate(area_km2 = as.numeric(st_area(st_transform(grid, 26986))) / 1e6,
         sites = lengths(st_intersects(grid, sites))) %>%
  st_drop_geometry() %>%
  left_join(census %>% select(cell, neighborhood, pop_density), by = "cell") %>%
  mutate(pop = round(pop_density * area_km2),
         num = as.integer(sub("Cell ", "", cell))) %>%
  arrange(num) %>%
  select(cell, neighborhood, pop, sites)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(cells, file.path(root, data_out), na = "")

# Grid polygons: plain JSON with 5-decimal coordinates so the bytes are stable across runs.
feats = lapply(seq_len(nrow(grid)), function(i) {
  ring = st_coordinates(grid$geometry[[i]])[, 1:2]
  # d3-geo wants clockwise exterior rings (the reverse of RFC 7946), else a cell covers the globe
  signed = sum(ring[-nrow(ring), 1] * ring[-1, 2] - ring[-1, 1] * ring[-nrow(ring), 2])
  if (signed > 0) ring = ring[nrow(ring):1, ]
  list(type = "Feature", properties = list(id = grid$cell[i]),
       geometry = list(type = "Polygon", coordinates = list(unname(round(ring, 5)))))
})
feats = feats[order(as.integer(sub("Cell ", "", grid$cell)))]
writeLines(toJSON(list(type = "FeatureCollection", features = feats), auto_unbox = TRUE, digits = NA),
           file.path(root, grid_out), useBytes = TRUE)
stopifnot(file.size(file.path(root, data_out)) + file.size(file.path(root, grid_out)) <= 500 * 1024)

# Re-read the file the page will fetch; the panel code calls this data `areas`.
areas = read_csv(file.path(root, data_out), show_col_types = FALSE)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline = list(metric = "raw",    min_pop = 1,    sort = "rate"),
  ci       = list(metric = "ci",     min_pop = 1,    sort = "rate"),
  shrunk   = list(metric = "shrunk", min_pop = 1,    sort = "rate"),
  min500   = list(metric = "raw",    min_pop = 500,  sort = "rate"),
  by_pop   = list(metric = "shrunk", min_pop = 2000, sort = "pop")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
code_r = function(s) {
  value = if (s$metric == "shrunk") "shrunk" else "rate"
  paste0(
    "# sites per 1,000 residents, with an exact Poisson 95% interval\n",
    "ranked = areas %>%\n",
    "  filter(pop >= ", s$min_pop, ") %>%\n",
    "  mutate(rate  = sites / pop * 1000,\n",
    "         lower = qchisq(0.025, 2 * sites) / 2 / pop * 1000,\n",
    "         upper = qchisq(0.975, 2 * (sites + 1)) / 2 / pop * 1000)\n",
    "\n",
    "# funnel plot: the city rate and its 95% / 99.8% control limits at each population\n",
    "m = sum(ranked$sites) / sum(ranked$pop)\n",
    "ranked = ranked %>%\n",
    "  mutate(hi95 = (m + 1.96 * sqrt(m / pop)) * 1000,\n",
    "         hi998 = (m + 3.09 * sqrt(m / pop)) * 1000)\n",
    if (s$metric == "shrunk") paste0(
      "\n",
      "# empirical Bayes: pull each rate toward m, harder when pop is small\n",
      "r = ranked$sites / ranked$pop\n",
      "s2 = sum(ranked$pop * (r - m)^2) / sum(ranked$pop)\n",
      "A = max(s2 - m / mean(ranked$pop), 0)\n",
      "ranked = ranked %>%\n",
      "  mutate(weight = A / (A + m / pop),\n",
      "         shrunk = (m + weight * (sites / pop - m)) * 1000)\n") else "",
    "\n",
    "leader = ranked$cell[which.max(ranked$rate)]\n",
    "ranked = ranked %>%\n",
    "  arrange(desc(", value, "))\n",
    "top10 = ranked %>%\n",
    "  arrange(desc(", if (s$sort == "pop") "pop" else value, ")) %>%\n",
    "  head(10)\n",
    "\n",
    "ranked %>%\n",
    "  summarize(n_areas = n(), city_rate = m * 1000,\n",
    "            top_cell = first(cell), top_value = first(", value, "), top_pop = first(pop),\n",
    "            leader_rank = which(cell == leader),\n",
    "            leader_lower = lower[cell == leader], leader_upper = upper[cell == leader],\n",
    "            n_above_998 = sum(rate > hi998))"
  )
}
code_sql = NULL

# The readouts the gate compares: the summarize() row of the panel code.
readouts_from = function(result) {
  as.list(result)
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$areas = areas
  result = eval(parse(text = code_r(s)), envir = env)        # the panel's R, run for real
  list(name = name, state = s, readouts = readouts_from(result), code = list(r = code_r(s)))
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
message("prep: wrote ", data_out, ", ", grid_out, " and ", golden_out)
