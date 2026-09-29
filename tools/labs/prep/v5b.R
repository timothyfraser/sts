# v5b.R - prep script for lab v5b "Sampling from a big table: the sampling distribution".
# Copied from _template.R; [CHANGE] blocks edited, [KEEP] blocks unchanged in spirit.
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v5b.R
# It writes:
#   docs-v3/labs/data/v5b.json          population histogram + every sample statistic the page shows
#   tests/labs/golden/v5b.json          named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)

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
setwd(root)   # the panel code reads "data/flights.csv" relative to the repo root

# ---- 1. CONFIG [CHANGE] --------------------------------------------------------------------
lab_id     = "v5b"
page       = "docs-v3/labs/v5b-sample-a-big-table.html"
source     = "data/flights.csv"
seed       = 5460                     # never change after first publish
sizes      = c(5, 25, 100, 400)       # sample size n (control)
max_reps   = 1000                     # number of samples; 100 is the first 100 of these 1000
n_flash    = 3                        # draws kept per (n, replace) for the flashing motion
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. PANEL CODE [CHANGE the templates] --------------------------------------------------
# The page's code.r / code.sql build these same strings.
code_r = function(s) {
  paste0(
    "library(dplyr)\n",
    "library(readr)\n\n",
    "flights = read_csv(\"data/flights.csv\") %>%\n",
    "  select(dep_delay) %>%\n",
    "  filter(!is.na(dep_delay))\n\n",
    "set.seed(", seed, ")\n",
    "stats = replicate(n = ", s$reps, ", expr = {\n",
    "  flights %>%\n",
    "    slice_sample(n = ", s$n, ", replace = ", if (s$replace) "TRUE" else "FALSE", ") %>%\n",
    "    summarize(stat = ", s$stat, "(dep_delay)) %>%\n",
    "    pull(stat)\n",
    "})\n\n",
    "tibble(stat = stats) %>%\n",
    "  summarize(samples = n(), center = mean(stat),\n",
    "            se_observed = sd(stat)",
    if (s$stat == "mean") paste0(",\n            se_formula = sd(flights$dep_delay) / sqrt(", s$n, "))")
    else ")\n# sd/sqrt(n) is the SE of the mean only: no formula for the median"
  )
}
code_sql = function(s) {
  paste0(
    "-- one sample of ", s$n, " flights ", if (s$replace) "(SQL draws without replacement; R's replace = TRUE has no LIMIT twin)" else "without replacement", "\n",
    "SELECT ", if (s$stat == "mean") "AVG(dep_delay) AS stat" else "dep_delay  -- no MEDIAN() in SQLite: take the middle row in R", "\n",
    "FROM (\n",
    "  SELECT dep_delay FROM flights\n",
    "  WHERE dep_delay IS NOT NULL\n",
    "  ORDER BY random()\n",
    "  LIMIT ", s$n, "\n",
    ");\n",
    "-- PostgreSQL shortcut: FROM flights TABLESAMPLE SYSTEM (1)\n",
    "-- repeat ", s$reps, " times, then take sd() of the ", s$reps, " stats"
  )
}
readouts_from = function(result) as.list(result)

run_panel = function(s) {
  env = new.env(parent = globalenv())
  result = eval(parse(text = code_r(s)), envir = env)
  list(result = result, stats = env$stats, flights = env$flights)
}

# ---- 3. DATA FILE [CHANGE] -----------------------------------------------------------------
# Every combination of the controls, each produced by running the panel code itself.
grid = expand.grid(n = sizes, replace = c(FALSE, TRUE), stat = c("mean", "median"),
                   stringsAsFactors = FALSE)
runs = pmap(grid, function(n, replace, stat) {
  s = list(n = n, reps = max_reps, replace = replace, stat = stat)
  out = run_panel(s)
  # integers keep the file small and exact: mean * n and median * 2 are whole minutes
  k = if (stat == "mean") n else 2
  v = round(out$stats * k)
  stopifnot(isTRUE(all.equal(v / k, out$stats)))
  list(key = paste(n, if (replace) "r" else "w", stat, sep = "-"), k = k, v = v)
})
flights = run_panel(list(n = 5, reps = 1, replace = FALSE, stat = "mean"))$flights

# The first draws (the rows themselves) for the flash motion: same seed, same stream.
draws = pmap(unique(grid[, c("n", "replace")]), function(n, replace) {
  set.seed(seed)
  d = replicate(n = n_flash, expr = {
    flights %>% slice_sample(n = n, replace = replace) %>% pull(dep_delay)
  }, simplify = FALSE)
  list(key = paste(n, if (replace) "r" else "w", sep = "-"), draws = d)
})

# Population histogram, 10-minute bins; the long right tail is folded into one last bin.
lo = -50; hi = 300; w = 10
pop = flights %>%
  mutate(bin = pmin(pmax(floor(dep_delay / w) * w, lo), hi)) %>%
  count(bin) %>%
  arrange(bin)

page_data = list(
  rows_total = nrow(read_csv(source, show_col_types = FALSE)),
  rows = nrow(flights),
  pop_mean = mean(flights$dep_delay),
  pop_median = median(flights$dep_delay),
  pop_sd = sd(flights$dep_delay),
  pop_max = max(flights$dep_delay),
  bin_width = w, bin_lo = lo, bin_hi = hi,
  hist = list(bin = pop$bin, count = pop$n),
  sizes = sizes, max_reps = max_reps,
  stats = set_names(map(runs, function(r) list(k = r$k, v = r$v)), map_chr(runs, "key")),
  draws = set_names(map(draws, "draws"), map_chr(draws, "key"))
)
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(page_data, file.path(root, data_out), auto_unbox = TRUE, digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# ---- 4. STATES [CHANGE] --------------------------------------------------------------------
states = list(
  baseline  = list(n = 25,  reps = 1000, replace = FALSE, stat = "mean"),
  quadruple = list(n = 100, reps = 1000, replace = FALSE, stat = "mean"),
  tiny      = list(n = 5,   reps = 1000, replace = FALSE, stat = "mean"),
  few       = list(n = 25,  reps = 100,  replace = FALSE, stat = "mean"),
  replace   = list(n = 25,  reps = 1000, replace = TRUE,  stat = "mean"),
  median    = list(n = 25,  reps = 1000, replace = FALSE, stat = "median")
)

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "flights", as.data.frame(flights))
golden_states = imap(states, function(s, name) {
  out = run_panel(s)
  code = list(r = code_r(s), sql = code_sql(s))
  via_sql = dbGetQuery(con, code$sql)   # the panel's SQL, run for real (one random sample)
  if (nrow(via_sql) != if (s$stat == "mean") 1 else s$n) stop("prep: SQL returned the wrong shape for '", name, "'")
  list(name = name, state = s, readouts = readouts_from(out$result), code = code)
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
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)
