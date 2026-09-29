# v3b.R - prep script for lab v3b (groups, summaries and lags on Japanese municipal solar data).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v3b.R
# It writes:
#   docs-v3/labs/data/v3b.csv        8 municipalities x 12 observations (seeded sample)
#   tests/labs/golden/v3b.json       named states, readouts, code, acks (the gate's truth)
# Running it twice leaves `git status` unchanged (byte-for-byte output).
# Built from tools/labs/prep/_template.R: only the [CHANGE] blocks differ.

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

# ---- 1. CONFIG [CHANGE] --------------------------------------------------------------------
lab_id    = "v3b"
page      = "docs-v3/labs/v3b-groups-and-windows.html"
source    = "data/jp_solar.csv"
seed      = 5460
n_munis   = 8
n_obs     = 12
data_out  = paste0("docs-v3/labs/data/", lab_id, ".csv")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + SAMPLE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
# 8 municipalities, their 12 latest observations each. Rows stay together by municipality,
# but are stored in a shuffled date order inside each one, so "order_by off" really is unordered.
set.seed(seed)
raw = read_csv(file.path(root, source), show_col_types = FALSE,
               col_types = cols(muni_code = col_character(), date = col_date()))
munis = raw %>% distinct(muni_code) %>% arrange(muni_code) %>% slice_sample(n = n_munis) %>% arrange(muni_code)
sample_df = raw %>%
  filter(muni_code %in% munis$muni_code) %>%
  group_by(muni_code) %>%
  slice_max(date, n = n_obs) %>%
  slice_sample(prop = 1) %>%
  ungroup() %>%
  arrange(muni_code) %>%
  select(muni_code, date, solar)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(sample_df, file.path(root, data_out), na = "")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

jp_solar = read_csv(file.path(root, data_out), show_col_types = FALSE,
                    col_types = cols(muni_code = col_character(), date = col_date(), solar = col_double()))

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
st = function(group, verb, fn = "lag", n = 1, order = TRUE) list(group = group, verb = verb, fn = fn, n = n, order = order)
states = list(
  baseline  = st(TRUE,  "summarize"),
  ungrouped_summary = st(FALSE, "summarize"),
  reframe   = st(TRUE,  "reframe"),
  lag1      = st(TRUE,  "mutate", "lag", 1),
  lag1_leak = st(FALSE, "mutate", "lag", 1),
  lag3_leak = st(FALSE, "mutate", "lag", 3),
  unordered = st(TRUE,  "mutate", "lag", 1, FALSE),
  cumsum    = st(TRUE,  "mutate", "cumsum"),
  cumsum_leak = st(FALSE, "mutate", "cumsum"),
  rolling   = st(TRUE,  "mutate", "rolling")
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
new_col = function(s) switch(s$verb, summarize = "mean_solar", reframe = "solar",
                             mutate = switch(s$fn, lag = "solar_lag", cumsum = "solar_cumsum", rolling = "solar_roll3"))
window_r = function(s) switch(s$fn,
  lag = paste0("lag(solar, n = ", s$n, ")"),
  cumsum = "cumsum(solar)",
  rolling = "(solar + lag(solar, n = 1) + lag(solar, n = 2)) / 3")
code_r = function(s) {
  verb = switch(s$verb,
    summarize = "summarize(mean_solar = mean(solar))",
    reframe = "reframe(stat = c(\"min\", \"mean\", \"max\"),\n          solar = c(min(solar), mean(solar), max(solar)))",
    mutate = paste0("mutate(", new_col(s), " = ", window_r(s), ")"))
  paste0(
    "result = jp_solar %>%\n",
    if (s$order) "  arrange(muni_code, date) %>%\n" else "",
    if (s$group) "  group_by(muni_code) %>%\n" else "",
    "  ", verb,
    if (s$group && s$verb == "mutate") " %>%\n  ungroup()" else ""
  )
}
code_sql = function(s) {
  by = if (s$group) "muni_code, " else ""
  gb = if (s$group) "\nGROUP BY muni_code" else ""
  if (s$verb == "summarize")
    return(paste0("SELECT ", by, "AVG(solar) AS mean_solar\nFROM jp_solar", gb, ";"))
  if (s$verb == "reframe")
    return(paste0(
      "SELECT ", by, "'min' AS stat, MIN(solar) AS solar FROM jp_solar", gb, "\n",
      "UNION ALL\n",
      "SELECT ", by, "'mean', AVG(solar) FROM jp_solar", gb, "\n",
      "UNION ALL\n",
      "SELECT ", by, "'max', MAX(solar) FROM jp_solar", gb, ";"))
  w = paste0(
    if (s$group) "PARTITION BY muni_code" else "",
    if (s$group && s$order) " " else "",
    if (s$order) paste0("ORDER BY ", if (s$group) "" else "muni_code, ", "date") else "")
  expr = switch(s$fn,
    lag = paste0("LAG(solar, ", s$n, ") OVER w"),
    cumsum = "SUM(solar) OVER w",
    rolling = "(solar + LAG(solar, 1) OVER w + LAG(solar, 2) OVER w) / 3.0")
  paste0("SELECT *,\n  ", expr, " AS ", new_col(s), "\nFROM jp_solar\nWINDOW w AS (", w, ");")
}

# Leaks: rows whose window value draws on a row from ANOTHER municipality. Computed by running
# the same window over the municipality code instead of solar.
leaks_for = function(s) {
  if (s$verb != "mutate") return(0)
  d = jp_solar
  if (s$order) d = d %>% arrange(muni_code, date)
  if (s$group) return(0)
  m = d$muni_code
  src = function(k) lag(m, n = k)
  bad = switch(s$fn,
    lag = !is.na(src(s$n)) & src(s$n) != m,
    cumsum = m != m[1],
    rolling = (!is.na(src(1)) & src(1) != m) | (!is.na(src(2)) & src(2) != m))
  sum(bad)
}

readouts_from = function(result, s) {
  v = result[[new_col(s)]]
  list(rows_out = nrow(result), value_sum = sum(v, na.rm = TRUE), na_out = sum(is.na(v)), leaks = leaks_for(s))
}

# ---- 5. RUN + CHECK EACH STATE [KEEP, SQL check adapted] -----------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "jp_solar", as.data.frame(jp_solar %>% mutate(date = format(date))))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$jp_solar = jp_solar
  eval(parse(text = code_r(s)), envir = env)
  result = env$result
  code = list(r = code_r(s), sql = code_sql(s))
  # SQL has no row order without ORDER BY, so only ordered (or non-window) states are checked.
  if (s$order || s$verb != "mutate") {
    via_sql = dbGetQuery(con, code$sql)
    rv = result[[new_col(s)]]; sv = via_sql[[new_col(s)]]
    if (nrow(via_sql) != nrow(result) || abs(sum(rv, na.rm = TRUE) - sum(sv, na.rm = TRUE)) > 1e-9 ||
        sum(is.na(rv)) != sum(is.na(sv)))
      stop("prep: SQL and R disagree for state '", name, "'")
  }
  list(name = name, state = s, readouts = readouts_from(result, s), code = code)
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
