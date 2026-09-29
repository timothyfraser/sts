# v4a.R - prep script for lab v4a (Anatomy of an API call: React -> Plumber -> R -> JSON).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v4a.R
# It writes:
#   docs-v3/labs/data/v4a.json      one record per request (origin x month x stat x mode):
#                                   rows matched, rows + bytes of the JSON payload, a preview
#   tests/labs/golden/v4a.json      named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
# Built from _template.R: only the [CHANGE] blocks differ.

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)

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
lab_id     = "v4a"
page       = "docs-v3/labs/v4a-api-anatomy.html"
source     = "data/flights.csv"                           # full file: every row, no sample
seed       = 5460
data_out   = paste0("docs-v3/labs/data/", lab_id, ".json")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

origins = c("EWR", "JFK", "LGA")
months  = 1:12
stats   = c("mean_arr_delay", "pct_late", "n_flights")
modes   = c("server", "raw")

# Modeled timing (not measured): a fixed round trip, R work that grows with rows scanned,
# and transfer at about 5 MB/s. The page shows these as "modeled ms".
ms_network = 25
ms_r = function(rows_matched) 60 + round(rows_matched / 200)
ms_transfer = function(bytes) round(bytes / 5000)

# ---- 2. LOAD [CHANGE] ----------------------------------------------------------------------
set.seed(seed)
flights = read_csv(file.path(root, source), show_col_types = FALSE) %>%
  select(month, day, carrier, origin, arr_delay)

# ---- 3. PANEL CODE [CHANGE] ----------------------------------------------------------------
# core_r() is the R function inside plumber.R; it is evaluated for real below.
stat_expr = function(stat) {
  switch(stat,
    mean_arr_delay = "mean(arr_delay, na.rm = TRUE)",
    pct_late       = "mean(arr_delay > 15, na.rm = TRUE) * 100",
    n_flights      = "n()")
}
path_of = function(s) {
  if (s$mode == "raw") return("/rows")
  if (s$design == "shared") "/stat" else paste0("/carrier-", gsub("_", "-", s$stat))
}
query_of = function(s) {
  q = paste0("origin=", s$origin, "&month=", s$month)
  if (s$mode == "server" && s$design == "shared") q = paste0(q, "&stat=", s$stat)
  q
}
core_r = function(s) {
  if (s$mode == "raw") {
    body = paste0(
      "  flights %>%\n",
      "    filter(origin == !!origin, month == !!month) %>%\n",
      "    select(month, day, carrier, origin, arr_delay)\n")
    args = "origin, month"
  } else if (s$design == "shared") {
    body = paste0(
      "  flights %>%\n",
      "    filter(origin == !!origin, month == !!month) %>%\n",
      "    group_by(carrier) %>%\n",
      "    summarize(value = switch(stat,\n",
      "      mean_arr_delay = ", stat_expr("mean_arr_delay"), ",\n",
      "      pct_late = ", stat_expr("pct_late"), ",\n",
      "      n_flights = ", stat_expr("n_flights"), "), .groups = \"drop\")\n")
    args = "origin, month, stat"
  } else {
    body = paste0(
      "  flights %>%\n",
      "    filter(origin == !!origin, month == !!month) %>%\n",
      "    group_by(carrier) %>%\n",
      "    summarize(value = ", stat_expr(s$stat), ", .groups = \"drop\")\n")
    args = "origin, month"
  }
  paste0("get_data = function(", args, ") {\n", body, "}\n",
         if (isTRUE(s$cache)) "get_data = memoise(get_data)\n" else "")
}
code_r = function(s) {
  params = paste0("#* @param origin EWR, JFK or LGA\n#* @param month 1 to 12\n",
                  if (s$mode == "server" && s$design == "shared") "#* @param stat mean_arr_delay, pct_late or n_flights\n" else "")
  call_args = if (s$mode == "server" && s$design == "shared") "origin, as.integer(month), stat" else "origin, as.integer(month)"
  fn_args = if (s$mode == "server" && s$design == "shared") paste0("origin = \"", s$origin, "\", month = ", s$month, ", stat = \"", s$stat, "\"")
            else paste0("origin = \"", s$origin, "\", month = ", s$month)
  paste0(
    "# plumber.R\n",
    "library(plumber)\n",
    "library(dplyr)\n",
    "library(readr)\n",
    if (isTRUE(s$cache)) "library(memoise)\n" else "",
    "\n",
    "flights = read_csv(\"flights.csv\", show_col_types = FALSE)\n",
    "\n",
    core_r(s),
    "\n",
    params,
    "#* @serializer json\n",
    "#* @get ", path_of(s), "\n",
    "function(", fn_args, ") {\n",
    "  get_data(", call_args, ")\n",
    "}\n",
    "\n",
    "# testme.R\n",
    "library(httr2)\n",
    "library(dplyr)\n",
    "\n",
    "resp = request(\"http://localhost:8000", path_of(s), "\") %>%\n",
    "  req_url_query(", fn_args, ") %>%\n",
    "  req_perform()\n",
    "resp %>% resp_body_json(simplifyVector = TRUE)"
  )
}
code_js = function(s) {
  paste0(
    "// React component: fetch the endpoint for the current controls\n",
    "const API = \"http://localhost:8000\";\n",
    "const url = API + \"", path_of(s), "?", query_of(s), "\";\n",
    "\n",
    "useEffect(() => {\n",
    "  fetch(url)\n",
    "    .then((res) => res.json())\n",
    "    .then((data) => setRows(data));\n",
    "}, [url]);"
  )
}

# ---- 4. EVERY REQUEST THE PAGE CAN MAKE [CHANGE] -------------------------------------------
run_core = function(s) {
  env = new.env(parent = globalenv())
  env$flights = flights
  s0 = s; s0$cache = FALSE                     # memoise changes timing, never the result
  eval(parse(text = core_r(s0)), envir = env)
  if (s$mode == "server" && s$design == "shared") env$get_data(s$origin, as.integer(s$month), s$stat)
  else env$get_data(s$origin, as.integer(s$month))
}
payload_of = function(result) as.character(toJSON(result))   # plumber's json serializer defaults

grid = expand.grid(origin = origins, month = months, stat = stats, mode = modes,
                   stringsAsFactors = FALSE) %>%
  filter(!(mode == "raw" & stat != "mean_arr_delay"))     # raw rows do not depend on stat
records = pmap(grid, function(origin, month, stat, mode) {
  s = list(origin = origin, month = month, stat = stat, mode = mode, design = "per_chart", cache = FALSE)
  result = run_core(s)
  json = payload_of(result)
  rows_matched = sum(flights$origin == origin & flights$month == month)
  preview = if (mode == "raw") as.character(toJSON(head(result, 3))) else json
  list(origin = origin, month = month, stat = if (mode == "raw") "any" else stat, mode = mode,
       rows_matched = rows_matched, payload_rows = nrow(result),
       payload_bytes = nchar(json, type = "bytes"), preview = preview)
})
dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_json(list(source_rows = nrow(flights), records = records),
           file.path(root, data_out), auto_unbox = TRUE, digits = NA)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# ---- 5. STATES [CHANGE] --------------------------------------------------------------------
base = list(origin = "JFK", month = 1, stat = "mean_arr_delay", mode = "server",
            cache = FALSE, call = 1, design = "shared")
with = function(...) modifyList(base, list(...))
states = list(
  baseline     = base,
  raw_rows     = with(mode = "raw"),
  cache_first  = with(cache = TRUE, call = 1),
  cache_second = with(cache = TRUE, call = 2),
  per_chart    = with(design = "per_chart"),
  lga_july     = with(origin = "LGA", month = 7, stat = "pct_late")
)

readouts_of = function(s) {
  result = run_core(s)                         # the panel's R, run for real
  json = payload_of(result)
  bytes = nchar(json, type = "bytes")
  rows_matched = sum(flights$origin == s$origin & flights$month == s$month)
  hit = isTRUE(s$cache) && s$call >= 2
  list(
    rows_matched  = rows_matched,
    payload_rows  = nrow(result),
    payload_bytes = bytes,
    steps_run     = if (hit) 3 else 4,
    path          = if (hit) "React > Plumber > cache > JSON" else "React > Plumber > R > JSON",
    url           = paste0(path_of(s), "?", query_of(s)),
    time_ms       = ms_network + (if (hit) 0 else ms_r(rows_matched)) + ms_transfer(bytes)
  )
}

# ---- 6. RUN + WRITE GOLDEN [KEEP] ----------------------------------------------------------
golden_states = imap(states, function(s, name) {
  list(name = name, state = s, readouts = readouts_of(s), code = list(r = code_r(s), js = code_js(s)))
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
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)
