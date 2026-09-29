# v1a.R - prep script for the query-builder lab (dplyr and SQL, verb by verb).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v1a.R
# It writes:
#   docs-v3/labs/data/v1a_flights.csv   a seeded 2,000-row sample of data/flights.csv
#   docs-v3/labs/data/v1a_airlines.csv  the carrier names from data/airlines.csv (carriers in the sample)
#   tests/labs/golden/v1a.json          named states, readouts, code, acks (the gate's truth)
# Running it twice leaves `git status` unchanged (byte-for-byte output).

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

# ---- 1. CONFIG -----------------------------------------------------------------------------
lab_id     = "v1a"
page       = "docs-v3/labs/v1a-query-builder.html"
source     = "data/flights.csv"
seed       = 5460
n_sample   = 2000
data_out   = "docs-v3/labs/data/v1a_flights.csv"
air_src    = "data/airlines.csv"
air_out    = "docs-v3/labs/data/v1a_airlines.csv"
golden_out = "tests/labs/golden/v1a.json"
ack = list()

# ---- 2. LOAD + SAMPLE ----------------------------------------------------------------------
set.seed(seed)
sample_df = read_csv(file.path(root, source), show_col_types = FALSE) %>%
  filter(!is.na(dep_delay), !is.na(arr_delay), !is.na(air_time)) %>%
  slice_sample(n = n_sample) %>%
  arrange(month, day, sched_dep_time, carrier, flight) %>%
  mutate(id = sprintf("f%04d", row_number())) %>%
  select(id, month, day, carrier, origin, dest, dep_delay, arr_delay, air_time, distance)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(sample_df, file.path(root, data_out), na = "")
# airlines.csv names the carrier codes; keep only the carriers in the sample.
airlines = read_csv(file.path(root, air_src), show_col_types = FALSE) %>%
  filter(carrier %in% sample_df$carrier) %>%
  arrange(carrier)
write_csv(airlines, file.path(root, air_out), na = "")
stopifnot(file.size(file.path(root, data_out)) + file.size(file.path(root, air_out)) <= 500 * 1024)

# The panel code calls the sample `flights`, exactly as the class script does.
flights = read_csv(file.path(root, data_out), show_col_types = FALSE)

# ---- 3. STATES -----------------------------------------------------------------------------
# A state is a pipeline: `order` lists the verb blocks top to bottom (head is always last),
# and the other fields are each block's parameters. month = 0 means "any month".
base = list(order = "filter,mutate,select,arrange,head", origin = "JFK", min_delay = 60,
            month = 0, mutate = "gain", cols = "carrier,dep_delay,gain",
            sort = "dep_delay", desc = TRUE, n = 10)
with = function(...) modifyList(base, list(...))
states = list(
  baseline  = base,
  no_filter = with(order = "mutate,select,arrange,head"),
  lga_june  = with(origin = "LGA", min_delay = 0, month = 6),
  swapped   = with(order = "filter,select,mutate,arrange,head", cols = "carrier,dep_delay"),
  by_gain   = with(sort = "gain"),
  speed     = with(mutate = "speed", cols = "carrier,origin,speed", sort = "speed"),
  select_first = with(order = "select,filter,mutate,arrange,head"),
  sort_missing = with(mutate = "speed", cols = "carrier,origin,speed", sort = "gain")
)

# ---- 4. PANEL CODE -------------------------------------------------------------------------
mutate_r   = c(gain = "gain = dep_delay - arr_delay",
               speed = "speed = distance / air_time * 60",
               late = "late = dep_delay > 15")
mutate_sql = c(gain = "dep_delay - arr_delay AS gain",
               speed = "distance * 60.0 / air_time AS speed",
               late = "dep_delay > 15 AS late")
split_cols = function(s) strsplit(s$cols, ",", fixed = TRUE)[[1]]
steps_of   = function(s) if (nchar(s$order) == 0) character(0) else strsplit(s$order, ",", fixed = TRUE)[[1]]

pred_r = function(s) {
  p = c(if (s$origin != "any") paste0("origin == \"", s$origin, "\""),
        paste0("dep_delay > ", s$min_delay),
        if (s$month != 0) paste0("month == ", s$month))
  paste(p, collapse = ", ")
}
step_r = function(v, s) switch(v,
  filter  = paste0("filter(", pred_r(s), ")"),
  mutate  = paste0("mutate(", mutate_r[[s$mutate]], ")"),
  select  = paste0("select(", paste(split_cols(s), collapse = ", "), ")"),
  arrange = paste0("arrange(", if (isTRUE(s$desc)) paste0("desc(", s$sort, ")") else s$sort, ")"),
  head    = paste0("head(", s$n, ")"))

code_r = function(s) {
  st = steps_of(s)
  paste0("result = flights",
         paste0(" %>%\n  ", vapply(st, step_r, "", s = s), collapse = ""),
         "\n\nresult %>%\n  summarize(rows_out = n(), cols_out = ncol(result))")
}

# SQL: the flat SELECT ... FROM ... WHERE ... ORDER BY ... LIMIT when every block reads columns
# that still exist; when select drops a column a later block needs, the honest twin is a
# subquery, and SQLite rejects it just as dplyr does.
code_sql = function(s) {
  st = steps_of(s)
  where = if ("filter" %in% st) {
    p = c(if (s$origin != "any") paste0("origin = '", s$origin, "'"),
          paste0("dep_delay > ", s$min_delay),
          if (s$month != 0) paste0("month = ", s$month))
    paste0("\nWHERE ", paste(p, collapse = "\n  AND "))
  } else ""
  order = if ("arrange" %in% st) paste0("\nORDER BY ", s$sort, if (isTRUE(s$desc)) " DESC" else "") else ""
  limit = if ("head" %in% st) paste0("\nLIMIT ", s$n) else ""
  new_col = if ("mutate" %in% st) s$mutate else NULL
  sel = if ("select" %in% st) split_cols(s) else c("*")
  expr = function(cn) if (!is.null(new_col) && cn == new_col) mutate_sql[[new_col]] else cn
  if ("select" %in% st && "mutate" %in% st && match("select", st) < match("mutate", st)) {
    inner = paste0("SELECT ", paste(sel, collapse = ", "), "\nFROM flights", where)
    return(paste0("SELECT *, ", mutate_sql[[new_col]], "\nFROM (", gsub("\n", "\n  ", inner), ")", order, limit, ";"))
  }
  cols = if (identical(sel, "*")) c("*", if (!is.null(new_col)) mutate_sql[[new_col]]) else vapply(sel, expr, "")
  paste0("SELECT ", paste(cols, collapse = ", "), "\nFROM flights", where, order, limit, ";")
}

# Readouts: rows after the filter block (the LC 01 number), rows and columns out, the first
# row's id, and the R error text (empty when the pipeline runs).
readouts_of = function(s, env) {
  err = env$err
  f = flights
  if ("filter" %in% steps_of(s)) f = eval(parse(text = paste0("flights %>% ", step_r("filter", s))))
  list(rows_filtered = nrow(f),
       rows_out = if (is.null(err)) nrow(env$result) else 0L,
       cols_out = if (is.null(err)) ncol(env$result) else 0L,
       error = if (is.null(err)) "" else err)
}

# ---- 5. RUN + CHECK EACH STATE -------------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "flights", as.data.frame(flights))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$flights = flights
  env$err = NULL
  tryCatch(eval(parse(text = code_r(s)), envir = env),
           error = function(e) {
             verb = sub("\\(.*", "", deparse(e$call)[1])
             msg = if (!is.null(e$parent)) conditionMessage(e$parent) else conditionMessage(e)
             # one line, bullets dropped: the text is the same in every locale
             msg = paste(sub("^[^A-Za-z`]*([xi!] )?", "", strsplit(msg, "\n")[[1]]), collapse = " ")
             env$err = paste0("Error in `", verb, "()`: ", msg)
           })
  code = list(r = code_r(s), sql = code_sql(s))
  via_sql = tryCatch(dbGetQuery(con, code$sql), error = function(e) NULL)
  if (is.null(env$err)) {
    if (is.null(via_sql) || nrow(via_sql) != nrow(env$result))
      stop("prep: SQL and R disagree for state '", name, "'")
  } else if (!is.null(via_sql)) stop("prep: R errors but SQL runs for state '", name, "'")
  list(name = name, state = s, readouts = readouts_of(s, env), code = code)
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
