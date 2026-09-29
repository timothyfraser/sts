# v3a.R - prep for the joins lab (docs-v3/labs/v3a-join-lab.html).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v3a.R
# It writes:
#   docs-v3/labs/data/v3a_<table>.csv    five small seeded tables the page joins in the browser
#   tests/labs/golden/v3a.json           named states, readouts, code, acks (the gate's truth)
# Running it twice must leave `git status` unchanged (byte-for-byte output).
# Built from tools/labs/prep/_template.R; only the [CHANGE] blocks differ.

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
lab_id     = "v3a"
page       = "docs-v3/labs/v3a-join-lab.html"
seed       = 5460
n_left     = 12                      # left-table rows shown on the bench
data_dir   = "docs-v3/labs/data/"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD + SAMPLE [CHANGE] -------------------------------------------------------------
# Left tables are small seeded samples so every row can be drawn; right tables keep the rows
# the sample needs plus a few it does not, so a right or full join has something to add.
set.seed(seed)
flights_all = read_csv(file.path(root, "data/flights.csv"), show_col_types = FALSE,
                       col_types = cols(.default = col_character(), distance = col_double()))
flights = flights_all %>%
  filter(carrier %in% c("UA", "AA", "DL", "B6", "WN")) %>%
  slice_sample(n = n_left - 1) %>%
  bind_rows(flights_all %>% filter(dest == "SJU") %>% slice_sample(n = 1)) %>%
  arrange(carrier, dest) %>%
  mutate(id = sprintf("f%02d", row_number())) %>%
  select(id, carrier, dest, distance)

airlines = read_csv(file.path(root, "data/airlines.csv"), show_col_types = FALSE,
                    col_types = cols(.default = col_character())) %>%
  filter(carrier %in% c(flights$carrier, "HA", "OO")) %>%
  arrange(carrier)

airports = read_csv(file.path(root, "data/airports.csv"), show_col_types = FALSE,
                    col_types = cols(.default = col_character())) %>%
  filter(faa %in% c(flights$dest, "ITH", "BTV")) %>%
  select(faa, name) %>%
  arrange(faa)

solar_all = read_csv(file.path(root, "data/jp_solar.csv"), show_col_types = FALSE,
                     col_types = cols(.default = col_character(), solar = col_double()))
covars_all = read_csv(file.path(root, "data/jp_solar_farms_2018.csv"), show_col_types = FALSE,
                      col_types = cols(.default = col_character()))
munis_pick = sample(sort(unique(solar_all$muni_code)), 4)
solar = solar_all %>%
  filter(muni_code %in% munis_pick, year == "2019") %>%
  group_by(muni_code) %>% slice_min(date, n = 3, with_ties = FALSE) %>% ungroup() %>%
  arrange(muni_code, date) %>%
  mutate(id = sprintf("m%02d", row_number())) %>%
  select(id, muni_code, date, solar)
munis = covars_all %>%
  filter(muni_code %in% c(munis_pick, sample(setdiff(covars_all$muni_code, munis_pick), 2))) %>%
  select(muni_code, muni, pref) %>%
  arrange(muni_code)

tables = list(flights = flights, airlines = airlines, airports = airports, solar = solar, munis = munis)
dir.create(file.path(root, data_dir), recursive = TRUE, showWarnings = FALSE)
iwalk(tables, function(t, nm) write_csv(t, file.path(root, data_dir, paste0(lab_id, "_", nm, ".csv")), na = ""))
total = sum(file.size(file.path(root, data_dir, paste0(lab_id, "_", names(tables), ".csv"))))
stopifnot(total <= 500 * 1024)

# Re-read the files the page fetches: keys stay character, as the course reads them.
reread = function(nm, num = character()) {
  ct = cols(.default = col_character())
  for (n in num) ct$cols[[n]] = col_double()
  read_csv(file.path(root, data_dir, paste0(lab_id, "_", nm, ".csv")), show_col_types = FALSE, col_types = ct)
}
flights  = reread("flights", "distance")
airlines = reread("airlines")
airports = reread("airports")
solar    = reread("solar", "solar")
munis    = reread("munis")

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
# pair: airlines (flights x airlines by carrier), airports (flights x airports by dest = faa),
#       munis (municipality-months x covariates by muni_code)
# how:  inner | left | right | full | semi | anti
# dup:  duplicate the right-table row for the first left key; miss: add a left row whose key
#       is in no right row; cast: read the right key as integer (the type-mismatch bug);
# blank: set the name of a MATCHED right row to NA, so left_join() + filter(is.na(name)) and
#       anti_join() stop agreeing
st = function(pair = "airlines", how = "left", dup = FALSE, miss = FALSE, cast = FALSE, blank = FALSE)
  list(pair = pair, how = how, dup = dup, miss = miss, cast = cast, blank = blank)
states = list(
  baseline      = st(),
  inner         = st(how = "inner"),
  right         = st(how = "right"),
  full          = st(how = "full"),
  semi          = st(how = "semi"),
  anti_missing  = st(how = "anti", miss = TRUE),
  dup_key       = st(dup = TRUE),
  left_missing  = st(miss = TRUE),
  airports_left = st(pair = "airports"),
  airports_anti = st(pair = "airports", how = "anti"),
  munis_left    = st(pair = "munis"),
  munis_cast    = st(pair = "munis", cast = TRUE),
  anti_vs_leftna = st(pair = "airports", how = "anti", blank = TRUE)
)

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
pairs = list(
  airlines = list(x = "flights", y = "airlines", by = '"carrier"', xk = "carrier", yk = "carrier", yn = "name",
                  dupkey = "UA", misskey = "ZZ", missrow = 'id = "f99", carrier = "ZZ", dest = "ORD", distance = 733'),
  airports = list(x = "flights", y = "airports", by = 'c("dest" = "faa")', xk = "dest", yk = "faa", yn = "name",
                  dupkey = "MCO", misskey = "XNA", missrow = 'id = "f99", carrier = "UA", dest = "XNA", distance = 1085'),
  munis    = list(x = "solar", y = "munis", by = '"muni_code"', xk = "muni_code", yk = "muni_code", yn = "muni",
                  dupkey = NA, misskey = "99999", missrow = 'id = "m99", muni_code = "99999", date = "2019-01-25", solar = 0')
)
pairs$munis$dupkey = sort(unique(solar$muni_code))[1]
warn_mm = "Detected an unexpected many-to-many relationship between `x` and `y`."

prep_lines = function(s) {
  p = pairs[[s$pair]]; out = character()
  xname = p$x; yname = p$y
  if (s$miss) { out = c(out, paste0("x = ", p$x, " %>%\n  bind_rows(tibble(", p$missrow, "))")); xname = "x" }
  if (s$dup || s$cast || s$blank) {
    steps = paste0("y = ", p$y)
    if (s$dup)  steps = paste0(steps, " %>%\n  bind_rows(", p$y, " %>% filter(", p$yk, ' == "', p$dupkey, '"))')
    if (s$blank) steps = paste0(steps, " %>%\n  mutate(", p$yn, " = if_else(", p$yk, ' == "', p$dupkey, '", NA, ', p$yn, "))")
    if (s$cast) steps = paste0(steps, " %>%\n  mutate(", p$yk, " = as.integer(", p$yk, "))")
    out = c(out, steps); yname = "y"
  }
  list(lines = out, x = xname, y = yname)
}
join_call = function(s) {
  p = pairs[[s$pair]]; pl = prep_lines(s)
  check = if (s$blank) paste0("na_rows = ", pl$x, " %>%\n  left_join(by = ", p$by, ", y = ", pl$y, ") %>%\n  filter(is.na(", p$yn, "))") else character()
  paste0(c(pl$lines, paste0("result = ", pl$x, " %>%\n  ", s$how, "_join(by = ", p$by, ", y = ", pl$y, ")"), check), collapse = "\n")
}
# Whether this state warns or errors: decided by the data, checked against R below.
outcome = function(s) {
  if (s$cast) return("error")
  if (s$dup && s$how %in% c("inner", "left", "right", "full")) return("warn")
  "ok"
}
code_r = function(s) {
  o = outcome(s)
  tail = switch(o,
    error = paste0("\n# Error in `", s$how, "_join()`:\n# ! Can't join `x$", pairs[[s$pair]]$xk,
                   "` with `y$", pairs[[s$pair]]$yk, "` due to incompatible types."),
    warn  = paste0("\n# Warning message:\n# ", warn_mm,
                   "\n# Set `relationship = \"many-to-many\"` if you expect it."),
    ok    = "")
  paste0(join_call(s), tail)
}
sql_kind = c(inner = "INNER JOIN", left = "LEFT JOIN", right = "RIGHT JOIN", full = "FULL JOIN")
code_sql = function(s) {
  p = pairs[[s$pair]]
  xt = if (s$miss) "x" else p$x; yt = if (s$dup || s$cast || s$blank) "y" else p$y
  on = paste0("a.", p$xk, " = b.", p$yk)
  if (s$how %in% names(sql_kind)) return(paste0("SELECT a.*, b.*\nFROM ", xt, " AS a\n", sql_kind[[s$how]], " ", yt, " AS b\n  ON ", on, ";"))
  paste0("SELECT a.*\nFROM ", xt, " AS a\nWHERE ", if (s$how == "anti") "NOT " else "", "EXISTS (\n  SELECT 1 FROM ", yt, " AS b WHERE ", on, "\n);")
}

card = function(xkeys, ykeys) {
  m = intersect(xkeys, ykeys)
  xd = any(duplicated(xkeys[xkeys %in% m])); yd = any(duplicated(ykeys[ykeys %in% m]))
  paste0(if (xd) "m" else "1", ":", if (yd) "m" else "1")
}
run_state = function(s) {
  env = new.env(parent = globalenv())
  for (nm in names(tables)) assign(nm, get(nm), envir = env)
  warned = character(); err = NULL
  withCallingHandlers(
    tryCatch(eval(parse(text = join_call(s)), envir = env), error = function(e) err <<- conditionMessage(e)),
    warning = function(w) { warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning") })
  got = if (!is.null(err)) "error" else if (length(warned)) "warn" else "ok"
  if (got != outcome(s)) stop("prep: outcome mismatch for ", s$pair, "/", s$how, ": ", got)
  if (got == "warn" && !startsWith(warned[1], warn_mm)) stop("prep: unexpected warning text: ", warned[1])
  p = pairs[[s$pair]]
  x = if (exists("x", envir = env, inherits = FALSE)) env$x else get(p$x)
  y = if (exists("y", envir = env, inherits = FALSE)) env$y else get(p$y)
  res = if (got == "error") NULL else env$result
  # left_join() + filter(is.na(<name>)): run by the panel code when blank is on, else computed here
  na_name = if (got == "error") -1 else nrow(suppressWarnings(left_join(x, y, by = setNames(p$yk, p$xk))) %>% filter(is.na(.data[[p$yn]])))
  if (s$blank && got != "error" && nrow(env$na_rows) != na_name) stop("prep: na_rows disagrees for ", s$pair)
  list(env = env, x = x, y = y, res = res,
       readouts = list(
         rows_left  = nrow(x),
         rows_right = nrow(y),
         rows_out   = if (is.null(res)) -1 else nrow(res),
         na_cells   = if (is.null(res)) -1 else sum(is.na(res)),
         na_name_rows = na_name,
         explosion  = if (is.null(res)) -1 else nrow(res) / nrow(x),
         cardinality = if (got == "error") "\u2014" else card(as.character(x[[p$xk]]), as.character(y[[p$yk]])),
         outcome    = got))
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
golden_states = imap(states, function(s, name) {
  r = run_state(s)
  code = list(r = code_r(s), sql = code_sql(s))
  if (r$readouts$outcome != "error") {
    con = dbConnect(SQLite(), ":memory:")
    for (nm in names(tables)) dbWriteTable(con, nm, as.data.frame(get(nm)))
    if (s$miss) dbWriteTable(con, "x", as.data.frame(r$x))
    if (s$dup || s$blank) dbWriteTable(con, "y", as.data.frame(r$y))
    via_sql = dbGetQuery(con, code$sql)
    dbDisconnect(con)
    if (nrow(via_sql) != nrow(r$res)) stop("prep: SQL and R disagree for state '", name, "'")
  }
  list(name = name, state = s, readouts = r$readouts, code = code)
})

# ---- 6. WRITE GOLDEN [KEEP] ----------------------------------------------------------------
golden = list(lab = lab_id, page = page, generated_by = paste0("tools/labs/prep/", lab_id, ".R"),
              tolerance = 1e-6, states = unname(golden_states), ack = ack)
dir.create(dirname(file.path(root, golden_out)), recursive = TRUE, showWarnings = FALSE)
write_json(golden, file.path(root, golden_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
message("prep: wrote ", data_dir, lab_id, "_*.csv (", total, " B) and ", golden_out)
