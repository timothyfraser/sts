# v2a.R - prep for lab v2a (tidy data: pivot wide <-> long, cell by cell).
# Copied from tools/labs/prep/_template.R; only the [CHANGE] blocks differ.
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v2a.R
# It writes:
#   docs-v3/labs/data/v2a.csv       5 countries x 4 years x (lifeExp, pop, gdpPercap)
#   tests/labs/golden/v2a.json      named states, readouts, code, acks
# Running it twice must leave `git status` unchanged (byte-for-byte output).

library(dplyr)
library(readr)
library(tidyr)
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
lab_id    = "v2a"
page      = "docs-v3/labs/v2a-pivot-long-wide.html"
source    = "data/gapminder.csv"
seed      = 5460                                          # no sampling here; kept for the template
countries = c("Afghanistan", "Brazil", "Japan", "Nigeria", "Sweden")
years     = c(1952, 1972, 1992, 2007)
data_out  = paste0("docs-v3/labs/data/", lab_id, ".csv")
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list(list(rule = "design-system-radius", reason = "advisory only: the 4px select and 2px legend-swatch radii come from the shared kit stylesheet (kit/lab.css), which this lab may not edit; the page itself uses the 6px control token"))

# ---- 2. LOAD + REDUCE [CHANGE the pipeline, KEEP set.seed + write + re-read] ---------------
set.seed(seed)
gap_df = read_csv(file.path(root, source), show_col_types = FALSE) %>%
  filter(country %in% countries, year %in% years) %>%
  select(country, year, lifeExp, pop, gdpPercap) %>%
  arrange(country, year)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(gap_df, file.path(root, data_out), na = "")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

# The panel code calls this data `gap`.
gap = read_csv(file.path(root, data_out), show_col_types = FALSE)

# ---- 3. STATES [CHANGE] --------------------------------------------------------------------
# dir: "start" (the file as read), "longer" (pivot_longer), "wider" (pivot_wider by year).
# cols: which measure columns pivot_longer stacks. names_to / values_to: the new column names.
states = list(
  baseline     = list(dir = "start",  cols = "all",     names_to = "name",     values_to = "value"),
  long_all     = list(dir = "longer", cols = "all",     names_to = "name",     values_to = "value"),
  long_pop_gdp = list(dir = "longer", cols = "pop_gdp", names_to = "name",     values_to = "value"),
  long_named   = list(dir = "longer", cols = "all",     names_to = "variable", values_to = "measure"),
  wider_year   = list(dir = "wider",  cols = "all",     names_to = "name",     values_to = "value")
)
col_sets = list(all = c("lifeExp", "pop", "gdpPercap"), pop_gdp = c("pop", "gdpPercap"), lifeExp = c("lifeExp"))

# ---- 4. PANEL CODE [CHANGE the templates] --------------------------------------------------
code_r = function(s) {
  if (s$dir == "start") return("tab = gap\n\ntab")
  if (s$dir == "wider") return(paste0(
    "tab = gap %>%\n",
    "  select(country, year, lifeExp) %>%\n",
    "  pivot_wider(names_from = year, values_from = lifeExp)\n\ntab"))
  paste0(
    "tab = gap %>%\n",
    "  pivot_longer(cols = c(", paste(col_sets[[s$cols]], collapse = ", "), "),\n",
    "               names_to = \"", s$names_to, "\", values_to = \"", s$values_to, "\")\n\ntab")
}
code_sql = function(s) {
  if (s$dir == "start") return("SELECT * FROM gap;")
  if (s$dir == "wider") return(paste0(
    "SELECT country,\n",
    paste0("  MAX(CASE WHEN year = ", years, " THEN lifeExp END) AS \"", years, "\"", collapse = ",\n"),
    "\nFROM gap\nGROUP BY country;"))
  cs = col_sets[[s$cols]]
  keep = setdiff(c("lifeExp", "pop", "gdpPercap"), cs)
  keep_txt = if (length(keep)) paste0(paste(keep, collapse = ", "), ", ") else ""
  paste0(paste0("SELECT country, year, ", keep_txt, "'", cs, "' AS ", s$names_to, ", ", cs, " AS ", s$values_to,
                " FROM gap", collapse = "\nUNION ALL\n"), ";")
}

# The aes() checker: a mapping is possible only if every column it names exists.
aes_candidates = function(s) list(
  list(label = "aes(x = year, y = lifeExp)", needs = c("year", "lifeExp")),
  list(label = "aes(x = gdpPercap, y = lifeExp)", needs = c("gdpPercap", "lifeExp")),
  list(label = paste0("aes(x = year, y = ", s$values_to, ", colour = ", s$names_to, ")"), needs = c("year", s$values_to, s$names_to)),
  list(label = "aes(x = year, y = lifeExp, colour = country)", needs = c("year", "lifeExp", "country"))
)
readouts_from = function(tab, s) {
  cn = names(tab)
  life_rows = if ("lifeExp" %in% cn) sum(!is.na(tab$lifeExp)) else
    if (s$names_to %in% cn) sum(tab[[s$names_to]] == "lifeExp") else nrow(tab)
  ok = keep(aes_candidates(s), function(a) all(a$needs %in% cn))
  list(rows = nrow(tab), cols = ncol(tab), cells = nrow(tab) * ncol(tab),
       lifeexp_rows = life_rows, aes_valid = length(ok),
       aes_list = paste(map_chr(ok, "label"), collapse = " | "))
}

# ---- 5. RUN + CHECK EACH STATE [KEEP] ------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "gap", as.data.frame(gap))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$gap = gap
  result = eval(parse(text = code_r(s)), envir = env)
  code = list(r = code_r(s))
  if (!is.null(code_sql)) {
    code$sql = code_sql(s)
    via_sql = dbGetQuery(con, code$sql)
    if (nrow(via_sql) != nrow(result) || ncol(via_sql) != ncol(result))
      stop("prep: SQL and R disagree for state '", name, "'")
  }
  list(name = name, state = s, readouts = readouts_from(result, s), code = code)
})
dbDisconnect(con)

# ---- 6. WRITE GOLDEN [KEEP] ----------------------------------------------------------------
golden = list(
  lab          = lab_id,
  page         = page,
  generated_by = paste0("tools/labs/prep/", if (lab_id == "_reference") "_template" else lab_id, ".R"),
  tolerance    = 1e-6,
  states       = unname(golden_states),
  ack          = ack
)
dir.create(dirname(file.path(root, golden_out)), recursive = TRUE, showWarnings = FALSE)
write_json(golden, file.path(root, golden_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)
