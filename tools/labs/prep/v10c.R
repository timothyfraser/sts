# v10c.R - prep for the lab "Two-mode to one-mode: projecting affiliations" (chapter 10).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v10c.R
# It writes:
#   docs-v3/labs/data/v10c_memberships.csv   the national committees' membership edgelist
#   tests/labs/golden/v10c.json              named states, readouts, code, acks
# Running it twice leaves `git status` unchanged (no randomness; fixed row order).

library(dplyr)
library(readr)
library(purrr)
library(jsonlite)
library(DBI)
library(RSQLite)
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
lab_id     = "v10c"
page       = "docs-v3/labs/v10c-projection.html"
seed       = 5460
data_out   = "docs-v3/labs/data/v10c_memberships.csv"
golden_out = paste0("tests/labs/golden/", lab_id, ".json")
ack = list()

# ---- 2. LOAD -------------------------------------------------------------------------------
# One network: the five national-level recovery committees (84 people, 90 seats). No single
# town in the data has more than two committees that share members, so the national set is
# the smallest one where projecting onto committees is not trivial.
set.seed(seed)
committees = read_csv(file.path(root, "data/committees/committees.csv"), show_col_types = FALSE) %>%
  filter(town == "National") %>%
  select(committee_id = name, committee_romaji)

out = read_csv(file.path(root, "data/committees/edgelist.csv"), show_col_types = FALSE) %>%
  inner_join(committees, by = "committee_id") %>%
  arrange(committee_id, member_id) %>%
  select(committee_id, member_id, weight, committee_romaji)

dir.create(dirname(file.path(root, data_out)), recursive = TRUE, showWarnings = FALSE)
write_csv(out, file.path(root, data_out), na = "")
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)

memberships = read_csv(file.path(root, data_out), show_col_types = FALSE)
largest = memberships %>% count(committee_id) %>% arrange(desc(n), committee_id) %>% slice(1) %>% pull(committee_id)

# ---- 3. STATES -----------------------------------------------------------------------------
# Same names and fields as `states` in the page. drop = "" keeps every committee.
states = list(
  baseline   = list(mode = "people",     weighting = "count",  threshold = 1, drop = ""),
  drop_large = list(mode = "people",     weighting = "count",  threshold = 1, drop = largest),
  newman     = list(mode = "people",     weighting = "newman", threshold = 1, drop = ""),
  shared2    = list(mode = "people",     weighting = "count",  threshold = 2, drop = ""),
  committees = list(mode = "committees", weighting = "count",  threshold = 1, drop = "")
)

# ---- 4. PANEL CODE -------------------------------------------------------------------------
code_r = function(s) {
  paste0(
    "# coaffiliate(), ported inline: incidence matrix, then B x t(B)\n",
    "kept = memberships",
    if (s$drop != "") paste0(" %>%\n  filter(committee_id != \"", s$drop, "\")") else "",
    "\n\n",
    "people = unique(memberships$member_id)   # everyone stays a node\n",
    "nodes = tibble(name = c(people, unique(kept$committee_id)))\n",
    "g = graph_from_data_frame(kept %>% select(member_id, committee_id),\n",
    "                          directed = FALSE, vertices = nodes)\n",
    "V(g)$type = V(g)$name %in% kept$committee_id\n",
    "B = as_biadjacency_matrix(g, names = TRUE, sparse = FALSE)  # people x committees\n",
    if (s$mode == "people") "P = B                                   # project onto people\n"
    else "P = t(B)                                # project onto committees\n",
    "\n",
    "K = P %*% t(P)                           # shared affiliations\n",
    if (s$weighting == "newman")
      "A = P %*% diag(1 / pmax(colSums(P) - 1, 1)) %*% t(P)  # Newman weights\n"
    else "A = K                                   # weight = count shared\n",
    "diag(A) = 0\n",
    "A[K < ", s$threshold, "] = 0\n",
    "gp = graph_from_adjacency_matrix(A, mode = \"undirected\", weighted = TRUE)\n",
    "\n",
    "tibble(nodes = vcount(gp), edges = ecount(gp),\n",
    "       density = edge_density(gp),\n",
    "       clustering = transitivity(gp, type = \"global\"),\n",
    "       total_weight = sum(E(gp)$weight))"
  )
}
code_sql = function(s) {
  a = if (s$mode == "people") "member_id" else "committee_id"
  by = if (s$mode == "people") "committee_id" else "member_id"
  where = if (s$drop != "") paste0("\n  WHERE committee_id <> '", s$drop, "'") else ""
  paste0(
    "-- self-join the membership table on the shared ", if (s$mode == "people") "committee" else "member", "\n",
    "WITH kept AS (\n  SELECT * FROM memberships", where, "\n),\n",
    "sizes AS (\n  SELECT ", by, ", COUNT(*) AS n FROM kept GROUP BY ", by, "\n)\n",
    "SELECT a.", a, " AS node_1, b.", a, " AS node_2,\n",
    "       COUNT(*) AS shared,\n",
    "       SUM(1.0 / MAX(s.n - 1, 1)) AS newman\n",
    "FROM kept a\n",
    "JOIN kept b ON a.", by, " = b.", by, " AND a.", a, " < b.", a, "\n",
    "JOIN sizes s ON s.", by, " = a.", by, "\n",
    "GROUP BY a.", a, ", b.", a, "\n",
    "HAVING COUNT(*) >= ", s$threshold, ";"
  )
}
readouts_from = function(result) as.list(result)

# ---- 5. RUN + CHECK EACH STATE -------------------------------------------------------------
con = dbConnect(SQLite(), ":memory:")
dbWriteTable(con, "memberships", as.data.frame(memberships))

golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$memberships = memberships
  result = eval(parse(text = code_r(s)), envir = env)
  code = list(r = code_r(s), sql = code_sql(s))
  via_sql = dbGetQuery(con, code$sql)
  w = if (s$weighting == "newman") via_sql$newman else via_sql$shared
  if (nrow(via_sql) != result$edges || abs(sum(w) - result$total_weight) > 1e-9)
    stop("prep: SQL and R disagree for state '", name, "'")
  list(name = name, state = s, readouts = readouts_from(result), code = code)
})
dbDisconnect(con)

# ---- 6. WRITE GOLDEN -----------------------------------------------------------------------
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
