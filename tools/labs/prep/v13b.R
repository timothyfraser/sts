# v13b.R - prep script for lab v13b (Deploy anatomy: from manifest to running API).
#
# Run from anywhere inside the repo:   Rscript tools/labs/prep/v13b.R
# It writes:
#   docs-v3/labs/data/v13b.json      a sanitised manifest.json + the log lines Connect shows
#   tests/labs/golden/v13b.json      named states, readouts, code, acks (the gate's truth)
# Running it twice leaves `git status` unchanged (no randomness, no clocks).

library(dplyr)
library(purrr)
library(jsonlite)

# ---- 0. REPO ROOT --------------------------------------------------------------------------
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
lab_id     = "v13b"
page       = "docs-v3/labs/v13b-deploy-anatomy.html"
data_out   = "docs-v3/labs/data/v13b.json"
golden_out = "tests/labs/golden/v13b.json"
ack = list()

# ---- 2. THE DATA: a sanitised manifest.json (the shape rsconnect::writeManifest() writes) --
# The API is the chapter 4 Plumber exemplar. Versions and checksums are fixed, sanitised values.
entry_path = "exemplars/api-plumber/plumber.R"
pkg_version = c(plumber = "1.2.2", dplyr = "1.1.4", readr = "2.1.5", sf = "1.0-16", jsonlite = "1.8.8")
files = c(entry_path, "data/jp_solar.csv", "data/boston_voting/precincts.geojson",
          "data/boston_voting/polling_places.geojson", "data/boston_voting/boston_votes.csv")
file_md5 = function(x) sprintf("%08x%08x%08x%08x", nchar(x) * 7919L, sum(utf8ToInt(x)) * 31L, nchar(x) * 104729L %% 65536L, 305419896L)
manifest = list(
  version = 1,
  locale = "en_US.UTF-8",
  platform = "4.3.3",
  metadata = list(appmode = "api", entrypoint = entry_path),
  packages = imap(as.list(pkg_version), function(v, p) list(Source = "CRAN", Repository = "https://cloud.r-project.org", description = list(Package = p, Version = v))),
  files = setNames(map(files, function(f) list(checksum = file_md5(f))), files)
)
wrapper_files = c("entrypoint.R", files)
logs = list(
  ok        = "GET /health 200 in 0.15s",
  no_sf     = "Error in library(sf) : there is no package called 'sf'",
  no_env    = "Error: ALLOWED_ORIGIN is not set; refusing to start",
  bad_entry = "Error: Unable to locate primary document 'plumber.R' in the bundle",
  cold      = "No running process for this content; starting one (min_processes = 0)",
  silent    = "GET /health 200 in 0.15s (no error: every origin allowed, Access-Control-Allow-Origin: *)"
)
timing = list(boot_s = 8.4, warm_s = 0.15)
step_names = c("bundle", "upload", "restore", "start", "health")
out = list(manifest = manifest, logs = logs, timing = timing, steps = step_names,
           needed = names(pkg_version))
write_json(out, file.path(root, data_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
stopifnot(file.size(file.path(root, data_out)) <= 500 * 1024)
data = fromJSON(file.path(root, data_out), simplifyVector = FALSE)   # the bytes the page fetches

# ---- 3. STATES -----------------------------------------------------------------------------
# pkgs: all | no_sf ; env: set | unset | default ; entry: path | wrong | wrapper ; min_procs: 1 | 0 ; idle: FALSE | TRUE
base_state = list(pkgs = "all", env = "set", entry = "path", min_procs = 1, idle = FALSE)
mk = function(...) modifyList(base_state, list(...))
states = list(
  baseline    = base_state,
  missing_pkg = mk(pkgs = "no_sf"),
  missing_env = mk(env = "unset"),
  wrong_entry = mk(entry = "wrong"),
  wrapper     = mk(entry = "wrapper"),
  cold_start  = mk(min_procs = 0, idle = TRUE),
  min0_warm   = mk(min_procs = 0, idle = FALSE),
  silent_default = mk(env = "default")
)

# The manifest this state ships (pure data preparation; the page does the same with the JSON).
manifest_for = function(s) {
  m = data$manifest
  if (s$pkgs == "no_sf") m$packages$sf = NULL
  if (s$entry == "wrong") m$metadata$entrypoint = "plumber.R"
  if (s$entry == "wrapper") {
    m$metadata$entrypoint = "entrypoint.R"
    m$files = c(list(entrypoint.R = list(checksum = file_md5("entrypoint.R"))), m$files)
  }
  m
}
env_for = function(s) switch(s$env,
  set     = c(ALLOWED_ORIGIN = "https://sts.example.edu"),
  unset   = character(0),
  default = c(ALLOWED_ORIGIN = "*"))

# ---- 4. PANEL CODE -------------------------------------------------------------------------
# Three helper scripts (shown, not run) followed by the diagnosis (run for real, below).
entry_shown = function(s) c(path = entry_path, wrong = "plumber.R", wrapper = "entrypoint.R")[[s$entry]]
scripts_r = function(s) {
  paste0(
    "# manifestme.R  (writes manifest.json)\n",
    if (s$pkgs == "no_sf") "# manifest written before library(sf) was added: sf is not recorded\n" else "",
    "rsconnect::writeManifest(\n",
    "  appMode = \"api\",\n",
    "  appPrimaryDoc = \"", entry_shown(s), "\"\n",
    ")\n\n",
    "# deployme.R  (bundle, upload, restore, start)\n",
    "rsconnect::deployAPI(api = \".\", appPrimaryDoc = \"", entry_shown(s), "\")\n",
    if (s$env == "set") "# Connect Vars: ALLOWED_ORIGIN is set\n" else "# Connect Vars: ALLOWED_ORIGIN was never set\n",
    if (s$env == "default") "# plumber.R reads it with a silent default:\n#   origin = Sys.getenv(\"ALLOWED_ORIGIN\", unset = \"*\")\n" else "",
    "# Connect Runtime: min processes = ", s$min_procs, "\n\n",
    "# testme.R  (health check)\n",
    "httr2::request(paste0(base, \"/health\")) %>%\n",
    "  httr2::req_perform()\n\n"
  )
}
diagnose_r = function(s) {
  ev = env_for(s)
  paste0(
    "# what Connect does with this bundle\n",
    "manifest = fromJSON(\"manifest.json\")\n",
    "needed = c(\"plumber\", \"dplyr\", \"readr\", \"sf\", \"jsonlite\")\n",
    "env_vars = ", if (length(ev) == 0) "character(0)" else paste0("c(ALLOWED_ORIGIN = \"", ev[["ALLOWED_ORIGIN"]], "\")"),
    if (s$env == "default") "   # unset on Connect: the code's default fills it in" else "", "\n",
    "min_processes = ", s$min_procs, "\n",
    "idle = ", if (s$idle) "TRUE" else "FALSE", "\n\n",
    "failed = case_when(\n",
    "  !(manifest$metadata$entrypoint %in% names(manifest$files)) ~ \"upload\",\n",
    "  length(setdiff(needed, names(manifest$packages))) > 0 ~ \"start\",\n",
    "  !(\"ALLOWED_ORIGIN\" %in% names(env_vars)) ~ \"start\",\n",
    "  TRUE ~ \"none\"\n",
    ")\n\n",
    "steps = c(\"bundle\", \"upload\", \"restore\", \"start\", \"health\")\n",
    "tibble(\n",
    "  steps_ok = if (failed == \"none\") 5 else match(failed, steps) - 1,\n",
    "  failed_step = failed,\n",
    "  first_response_s = case_when(\n",
    "    failed != \"none\" ~ 0,\n",
    "    min_processes == 0 & idle ~ 8.4 + 0.15,\n",
    "    TRUE ~ 0.15\n",
    "  )\n",
    ")"
  )
}
code_r = function(s) paste0(scripts_r(s), diagnose_r(s))

log_for = function(s, result) {
  if (result$failed_step == "upload") data$logs$bad_entry
  else if (result$failed_step == "start" && s$pkgs == "no_sf") data$logs$no_sf
  else if (result$failed_step == "start") data$logs$no_env
  else if (result$first_response_s > 1) data$logs$cold
  else if (s$env == "default") data$logs$silent
  else data$logs$ok
}

# ---- 5. RUN + CHECK EACH STATE -------------------------------------------------------------
golden_states = imap(states, function(s, name) {
  env = new.env(parent = globalenv())
  env$manifest = NULL
  # The panel reads manifest.json; give the evaluation the manifest this state ships.
  env$fromJSON = function(path) manifest_for(s)
  env$fromJSON_ok = TRUE
  result = eval(parse(text = diagnose_r(s)), envir = env)
  r = as.list(result)
  reads = list(steps_ok = r$steps_ok, failed_step = r$failed_step,
               first_response_s = r$first_response_s, log_line = log_for(s, r))
  list(name = name, state = s, readouts = reads, code = list(r = code_r(s)))
})

# ---- 6. WRITE GOLDEN -----------------------------------------------------------------------
golden = list(lab = lab_id, page = page, generated_by = "tools/labs/prep/v13b.R", tolerance = 1e-6,
              states = unname(golden_states), ack = ack)
dir.create(dirname(file.path(root, golden_out)), recursive = TRUE, showWarnings = FALSE)
write_json(golden, file.path(root, golden_out), auto_unbox = TRUE, digits = NA, pretty = TRUE)
message("prep: wrote ", data_out, " (", file.size(file.path(root, data_out)), " B) and ", golden_out)
