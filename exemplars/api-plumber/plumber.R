# plumber.R
# STS exemplar API: /health, /trend, /spatial-join, /predict (stub)
# Ported from the DSAI 12_end/03_plumber pattern.
# Reads local files from the repo's data/ folder. Supabase slots in later
# (see README): replace read_solar() and read_spatial() only.

library(plumber)
library(dplyr)
library(readr)
library(sf)
library(jsonlite)

# ---- Locate the repo root (the folder holding data/jp_solar.csv) ----
find_root = function() {
  env_root = Sys.getenv("STS_ROOT", unset = "")
  starts = c(env_root, getwd())
  for (s in starts[nzchar(starts)]) {
    d = normalizePath(s, mustWork = FALSE)
    for (i in 1:6) {
      if (file.exists(file.path(d, "data", "jp_solar.csv"))) return(d)
      d = dirname(d)
    }
  }
  stop("Could not find data/jp_solar.csv. Run from inside the repo or set STS_ROOT.")
}
suppressMessages(sf::sf_use_s2(FALSE))  # planar GEOS: some precinct polygons are invalid on the sphere
ROOT = find_root()
allowed_origin = Sys.getenv("ALLOWED_ORIGIN", unset = "http://localhost:5173")

# ---- Data access (swap these two for Supabase later) ----
read_solar = function() {
  readr::read_csv(file.path(ROOT, "data", "jp_solar.csv"), show_col_types = FALSE,
                  col_types = readr::cols(muni_code = readr::col_character()))
}
read_spatial = function() {
  d = file.path(ROOT, "data", "boston_voting")
  list(
    precincts = sf::st_read(file.path(d, "precincts.geojson"), quiet = TRUE),
    points = sf::st_read(file.path(d, "polling_places.geojson"), quiet = TRUE),
    votes = readr::read_csv(file.path(d, "boston_votes.csv"), show_col_types = FALSE,
                            col_types = readr::cols(.default = "c", voted_biden = "d",
                                                    voted_trump = "d", voter_turnout = "d"))
  )
}

#* @apiTitle STS Exemplar API
#* @apiDescription The course's example Plumber API: rooftop solar adoption in Japanese municipalities (/trend), Boston polling places joined to precincts (/spatial-join), a stub model (/predict), and precinct GeoJSON (/geo). Try any route below with "Try it out".
#* @apiVersion 1.0.0
#* @plumber
function(pr) pr

#* CORS for the React starter origin (env ALLOWED_ORIGIN)
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", allowed_origin)
  res$setHeader("Vary", "Origin")
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")
    res$status = 200
    return(list())
  }
  plumber::forward()
}

#* Liveness check
#* @serializer unboxedJSON
#* @get /health
function() {
  list(status = "ok", service = "sts-api-plumber", time = format(Sys.time(), tz = "UTC", usetz = TRUE))
}

#* Mean solar adoption per month (jp_solar). Measure: solar_rate.
#* @param year Optional year filter, e.g. 2012
#* @serializer unboxedJSON
#* @get /trend
function(year = "") {
  d = read_solar() %>% mutate(month = format(date, "%Y-%m"))
  if (nzchar(year)) d = d %>% filter(year == as.integer(!!year))
  out = d %>%
    group_by(month) %>%
    summarize(mean_solar_rate = round(mean(solar_rate, na.rm = TRUE), 4),
              total_solar = sum(solar, na.rm = TRUE),
              n_munis = n_distinct(muni_code), .groups = "drop") %>%
    arrange(month)
  list(measure = "solar_rate (rooftop solar adoption rate per municipality; see data/README.md)",
       n = nrow(out), data = out)
}

#* Polling places per precinct via a spatial join, with turnout attached
#* @param ward Optional ward filter, e.g. 01 (matches the ward part of ward_precinct)
#* @param min_places Optional minimum polling places per precinct (default 0)
#* @serializer unboxedJSON
#* @get /spatial-join
function(ward = "", min_places = 0) {
  s = read_spatial()
  joined = sf::st_join(s$points %>% select(id), s$precincts, join = sf::st_within)
  counts = joined %>% sf::st_drop_geometry() %>%
    filter(!is.na(ward_precinct)) %>%
    count(ward_precinct, name = "n_polling_places")
  out = s$precincts %>% sf::st_drop_geometry() %>%
    left_join(counts, by = "ward_precinct") %>%
    mutate(n_polling_places = coalesce(n_polling_places, 0L)) %>%
    left_join(s$votes %>% select(ward_precinct, voter_turnout), by = "ward_precinct") %>%
    filter(n_polling_places >= as.integer(min_places))
  if (nzchar(ward)) out = out %>% filter(substr(ward_precinct, 1, 2) == ward)
  out = out %>% arrange(ward_precinct)
  list(join = "polling_places within precincts (sf::st_within)", n = nrow(out), data = out)
}

#* STUB: returns a fixed baseline so the React starter has a shape to call.
#* Replace with a real model (see DSAI 12_end for the xgboost pattern).
#* @param month Month to predict, YYYY-MM
#* @serializer unboxedJSON
#* @get /predict
function(month = "2012-01") {
  base = read_solar() %>% summarize(m = mean(solar_rate, na.rm = TRUE)) %>% pull(m)
  list(stub = TRUE, month = month, predicted_solar_rate = round(base, 4),
       model = "baseline-mean")
}

#* Boston precincts as GeoJSON with voter turnout attached (for exemplars/react-map).
#* GeoJSON must be WGS84 longitude/latitude (EPSG:4326): the spec (RFC 7946) says so
#* and MapLibre/Leaflet assume it. So we st_transform() BEFORE writing, always, even
#* when the file already looks like degrees. Simplified and rounded to keep it small.
#* @param tolerance Simplify tolerance in metres (default 15; 0 = no simplification)
#* @serializer contentType list(type = "application/geo+json")
#* @get /geo
function(tolerance = 15) {
  s = read_spatial()
  tol = suppressWarnings(as.numeric(tolerance))
  if (is.na(tol) || tol < 0) tol = 15
  shapes = s$precincts %>%
    left_join(s$votes %>% select(ward_precinct, voter_turnout), by = "ward_precinct") %>%
    select(ward_precinct, voter_turnout)
  if (tol > 0) {
    # Simplify in a metre-based CRS (Massachusetts State Plane, EPSG:26986), not in degrees.
    shapes = shapes %>% sf::st_transform(26986) %>%
      sf::st_simplify(preserveTopology = TRUE, dTolerance = tol)
  }
  shapes = sf::st_transform(shapes, 4326)   # THE CRS TRAP: GeoJSON is WGS84, full stop
  path = tempfile(fileext = ".geojson")
  on.exit(unlink(path))
  sf::st_write(shapes, path, driver = "GeoJSON", quiet = TRUE,
               layer_options = c("COORDINATE_PRECISION=5", "RFC7946=YES"))
  readChar(path, file.info(path)$size, useBytes = TRUE)
}
