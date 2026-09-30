# test_smoke.R
# One-command check: start the API locally, run testme.R against it, stop it.
#   Rscript exemplars/api-plumber/test_smoke.R      (exits 0 on pass)
# Env: SMOKE_PORT (default 8765)

library(httr2)

args = commandArgs(FALSE)
here = dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1])))
port = Sys.getenv("SMOKE_PORT", unset = "8765")
rscript = file.path(R.home("bin"), "Rscript")
log = tempfile(fileext = ".log")

pid = system(paste0("PORT=", port, " ", shQuote(rscript), " ", shQuote(file.path(here, "runme.R")),
                    " > ", shQuote(log), " 2>&1 & echo $!"), intern = TRUE)
stop_api = function() system(paste("kill", pid, "2>/dev/null"))

up = FALSE
for (i in 1:60) {
  up = tryCatch({ httr2::req_perform(httr2::request(paste0("http://localhost:", port, "/health"))); TRUE },
                error = function(e) FALSE)
  if (up) break
  Sys.sleep(1)
}
if (!up) { cat("FAIL: API did not start; log:", log, "\n"); stop_api(); quit(status = 1) }

status = system2(rscript, shQuote(file.path(here, "testme.R")),
                 env = paste0("API_PUBLIC_URL=http://localhost:", port))

# /geo: the precinct GeoJSON that exemplars/react-map draws (testme.R does not cover it).
geo_ok = tryCatch({
  r = httr2::req_perform(httr2::request(paste0("http://localhost:", port, "/geo")))
  g = jsonlite::fromJSON(httr2::resp_body_string(r), simplifyVector = FALSE)
  feats = g$features
  c1 = httr2::resp_status(r) == 200 && identical(g$type, "FeatureCollection") && length(feats) > 0
  # WGS84 lon/lat only: Boston sits near lon -71, lat 42 (a projected CRS would be huge numbers)
  xy = unlist(feats[[1]]$geometry$coordinates)
  c2 = all(abs(xy) < 180) && all(xy[seq(1, length(xy), 2)] < 0)
  c3 = "ward_precinct" %in% names(feats[[1]]$properties) && "voter_turnout" %in% names(feats[[1]]$properties)
  cat(if (c1) "PASS" else "FAIL", "/geo is a non-empty FeatureCollection\n")
  cat(if (c2) "PASS" else "FAIL", "/geo coordinates are WGS84 lon/lat\n")
  cat(if (c3) "PASS" else "FAIL", "/geo features carry ward_precinct + voter_turnout\n")
  c1 && c2 && c3
}, error = function(e) { cat("FAIL /geo request:", conditionMessage(e), "\n"); FALSE })
stop_api()
if (!geo_ok) quit(status = 1)
quit(status = status)
