# testme.R
# Smoke-test a running STS API. Exits 0 on pass, 1 on any failure.
#   Rscript exemplars/api-plumber/testme.R
# Env: API_PUBLIC_URL (default http://localhost:8000)

library(httr2)
library(dplyr)

base = sub("/$", "", trimws(Sys.getenv("API_PUBLIC_URL", unset = "http://localhost:8000")))
origin = Sys.getenv("ALLOWED_ORIGIN", unset = "http://localhost:5173")
fails = 0
check = function(name, ok) {
  cat(if (isTRUE(ok)) "PASS" else "FAIL", name, "\n")
  if (!isTRUE(ok)) fails <<- fails + 1
}
get = function(path, ...) {
  httr2::request(paste0(base, path)) %>%
    httr2::req_headers(Origin = origin) %>%
    httr2::req_url_query(...) %>%
    httr2::req_timeout(60) %>%
    httr2::req_perform()
}

r = get("/health")
check("/health 200", httr2::resp_status(r) == 200)
check("/health status ok", httr2::resp_body_json(r)$status == "ok")
check("CORS header", identical(httr2::resp_header(r, "Access-Control-Allow-Origin"), origin))

b = httr2::resp_body_json(get("/trend"), simplifyVector = TRUE)
check("/trend has rows", b$n > 0 && all(c("month", "mean_solar_rate") %in% names(b$data)))

b = httr2::resp_body_json(get("/spatial-join", ward = "01"), simplifyVector = TRUE)
check("/spatial-join ward filter", b$n > 0 && all(substr(b$data$ward_precinct, 1, 2) == "01"))
check("/spatial-join has counts", "n_polling_places" %in% names(b$data))

b = httr2::resp_body_json(get("/predict", month = "2012-01"), simplifyVector = TRUE)
check("/predict stub shape", isTRUE(b$stub) && is.numeric(b$predicted_solar_rate))

if (fails > 0) { cat(fails, "check(s) failed\n"); quit(status = 1) }
cat("all checks passed\n")
