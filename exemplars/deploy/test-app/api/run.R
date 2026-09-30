# run.R: start the test API in a container (Docker, DigitalOcean).
# The Dockerfile runs `Rscript run.R` from /app, so plumber.R and data/ sit
# right next to this file. Posit Connect does NOT use this file: Connect runs
# plumber.R itself (see manifest.json).
#
# Env:
#   PORT            the port to listen on. DigitalOcean sets it (8080 by default);
#                   locally it falls back to 8080 too.
#   ALLOWED_ORIGIN  which web page may call this API from a browser (CORS).
#                   "*" (any page) is fine for this toy API with no login.
#                   Set it to your front end's URL for a real app.

library(plumber)

port = as.integer(Sys.getenv("PORT", unset = "8080"))
allowed_origin = Sys.getenv("ALLOWED_ORIGIN", unset = "*")

# plumber.R is the unchanged course test API (/echo, /sum, /plot).
pr = plumber::plumb("plumber.R")

# Add CORS here, not in plumber.R, so plumber.R stays identical on every route.
# A filter runs before every endpoint. An OPTIONS "preflight" request gets its
# answer here and stops; every other request is forwarded to its endpoint.
pr = plumber::pr_filter(pr, "cors", function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", allowed_origin)
  res$setHeader("Vary", "Origin")
  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
    res$setHeader("Access-Control-Allow-Headers", "Content-Type")
    res$status = 200
    return(list())
  }
  plumber::forward()
})

message("Test API listening on port ", port, " (docs at /__docs__/)")
pr$run(host = "0.0.0.0", port = port, docs = TRUE)
