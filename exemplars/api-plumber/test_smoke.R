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
stop_api()
quit(status = status)
