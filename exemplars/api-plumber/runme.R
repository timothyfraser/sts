# runme.R
# Run the STS API locally. From the repo root:
#   Rscript exemplars/api-plumber/runme.R
# Env: PORT (default 8000), ALLOWED_ORIGIN (default http://localhost:5173)

library(plumber)

args = commandArgs(FALSE)
here = dirname(normalizePath(sub("^--file=", "", args[grepl("^--file=", args)][1])))
Sys.setenv(STS_ROOT = normalizePath(file.path(here, "..", "..")))
port = as.integer(Sys.getenv("PORT", unset = "8000"))
plumber::plumb(file.path(here, "plumber.R"))$run(host = "0.0.0.0", port = port)
