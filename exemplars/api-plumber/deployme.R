# deployme.R
# Deploy the STS API to Posit Connect. From the repo root:
#   Rscript exemplars/api-plumber/deployme.R
# Reads CONNECT_SERVER, CONNECT_API_KEY, CONNECT_TITLE, ALLOWED_ORIGIN from .env
# (never committed; copy .env.example). Set ALLOWED_ORIGIN on Connect to your
# deployed React origin.

library(rsconnect)
readRenviron(".env")

SERVER_NAME = "connect-sts"
rsconnect::addServer(url = Sys.getenv("CONNECT_SERVER"), name = SERVER_NAME)
rsconnect::connectApiUser(server = SERVER_NAME, apiKey = Sys.getenv("CONNECT_API_KEY"), account = "sts")

if (!file.exists("manifest.json")) source("exemplars/api-plumber/manifestme.R")

rsconnect::deployAPI(
  api = ".",
  server = SERVER_NAME,
  account = "sts",
  appPrimaryDoc = "exemplars/api-plumber/plumber.R",
  appName = Sys.getenv("CONNECT_TITLE", unset = "sts-api-plumber"),
  forceUpdate = TRUE
)
