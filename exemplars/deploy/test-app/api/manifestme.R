# manifestme.R: write manifest.json for the Posit Connect route.
# From the repo root:
#   Rscript exemplars/deploy/test-app/api/manifestme.R
# Connect runs plumber.R directly, so the bundle is plumber.R plus data/.
# run.R is left out on purpose: it is only the container's start script.
# (Needs the rsconnect package; run again whenever the packages change.)

rsconnect::writeManifest(
  appDir = "exemplars/deploy/test-app/api",
  appMode = "api",
  appPrimaryDoc = "plumber.R",
  appFiles = c("plumber.R", "data/example.csv")
)

# The manifest copies each package's DESCRIPTION, including its authors' and
# maintainer's email addresses. Connect installs by Package + Version +
# Repository and never reads those fields, so drop them: this folder holds no
# email addresses at all.
path = "exemplars/deploy/test-app/api/manifest.json"
m = jsonlite::read_json(path)
for (p in names(m$packages)) {
  for (f in c("Authors@R", "Author", "Maintainer")) m$packages[[p]]$description[[f]] = NULL
}
jsonlite::write_json(m, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
