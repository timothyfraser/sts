# manifestme.R
# Write manifest.json for Posit Connect. From the repo root:
#   Rscript exemplars/api-plumber/manifestme.R
# The app dir is the repo root so data/ ships with the API.
# (Needs the rsconnect package; run once per dependency change.)

rsconnect::writeManifest(
  appDir = ".",
  appMode = "api",
  appPrimaryDoc = "exemplars/api-plumber/plumber.R",
  appFiles = c("exemplars/api-plumber/plumber.R",
               "data/jp_solar.csv",
               "data/boston_voting/precincts.geojson",
               "data/boston_voting/polling_places.geojson",
               "data/boston_voting/boston_votes.csv")
)
