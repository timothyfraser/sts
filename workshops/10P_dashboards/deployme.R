# deployme.R

install.packages("rsconnect")
library(rsconnect)
rsconnect::writeManifest(appDir = "/cloud/project/workshops/10P_dashboards", appMode = "shiny")
