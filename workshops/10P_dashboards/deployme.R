# deployme.R

# Be sure to re-install this package, as it is updated often
# install.packages("rsconnect")
library(rsconnect)

# Write a manifest file for your app
rsconnect::writeManifest(appDir = "/cloud/project/workshops/10P_dashboards", appMode = "shiny")

# Connect your Cloud Account
connectCloudUser(launch.browser = TRUE)

# Check which accounts you are connected to
rsconnect::accounts()

# Set any environmental variables you need, 
# or read them from an .env file to be more secure
Sys.setenv("VARNAME" = "BIRD")

# Deploy the app!
rsconnect::deployApp(
  appDir = "/cloud/project/workshops/10P_dashboards", 
  appName = "Test App",
  # List the names of any environmental variables you want to share
  envVars = c("VARNAME")
)

# Very quick!
