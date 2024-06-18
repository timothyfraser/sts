# Script for testing


# Set your working directory to your app's folder
setwd(paste0(rstudioapi::getActiveProject(), "/workshops/app_nycflights"))

# You can use runapp to run a specific app.
# It can ONLY access data located within your app's folder, 
# so make sure you only reference data saved there.
shiny::runApp("app1.R")


