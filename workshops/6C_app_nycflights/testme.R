# Script for testing


# Set your working directory to your app's folder
setwd(paste0(rstudioapi::getActiveProject(), "/workshops/6C_app_nycflights"))

# You can use runapp to run a specific app.
# It can ONLY access data located within your app's folder, 
# so make sure you only reference data saved there.
shiny::runApp("app1.R")

shiny::runApp("app2.R")

shiny::runApp("app3.R")

shiny::runApp("app4.R")

shiny::runApp("app5.R")
