#' @name dev_editor.R
#' @title Demo of Building a ShinyApp with an App Editor GUI
#' @author Tim Fraser
#' @description
#' RStudio has recently built a lightweight graphical user interface (GUI) for
#' building ShinyApps, called shinyuieditor. Let's try it out.

# Install remotes package, for remote download from github.
# install.packages("remotes")

# Install package from Github
remotes::install_github("rstudio/shinyuieditor")

# When prompted, choose to update all packages.

# Load Packages
library(shinyuieditor)

# Follow Along to Instructions Here
# https://rstudio.github.io/shinyuieditor/getting-started/

# Helpful Video Tutorial Here too
# https://www.youtube.com/watch?v=gYPnLiudtGU

# Check working directory
getwd()
# Set working directory to your new folder
setwd("workshops/16C_app")
# Launch the editor in that directory
launch_editor(app_loc = ".")
