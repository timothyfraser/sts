#' @name setup_this_folder.R
#' @description 
#' To setup this folder, run this code to unzip the Bluebikes database into this folder.

# Set working directory
setwd("/cloud/project")
# Unzip into folder
unzip(zipfile = "data/bluebikes/bluebikes.zip", junkpaths = TRUE, exdir = "workshops/28C_datacom")
# Reset working directory to this folder
setwd("/cloud/project/workshops/28C_datacom")
# Check contents
dir()
# Great!
