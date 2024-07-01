# Lesson 12: Mapping Social Infrastructure
# Prof: Tim Fraser
# Read code here: https://rpubs.com/timothyfraser/mapping_social_infra

# Load Packages
library(dplyr)
library(readr)
library(tidyr)
library(viridis)
library(GGally)
# Mapping packages
library(sf)
library(ggspatial)

# Set working directory to this folder
setwd(paste0(rstudioapi::getActiveProject(), "/workshops/12C_databases_4"))


# Load Data
mypoints <- read_sf("boston_social_infra.geojson") 
myshapes <- read_sf("boston_grid.geojson")
mydata <- read_csv("boston_census_data.csv")
mylines <- read_sf("boston_train_lines.geojson")


# Follow along with the code shown in this guide.
# https://rpubs.com/timothyfraser/mapping_social_infra
# I wrote this a couple years back, but I think it's still helpful!