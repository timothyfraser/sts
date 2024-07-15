#' @name example.R
#' @description
#' Example script for how to use data in this folder.

library(dplyr)
library(readr)
library(sf)
library(ggplot)

# Load municipality polygons
muni = read_sf("data/japan/municipalities.geojson")

# Filter to just municipalities in Hokkaido prefecture
subset = muni %>%
  filter(pref == "Hokkaido")

ggplot()  +
  geom_sf(data = subset)


# Same idea applies for:
pref = read_sf("data/japan/prefectures.geojson")
regions = read_sf("data/japan/regions.geojson")
country = read_sf("data/japan/country.geojson")
