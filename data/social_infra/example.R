#' @name social_infra/example.R
#' @author Tim Fraser
#' @description
#' This data comes from a new study, describing 25 cities' 
#' social infrastructure rates for every city block.
#' 
#' This data is not yet released publicly, so please do not share :)

library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(viridis)

# Load in the grid
grid = rviridisgrid = read_sf("data/social_infra/tally1km.geojson") %>%
  filter(name == "nyc")

# Plot the grid
ggplot() +
  geom_sf(data = grid, mapping = aes(fill = log(park) )) +
  scale_fill_viridis(na.value = "black", option = "plasma")


rm(list = ls())
