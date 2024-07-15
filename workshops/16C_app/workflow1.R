#' @name workflow.R
#' @description
#' Script for testing out your app workflow.

# Check working directory
getwd()
# Set working directory to your new folder
setwd("workshops/16C_app")

library(sf)
library(dplyr)
library(ggplot2)

# Get Boston neighborhoods
# Downloaded from here
# https://data.boston.gov/dataset/boston-neighborhood-boundaries-approximated-by-2020-census-block-groups1/resource/c9663e7a-84c2-435c-91c0-91cdce1ee5ac
neighborhoods = read_sf("boston_neighborhoods.geojson") %>%
  select(name = blockgr2020_ctr_neighb_name, geometry) %>%
  # Narrow to just inner Boston neighborhoods for which we have good point data
  filter(!name %in% c("West Roxbury", "Roslindale", "Mattapan", "Hyde Park",
                      "Brighton", "Allston", "Charlestown", "East Boston", "Harbor Islands"))

# Get social infrastructure sites
points = read_sf("boston_social_infra.geojson")

input = list(area = c("Dorchester", "Roxbury"), type = c("Community Spaces", "Parks"))

# Filter polygons and points
polygons_poi = neighborhoods %>% filter(name %in% input$area)
points_poi = points %>% filter(group %in% input$type)

# Test run
gg0 = ggplot() +
  geom_sf(data = neighborhoods) +
  geom_sf(data = points, alpha = 0.5) +
  geom_sf_label(data = neighborhoods, mapping = aes(label = name))
gg0

# Find a nice crop zone
gg0 +
  coord_sf(xlim = c(-71.14, -71.01),
           ylim = c(42.37, 42.28))

# Save it and use it in the app
xlim = c(-71.14, -71.01)
ylim = c(42.37, 42.28)


# Plot the full map, highlighting the neighborhood
gg = ggplot() +
  geom_sf(data = neighborhoods) +
  geom_sf(data = points_poi, alpha = 0.5) +
  geom_sf(data = polygons_poi, fill = NA, linewidth = 1.5, color = 'dodgerblue') +
  geom_sf_label(data = neighborhoods, mapping = aes(label = name)) +
  theme_bw(base_size = 14) +
  coord_sf(xlim = xlim, ylim = ylim)

gg
