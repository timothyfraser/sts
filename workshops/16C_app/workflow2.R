#' @name workflow2.R
#' @title Workflow for a more advanced spatial app
#' @description
#' Let's try building out a workflow that develops several data derivatives
#' using several spatial processes.

# Packages
library(dplyr) # data wrangling
library(readr) # reading in data
library(ggplot2) # visualization
library(viridis) # color palettes
library(sf) # spatial data
library(ggspatial) # for map annotations


# DATA #####################################

# Get our starter data, loaded at app startup

# Check working directory
getwd()
# Set working directory to our app folder
setwd("workshops/16C_app")

# Get Boston neighborhoods (https://data.boston.gov/dataset/boston-neighborhood-boundaries-approximated-by-2020-census-block-groups1/resource/c9663e7a-84c2-435c-91c0-91cdce1ee5ac)
neighborhoods = read_sf("boston_neighborhoods.geojson") %>%
  select(name = blockgr2020_ctr_neighb_name, geometry) %>%
  # Narrow to just inner Boston neighborhoods for which we have good point data
  filter(!name %in% c("West Roxbury", "Roslindale", "Mattapan", "Hyde Park",
                      "Brighton", "Allston", "Charlestown", "East Boston", "Harbor Islands"))

# Get social infrastructure sites
points = read_sf("boston_social_infra.geojson")

# Get our default coordinate box 
xlim = c(-71.14, -71.01)
ylim = c(42.37, 42.28)

# Let's get a default list of categories
categories = tibble(
  group = c("Community Spaces", "Parks", "Places of Worship", "Social Business")
) # notice we skipped the "Other" category - we don't want to visualize it.

# Streets!
streets = read_sf("streets.geojson")
object.size(streets) / 1e6 # 5 MB!


# GOALS ###################################

# - Let's make a bar plot that counts up the total sites per selected type, 
# for whatever areas are selected

# - Let's make a map that visualizes just the selected types in selected areas



# INPUTS ###############################

# In this section, let's add an `input` list, to match our inputs in the app
input = list(type = c("Parks", "Community Spaces"),
             area = c("Roxbury", "Dorchester") )



# REACTIVE #############################

# In this section, we'll develop any reactive data for later.

# Let's write a sub-workflow that will count up
# all the social infrastructure sites of types A, B, and C
# located in polygons X, Y, and Z

# Narrow into just the points in those areas
poi = points %>%
  mutate(group = factor(group)) %>%
  # Just social infrastructure sites in our selected types
  filter(group %in% input$type) %>%
  # Spatially filter by just these neighborhoods
  st_join(
    y = neighborhoods %>% 
      filter(name %in% input$area) %>%
      select(name, geometry), 
    left = FALSE)

# Get total tally of sites per type
tally = poi %>% 
  as_tibble() %>% 
  group_by(group) %>%
  summarize(count = n()) %>%
  # But we want to compare against ALL categories, right?
  # So let's join this into the set of all categories
  right_join(by = "group", y = categories) %>%
  # And fill in NAs with 0, since they are not present
  mutate(count = case_when(is.na(count) ~ 0, TRUE ~ count))


# VISUALS ###############################

## gg_bars ################################

# update when:
# -- tally changes

gg_bars = ggplot() +
  geom_col(data = tally, mapping = aes(x = group, y = count, fill = group)) +
  # Add text labels with the 'label' aesthetic
  geom_text(
    data = tally, mapping = aes(x = group, y = count, label = count),
    # Static traits
    vjust = 0, # Vertically justify to the bottom (0)
    nudge_y = 1 # bump up by 1 points on y axis
  ) +
  theme(legend.position = "none") 

gg_bars # View it

## gg_map ################################

# Quickly filter the polygons by name
polygons_poi = neighborhoods %>% 
  filter(name %in% input$area)

# Find the bounding box (bb) of your new polygons
bb = polygons_poi %>%
  st_bbox() 

# You can query it like... 
bb$xmin

# Turn it into a literal polygon box as an sf feature
box = bb %>% 
  st_as_sfc() %>% # turn into a geometry
  tibble(geometry = .) %>% # put in a tibble
  st_as_sf() # turn into a spatial data.frame

# Get just streets in the box
streets_box = streets %>%
  st_crop(y = box) # Crop these spatial features to just those in the box

# Show specific neighborhoods 
gg_map = ggplot() +
  geom_sf(data = streets_box, color = "grey") +
  geom_sf(data = polygons_poi, fill = NA, color = "dodgerblue", linewidth = 2) +
  geom_sf(data = poi, mapping = aes(fill = group),
          shape = 21, color = "white", size = 5) +
  theme_void() + # clean map
  theme(legend.position = "bottom") +
  # Crop to filtered polygons
  # coord_sf(xlim = c(bb$xmin, bb$xmax),
  #          ylim = c(bb$ymin, bb$ymax)) +
  # Add a map scale and arrow
  ggspatial::annotation_scale(location = "bl") + # bottom left (bl)
  ggspatial::annotation_north_arrow(location = "br") # bottom right (br)

# View it!
gg_map



# All done!
rm(list = ls())
