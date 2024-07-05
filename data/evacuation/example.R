#' @name evacuation/example.rds
#' @title Evacuation Dataset for Hurricane Dorian
#' @description
#' Sourced from Fraser 2022 in Sustainability Science
#' Edges represent Facebook users going from City A to City B



# Getting Tabular Data #################################################
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(tidygraph)

# Since we haven't yet learned sf and sfnetworks in class,
# We're going to transform this data into just data.frames
# that will be more suitable for your temporal analysis.

# Import a spatial features network (sfnetwork object)
# This is 278 MB! That's a HUGE size!
read_rds("data/evacuation/dorian.rds") %>%
  # Focus on just the nodes of the network
  activate("nodes") %>%
  # Turn it into a tibble/data.frame
  as_tibble() %>%
  # Create a unique node id,
  # which we'll use to join in geoid to the from and to columns of the edges
  mutate(node = 1:n()) %>%
  # Reorder the variables; dropping geometry
  select(node, geoid, -geometry, social_capital:evacuation_less) %>%
  # Save the nodes
  saveRDS("nodes.rds")

# Load the nodes in
nodes = read_rds("data/evacuation/nodes.rds")

# Save the edges to file.
read_rds("data/evacuation/dorian.rds") %>%
  # Focus on just the edges of the network
  activate("edges") %>%
  # Turn it into a tibble/data.frame
  as_tibble() %>%
  # Drop the geometry 
  select(-geometry) %>%
  # Let's join the source (from) geoid in using the shared node id
  left_join(by = c("from" = "node"), y = nodes %>% select(node, from_geoid = geoid)) %>%
  # Let's join the destination (to) geoid in using the shared node id 
  left_join(by = c("to" = "node"), y = nodes %>% select(node, to_geoid = geoid)) %>%
  # Save this to file
  saveRDS("data/evacuation/edges.rds")

rm(list = ls())


# Temporal Analysis ###########################################

library(dplyr)
library(readr)
library(lubridate)


# How much does distance affect number of evacuees?

edges = read_rds("data/evacuation/edges.rds") %>%
  sample_n(size = 20000)

# date_time is a date-time formatted variable.
# This means the lubridate package can help us convert it
# to other formats easily.

edges %>%
  # Get the day of analysis.
  mutate(day = lubridate::day(date_time)) %>%
  # Get the month
  mutate(month = lubridate::month(date_time)) %>%
  # Get the hour
  mutate(hour = lubridate::hour(date_time)) 
  # Get hours since min time

# Get average evacuation over period at 8 AM, 4 PM, and 12 Midnight
edges %>%
  mutate(hour = lubridate::hour(date_time)) %>%
  group_by(from_geoid, to_geoid, hour) %>%
  summarize(evacuation = mean(evacuation, na.rm = TRUE))

# Etc.

rm(list = ls())

# Spatial Network Visualization #######################################
library(dplyr)
library(readr)
library(ggplot2)
library(sf)
library(tidygraph)

# Import a spatial features network (sfnetwork object)
# This is 278 MB! That's a HUGE size!
data = read_rds("data/evacuation/dorian.rds")

# Let's grab nodes from this network (cities / county-subdivisions)
nodes = data %>%
  # Focus on just the nodes of the network
  activate("nodes") %>%
  # Turn it into a tibble/data.frame
  as_tibble() %>%
  # Make it into a spatial data.frame
  st_as_sf()


# Let's grab edges from this network
edges = data %>%
  # Focus on just the edges of the network
  activate("edges") %>%
  # Turn it into a tibble/data.frame
  as_tibble() %>%
  # Make it into a spatial data.frame
  st_as_sf() %>%
  # Sample 1000 rows
  sample_n(size = 1000)

# Visualize spatial data using the geometry column of an sf object using geom_sf()
ggplot() +
  geom_sf(data = nodes, mapping = aes(fill = social_capital),
          shape = 21, color = "white", size = 4) +
  geom_sf(data = edges, mapping = aes(alpha = evacuation))


# Cleanup
rm(list = ls())


