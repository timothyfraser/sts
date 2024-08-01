#' @name 29C_databases.R
#' @title Lesson 29: Sampling Big Networks
#' @author Tim Fraser
#' @description
#' 
#' Often, when we are working with big data, 
#' we can't always work with the whole dataset. 
#' This is particularly true for network data, 
#' because of the N^2 problem,
#' which describes how an adjacency matrix contains
#' N x N cells where N is the number of nodes.
#' 
#' Once you get over 1000 nodes, it tends to be hard to work
#' with that network data in a local computing environment.
#'
#' So we need ways to work with samples of nodes and/or edges. 
#' What are some strategies we could use for taking meaningful samples?
#' 
#' To test this out, we will use the Hurricane Dorian evacuation network,
#' from Hurricane Dorian in Florida in September 2019.
#' The weights in this geospatial network refer to 
#' a count of local Facebook users,
#' which measures, compared to a pre-crisis baseline, 
#' how many more Facebook users were moving between city A and B
#' during every 8 hour period.

# 0. Setup ############################

## 0.1 Packages #########################
# Set directory
setwd("/cloud/project/")

library(dplyr) # data wrangling
library(readr) # reading data
library(ggplot2) # visualization
library(sf) # spatial data
library(igraph) # networks
library(tidygraph) # using dplyr functions on networks
library(ggraph) # network layouts


## 0.2 Data ##############################

# Our data includes...

# Nodes, with traits like:
#   node - unique ID from 1 to N
#   geoid - unique county subdivision (city) census ID
#   pop - population of city in 2019
#   median_income - median household income in a city in 2019 in USD
#   and many many more variables
nodes = read_rds("data/evacuation/nodes.rds")
nodes %>% glimpse()


# Edges, with traits like...
#   from - ID of source node (corresponds to 'node' in nodes.rds)
#   to - ID of destination node (corresponds to 'node' in nodes.rds)
#   date_time - date and time, in 8 hour chunks
#   evacuation - how many more/fewer local Facebook users went 
#              between city A and B than normal (pre-crisis)? 
#              Positive = evacuation
#              Negative = sheltering in place
#   km         - distance in kilometers between 'from' and 'to' cities
edges = read_rds("data/evacuation/edges.rds")
edges %>% glimpse()


# Let's remove these, since they are so big.
remove(nodes, edges)
# We'll load them in in smaller pieces.



# 1. Visualize It ##############################################


## 1.1 A Network Perspective #####################

# First, what does our network look like, anyways?
nodes = read_rds("data/evacuation/nodes.rds") %>%
  select(node, geoid, pop, median_income)
edges = read_rds("data/evacuation/edges.rds") %>%
  select(from, to, date_time, evacuation) %>%
  # Filter just to evacuation
  filter(evacuation > 0) %>%
  # Let's grab just one time slice
  filter(date_time == "2019-08-30 08:00:00")

nodes %>% head()
edges %>% head()

# Create a graph object
g = tbl_graph(nodes = nodes, edges = edges, directed = TRUE, node_key = "node") 
# View that graph quickly
plot(g, vertex.label=NA, vertex.size = 5)

# Cleanup
rm(list = ls())


## 1.2 A Spatial Perspective #################

# Let's load in our county subdivisions
points = read_sf("data/evacuation/county_subdivisions.geojson") %>%
  # Get centroid coordinates
  mutate(geometry %>% st_centroid() %>% st_coordinates() %>% 
           as_tibble() %>% select(x = 1, y = 2)) %>%
  # Get just the coordinates, for joining
  as_tibble() %>%
  select(geoid, x,y)

# Get state polygon
polygons = read_sf("data/evacuation/states.geojson") %>%
  # Narrow into florida
  filter(state == "FL")

# Get the nodes...
nodes = read_rds("data/evacuation/nodes.rds") %>%
  select(node, geoid, pop, median_income) %>%
  # Join in the x-y coordinates
  left_join(by = "geoid", y = points)

# Get the edges...
edges = read_rds("data/evacuation/edges.rds") %>%
  select(from, to, date_time, evacuation) %>%
  # Filter just to evacuation
  filter(evacuation > 0) %>%
  # Let's grab just one time slice
  filter(date_time == "2019-08-30 08:00:00") %>%
  # Join in the coordinates...
  left_join(by = c("from" = "node"), y = nodes %>% select(node, from_x = x, from_y = y)) %>%
  left_join(by = c("to" = "node"), y = nodes %>% select(node, to_x = x, to_y = y))

# Let's plot it.
ggplot() +
  # plot the state boundaries
  geom_sf(data = polygons, color = "black", fill = "#373737") +
  # plot the edges
  geom_segment(data = edges, mapping = aes(x = from_x, y = from_y,
                                           xend = to_x, yend = to_y),
               color = "dodgerblue", alpha = 0.5) +
  # plot the nodes
  geom_point(data = nodes, mapping = aes(x = x, y = y), 
             shape = 21, fill = "white", color = "dodgerblue") +
  # Apply a spatial coordinate reference system post-hoc
  coord_sf(crs = 4326)


# Okay.

## 1.3 Quantities of Interest #########################################

# Some sampling strategies are better than others. 
# Most may affect the traits of your network.
# Let's create a few methods we can use to measure traits about our network.
# We'll repeat these whenever we work on the network.

# Here are some simple quantities of interest we can measure about our network in aggregate.

stats = edges %>%
  summarize(
    # Get the total edgeweight (total evacuees)
    edgeweight = sum(evacuation, na.rm = TRUE),
    # Get the total number of county pairs
    n_edges = n(),
    # Get total number of nodes
    n_nodes = length(nodes$node),
    # Get total connected nodes
    n_nodes_linked = c(from, to) %>% unique() %>% length()
  )

# Remember, these attributes are all non-normalized,
# so they're not good for comparing against a network of a different size.

# How can we make normalized benchmarks?

stats = stats %>%
  mutate(
    # Network density
    density = 2 * n_edges / ( n_nodes * (n_nodes - 1) ),
    # Share of nodes that are linked
    pc_nodes_linked = n_nodes_linked / n_nodes,
    # Edges per node
    edge_ratio = n_edges / n_nodes,
    # Average Edgeweight per node
    avg_edgeweight = edgeweight / n_nodes
    )
stats

# We could also calculate any homophily measures, etc. for comparison.

# 2. Comparing Temporal Samples ########################################

## 2.1 Measuring Network Slices ###########################

# Let's use our benchmarks to compare our network temporally,
# taking samples of different time slices.

# Get nodes
nodes = read_rds("data/evacuation/nodes.rds") %>%
  select(node, geoid, pop, median_income)

# Then, let's use our edges to calculate some statistics per time slice.
stats = read_rds("data/evacuation/edges.rds") %>%
  select(from, to, date_time, evacuation) %>%
  # Filter just to evacuation
  filter(evacuation > 0) %>%
  # For each time slice...
  group_by(date_time) %>%
  # Calculate these outright and normalized statistics
  summarize(
    edgeweight = sum(evacuation, na.rm = TRUE),
    n_edges = n(),
    n_nodes = length(nodes$node),
    n_nodes_linked = c(from, to) %>% unique() %>% length(),
    density = 2 * n_edges / ( n_nodes * (n_nodes - 1) ),
    pc_nodes_linked = n_nodes_linked / n_nodes,
    edge_ratio = n_edges / n_nodes,
    avg_edgeweight = edgeweight / n_nodes
  )


# View the statistics summarizing our networks,
# where each row is a different network temporal slice
stats

## 2.2 Temporal Variation #############################

# Let's try and visualize some interesting trends

# How does average evacuation vary over time?
gg = ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = avg_edgeweight))

# Interesting - this suggests that some of our networks 
# capture more interesting variation than others.

# We can use geom_rect() to highlight specific sections of the graph,
# as long as we have a xmin, xmax, ymin, and ymax

# Here's a fairly homogenous - but not meaningful - section of the network over time.
# This is evacuation levels a few weeks after the disaster.
gg + 
  geom_rect(mapping = aes(xmin = as_datetime("2019-09-10 00:00:00"),
                          xmax = as_datetime("2019-10-30 00:00:00"),
                          ymin = 0, ymax = 600),
             fill = "grey", alpha = 0.5)
# So, while very regular, not good at showing the variation in the sample.


## 2.3 Filter by Time Randomly ####################################

# Suppose we randomly sampled just 20 of the time-slices of this network. 
# What story would result?
sample_rand = stats %>%
  sample_n(size = 20)

# Let's view the points and overall line trend.
gg +
  # Add the sampled points..
  geom_point(data = sample_qual,
             mapping = aes(x = date_time, y = avg_edgeweight), 
             color = "dodgerblue") +
  # Add a line connecting the points
  geom_line(data = sample_qual, 
            mapping = aes(x = date_time, y = avg_edgeweight), 
            color = "dodgerblue")

# So we show the range fairly well,
# but we miss the detail of the actual disaster event in entirety!

remove(afew)

## 2.4 Filtering by Time Qualitatively #################################

# Alternatively, maybe we should filter qualitatively,
# using a specific subjective rationale, like scope conditions
# Eg. it is more helpful for us to know evacuation 
# immediately around the crisis than randomly over time.

sample_qual = stats %>%
  filter(date_time <= "2019-09-10 00:00:00")

ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = avg_edgeweight)) +
  geom_line(data = sample_qual,
            mapping = aes(x = date_time, y= avg_edgeweight), 
            color = "dodgerblue")

ggplot() +
  geom_line(data = sample_qual,
            mapping = aes(x = date_time, y= avg_edgeweight), 
            color = "dodgerblue")



# 3. Comparing Sampling Strategies ############################

## 3.1 Ego-Centric Samples ###########################

# This is a two-stage sampler.
# First, you sample some nodes.
# Then, you find the nodes connected to those nodes.


# Let's get the full population of nodes.
nodes = read_rds("data/evacuation/nodes.rds") %>%
  select(node, geoid, pop, median_income)


# Let's sample some nodes randomly...
sampled_nodes = nodes %>%
  sample_n(size = 50)

# Then, let's use our edges to calculate some statistics per time slice.
edges = read_rds("data/evacuation/edges.rds") %>%
  select(from, to, date_time, evacuation) %>%
  # Filter just to evacuation
  filter(evacuation > 0) %>%
  # Filter to JUST source nodes OR destination nodes that were sampled
  filter(from %in% sampled_nodes$node | to %in% sampled_nodes$node)

# Produces a much smaller sample of edges.
# Let's check our stats.
sampled_stats = edges %>%
  # For each time slice...
  group_by(date_time) %>%
  # Calculate these outright and normalized statistics
  summarize(
    edgeweight = sum(evacuation, na.rm = TRUE),
    n_edges = n(),
    n_nodes = length(sampled_nodes$node),
    n_nodes_linked = c(from, to) %>% unique() %>% length(),
    density = 2 * n_edges / ( n_nodes * (n_nodes - 1) ),
    pc_nodes_linked = n_nodes_linked / n_nodes,
    edge_ratio = n_edges / n_nodes,
    avg_edgeweight = edgeweight / n_nodes
  )

# Let's view the results:

# Big chance in total edge weight - that makes sense.
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = edgeweight, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = edgeweight, color = "Ego-centric samples"))
  

# Nice! Very good alignment of average edgeweight in samples versus population.
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = avg_edgeweight, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = avg_edgeweight, color = "Ego-centric samples"))

# Some gap in share of nodes linked - but probably not as much as if we hadn't ego-centric sampled.
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = pc_nodes_linked, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = pc_nodes_linked, color = "Ego-centric samples"))

# Reduced edge ratio, but probably not as much as if we hadn't ego-centric sampled
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = edge_ratio, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = edge_ratio, color = "Ego-centric samples"))



## 3.2 Edgewise Sampling ##########################################

# In contrast, what happens if we sample just our edges instead?


# Let's get the full population of nodes.
nodes = read_rds("data/evacuation/nodes.rds") %>%
  select(node, geoid, pop, median_income)


# Grab a sample of edges
edges = read_rds("data/evacuation/edges.rds") %>%
  select(from, to, date_time, evacuation) %>%
  # Filter just to evacuation
  filter(evacuation > 0) %>%
  # Let's randomly sample 25000 edges 
  # about the number we got when we did an ego-centric sample
  sample_n(size = 25000)

# Which nodes ended up being sampled? Let's find out.
sampled_nodes2 = edges %>%
  reframe(node = c(from, to) %>% unique()) %>%
  left_join(by = "node", y= nodes)

# Okay, this method caught a LOT more nodes than the last one.
nrow(sampled_nodes) # ego-centric sampling
nrow(sampled_nodes2) # edgewise sampling

# Let's calculate 
sampled_stats2 = edges %>%
  # For each time slice...
  group_by(date_time) %>%
  # Calculate these outright and normalized statistics
  summarize(
    edgeweight = sum(evacuation, na.rm = TRUE),
    n_edges = n(),
    n_nodes = length(sampled_nodes2$node),
    n_nodes_linked = c(from, to) %>% unique() %>% length(),
    density = 2 * n_edges / ( n_nodes * (n_nodes - 1) ),
    pc_nodes_linked = n_nodes_linked / n_nodes,
    edge_ratio = n_edges / n_nodes,
    avg_edgeweight = edgeweight / n_nodes
  )



# Big change in total edge weight - that makes sense.
# Pretty similar to ego-centric sample though.
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = edgeweight, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = edgeweight, color = "Ego-centric samples")) +
  geom_line(data = sampled_stats2, mapping = aes(x = date_time, y = edgeweight, color = "Edgewise samples"))



# Edgewise samples match population almost perfectly in mean edge weight. That makes sense - it is a literal random sample of rows.
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = avg_edgeweight, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = avg_edgeweight, color = "Ego-centric samples")) +
  geom_line(data = sampled_stats2, mapping = aes(x = date_time, y = avg_edgeweight, color = "Edgewise samples"))


# Some gap in share of nodes linked - edgewise samples tend to match population more closely than ego-centric sample
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = pc_nodes_linked, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = pc_nodes_linked, color = "Ego-centric samples")) +
  geom_line(data = sampled_stats2, mapping = aes(x = date_time, y = pc_nodes_linked, color = "Edgewise samples"))


# Edgewise samples seems to deflate the edge ratio considerably, while ego-centric samples seem to inflate the edge rate.
ggplot() +
  geom_line(data = stats, mapping = aes(x = date_time, y = edge_ratio, color = "Population")) +
  geom_line(data = sampled_stats, mapping = aes(x = date_time, y = edge_ratio, color = "Ego-centric samples")) +
  geom_line(data = sampled_stats2, mapping = aes(x = date_time, y = edge_ratio, color = "Edgewise samples"))


# 4. Conclusion ##########################################

# To summarize, depending on how you sample your network, 
# the properties of your sample will change drastically.
# Be sure to consider what kind of trait you want to know about.

# Is it a node trait? Then try ego-centric sampling.
# Is it an edge trait? Then try edgewise sampling.
# Is it a network structure? Tough luck - it's hard to reproduce those. 
# You could try iteratively edgesampling, sort of like a random walk through your network.

# Great work! You've learn how to sample networks!!!

