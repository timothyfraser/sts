#' @name 24P_analytics/lab.R
#' @title Practice: Network Analytics with a Transit Dataset
#' @author Tim Fraser
#' @description
#'
#' Urban transit networks are a classic example of a socio-technical system:
#' physical infrastructure (rails, buses, stops) shaped by social decisions
#' about who deserves access, where to invest, and how to move people equitably.
#' By analyzing ridership flows as a directed, weighted network, we can ask
#' questions that matter for planners — which stations are true hubs,
#' which neighborhoods are well-connected, and where mobility gaps exist.
#'
#' In this practice script, you will apply the tidygraph toolkit from 24C
#' to explore the Riverdale Metro & Bus network.

# 0. SETUP ############################################

library(dplyr)
library(readr)
library(tidygraph)
library(ggraph)
library(ggplot2)
library(purrr)

# Load helper visualization functions
source("workshops/24P_analytics/functions.R")

edges = read_csv("workshops/24P_analytics/edges.csv")
nodes = read_csv("workshops/24P_analytics/nodes.csv")

# Build the tidygraph object
g = tbl_graph(nodes = nodes, edges = edges, directed = TRUE, node_key = "name")

# Preview the network
g

# Plot the full network using geographic coordinates
illustrate_unipartite(graph = g, layout = "coords")





# 1. In-Degree: Which stations receive the most riders? #####
##
## In a directed network, *in-degree* counts how many edges point INTO a node.
## Weighted in-degree sums the weights of those incoming edges —
## in our case, total annual riders arriving at a station.


## LC 1.1 --------------------------------------------------------
# Which station has the highest total incoming ridership across all routes?
# Report the top 5 stations by total riders arriving.
# Hint: activate the edges, use .N()$name[ .E()$to ] to get destination names,
# then group_by() and summarize().



## LC 1.2 --------------------------------------------------------
# Does the top-receiving station change when you look only at bus edges?
# Filter the graph to bus edges only, then re-calculate total incoming ridership.
# How does the ranking shift compared to LC 1.1?



# 2. Local Neighborhood: How many stops can you reach? ######
##
## to_local_neighborhood() filters the graph to nodes within a given number
## of 'hops' (degrees of separation) from a focal node.
## This is useful for understanding a station's *reachability* in the network —
## how many destinations are accessible without too many transfers?


## LC 2.1 --------------------------------------------------------
# From fernwood (a suburb station), how many distinct stations can you reach
# within 2 stops (order = 2), traveling in any direction (mode = "all")?
# Exclude fernwood itself from your count.
# Visualize the neighborhood subgraph with illustrate_unipartite().



## LC 2.2 --------------------------------------------------------
# Repeat the same 2-stop reachability analysis starting from northgate.
# Does northgate or fernwood give you access to more stations in 2 hops?
# Why might that be, given the network structure?
# Visualize the neighborhood subgraph with illustrate_unipartite().



# 3. Node Criticality: Which station lies on the most routes? #########

## A *critical* node is one whose removal would disconnect or lengthen many
## journeys. One way to probe criticality with tools we already know is to
## compare how quickly a station's neighborhood *expands* as we increase
## the order of to_local_neighborhood(). A hub's neighborhood should grow
## rapidly because it connects many spokes; a peripheral station's neighborhood
## grows slowly because it sits at the end of a line.


## LC 3.1 --------------------------------------------------------
# Compare union_terminal and northgate using to_local_neighborhood().
# For each station, count how many stations are reachable at order = 1,
# then at order = 2 (mode = "all" for both).
# How much does each station's neighborhood grow between order 1 and order 2?
# What does a large jump tell you about a station's structural position?
# Visualize at least one neighborhood subgraph with illustrate_unipartite().



## LC 3.2 --------------------------------------------------------
# Now flip the question: how many stations feed directly INTO union_terminal
# (i.e. are directly connected to it as a destination)?
# Use to_local_neighborhood() with order = 1 and mode = "in"
# to find all stations that send edges directly into union_terminal.
# Then do the same for ironworks. Which station has more direct feeders?
# Visualize each neighborhood with illustrate_unipartite().



# 4. Shortest Paths: How do riders get from A to B? #########
##
## to_shortest_path() extracts the subgraph of the single shortest path
## between two nodes. This lets us ask not just *how long* a journey is,
## but *what riders experience along the way*.


## LC 4.1 --------------------------------------------------------
# Find the shortest path (fewest hops) from oldbury to depot_north.
# How many legs does the journey involve?
# List all station names along the route, in order.
# Visualize the route with illustrate_unipartite().



## LC 4.2 --------------------------------------------------------
# Find the shortest path from airside to greenway (fewest hops).
# How many of the intermediate stations (excluding start and end)
# have high accessibility?
# What does this suggest about the equity of this route for riders
# who depend on accessible infrastructure?
# Visualize the route with illustrate_unipartite().



# 5. Zone-level Analysis: How does ridership vary by zone? ###########

## We can use .N()/.E() cross-referencing to pull node attributes onto edges,
## then use to_split() to compare network behavior across the
## downtown/midtown/uptown/suburb zones.


## LC 5.1 --------------------------------------------------------

# Add a new edge variable called from_zone that records the zone of each
# edge's origin station (using .N() and .E() indexing, as in 24C).
# Then summarize: what is the total outbound ridership leaving each zone,
# and how many routes leave each zone?
# Which zone generates the most outbound trips?


# CHALLENGE (OPTIONAL) ###########################################

# This question uses purrr::map(), which we haven't covered yet —
# give it a try if you're feeling adventurous!
# Alternatively, you could use lapply() or a for-loop to pull it off.
#
# Split the graph by zone using morph(to_split, zone, split_by = "nodes").
# For each zone subgraph, calculate the total ridership flowing INTO
# stations in that zone (sum of incoming edge weights).
# Which zone receives the most total ridership?
# Does this match what you found in LC 5.1, or is the pattern different?



# Conclusion ####################################################

# In this script, you used the tidygraph toolkit to explore a directed,
# weighted urban transit network. You practiced pulling node attributes onto
# edges with .N()/.E() indexing, measuring reachability with
# to_local_neighborhood(), tracing routes with to_shortest_path(),
# and summarizing ridership by zone. Next week, we'll add centrality
# functions — degree, betweenness, and closeness — to make these
# structural comparisons even more precise.
