#' @name 24P_analytics/solutions.R
#' @title Solutions: Network Analytics with a Transit Dataset
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

# Solution 1:
# Grab the destination station name from the node table, attach it to each edge,
# then group and sum.
g %>%
  activate("edges") %>%
  mutate(to_name = .N()$name[ .E()$to ]) %>%
  as_tibble() %>%
  group_by(to_name) %>%
  summarize(in_riders = sum(weight)) %>%
  arrange(desc(in_riders)) %>%
  head(5)
# union_terminal dominates, as expected for a central hub.
# central_commons and ironworks are next — they sit at the intersection
# of both rail and bus routes.

# Visualize: plot the full network to see the hub structure
illustrate_unipartite(graph = g, layout = "coords")

# Solution 2:
# There's a helper function centrality_degree() that we'll learn next week.
# It can calculate this for every node at once!
g %>%
  activate("nodes") %>%
  mutate(in_riders = centrality_degree(weights = .E()$weight, mode = "in")) %>%
  as_tibble() %>%
  arrange(desc(in_riders)) %>%
  select(name, zone, accessibility, in_riders) %>%
  head(5)


## LC 1.2 --------------------------------------------------------
# Does the top-receiving station change when you look only at bus edges?
# Filter the graph to bus edges only, then re-calculate total incoming ridership.
# How does the ranking shift compared to LC 1.1?

# Solution 1:
g %>%
  activate("edges") %>%
  filter(line == "bus") %>%
  mutate(to_name = .N()$name[ .E()$to ]) %>%
  as_tibble() %>%
  group_by(to_name) %>%
  summarize(in_riders = sum(weight)) %>%
  arrange(desc(in_riders)) %>%
  head(5)
# central_commons rises to the top on bus-only ridership —
# it sits at the center of the midtown ring and several spoke-cutter routes.
# union_terminal drops because its high in-degree is driven by heavy rail lines.

# Visualize: plot just the bus subgraph to see why central_commons dominates
g %>%
  activate("edges") %>%
  filter(line == "bus") %>%
  illustrate_unipartite(layout = "coords")

# Solution 2:
g %>%
  activate("edges") %>%
  filter(line == "bus") %>%
  activate("nodes") %>%
  mutate(in_riders = centrality_degree(weights = .E()$weight, mode = "in")) %>%
  as_tibble() %>%
  arrange(desc(in_riders)) %>%
  select(name, zone, accessibility, in_riders) %>%
  head(5)


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

# Solution:
x = g %>%
  morph(
    to_local_neighborhood,
    node  = which(.N()$name == "fernwood"),
    order = 2,
    mode  = "all"
  )

x %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "fernwood") %>%
  summarize(reachable_stations = n())
# fernwood connects to harborview and ironworks in 1 stop,
# then fans out broadly through the midtown ring in 2 stops.

# Visualize: plot fernwood's 2-hop neighborhood
x %>%
  with(neighborhood) %>%
  illustrate_unipartite(layout = "coords")

remove(x)


## LC 2.2 --------------------------------------------------------
# Repeat the same 2-stop reachability analysis starting from northgate.
# Does northgate or fernwood give you access to more stations in 2 hops?
# Why might that be, given the network structure?

# Solution:
x = g %>%
  morph(
    to_local_neighborhood,
    node  = which(.N()$name == "northgate"),
    order = 2,
    mode  = "all"
  )

x %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "northgate") %>%
  summarize(reachable_stations = n())
# northgate reaches fewer stations — it sits at the end of the green line
# with only one rail neighbor (elmhurst) and one bus neighbor (riverside_park).
# fernwood, by contrast, connects into the midtown ring via bus routes,
# giving it much broader 2-hop reach despite both being suburb nodes.
# This shows how bus connectivity can compensate for peripheral rail position.

# Visualize: plot northgate's 2-hop neighborhood to confirm how sparse it is
x %>%
  with(neighborhood) %>%
  illustrate_unipartite(layout = "coords")

remove(x)


# 3. Node Criticality: Which station lies on the most routes? #############
##
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

# Solution:

# --- union_terminal, order 1 ---
x1 = g %>%
  morph(to_local_neighborhood,
        node = which(.N()$name == "union_terminal"),
        order = 1, mode = "all")

n1_order1 = x1 %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "union_terminal") %>%
  summarize(reachable = n()) %>%
  with(reachable)

# Visualize: union_terminal's 1-hop neighborhood
x1 %>%
  with(neighborhood) %>%
  illustrate_unipartite(layout = "coords")

remove(x1)

# --- union_terminal, order 2 ---
x2 = g %>%
  morph(to_local_neighborhood,
        node = which(.N()$name == "union_terminal"),
        order = 2, mode = "all")

n1_order2 = x2 %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "union_terminal") %>%
  summarize(reachable = n()) %>%
  with(reachable)

# Visualize: union_terminal's 2-hop neighborhood — notice how much it expands
x2 %>%
  with(neighborhood) %>%
  illustrate_unipartite(layout = "coords")

remove(x2)

# --- northgate, order 1 ---
x3 = g %>%
  morph(to_local_neighborhood,
        node = which(.N()$name == "northgate"),
        order = 1, mode = "all")

n2_order1 = x3 %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "northgate") %>%
  summarize(reachable = n()) %>%
  with(reachable)

remove(x3)

# --- northgate, order 2 ---
x4 = g %>%
  morph(to_local_neighborhood,
        node = which(.N()$name == "northgate"),
        order = 2, mode = "all")

n2_order2 = x4 %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "northgate") %>%
  summarize(reachable = n()) %>%
  with(reachable)

# Visualize: northgate's 2-hop neighborhood — compare its sparseness to union_terminal
x4 %>%
  with(neighborhood) %>%
  illustrate_unipartite(layout = "coords")

remove(x4)

# Compare the growth side by side
tibble(
  station = c("union_terminal", "northgate"),
  order1  = c(n1_order1, n2_order1),
  order2  = c(n1_order2, n2_order2),
  growth  = c(n1_order2 - n1_order1, n2_order2 - n2_order1)
)
# union_terminal's neighborhood explodes from order 1 to order 2 —
# each of its many direct neighbors pulls in its own spoke of stations.
# northgate grows much more slowly: it has few direct neighbors,
# so there is little to fan out from at order 2.
# Rapid neighborhood growth is a practical signal of hub-like criticality.


## LC 3.2 --------------------------------------------------------
# Now flip the question: how many stations feed directly INTO union_terminal
# (i.e. are directly connected to it as a destination)?
# Use to_local_neighborhood() with order = 1 and mode = "in"
# to find all stations that send edges directly into union_terminal.
# Then do the same for ironworks.
# Which station has more direct feeders?

# Solution:
x_union = g %>%
  morph(
    to_local_neighborhood,
    node  = which(.N()$name == "union_terminal"),
    order = 1,
    mode  = "in"   # edges pointing INTO union_terminal
  )

x_union %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "union_terminal") %>%
  summarize(direct_feeders = n())

# Visualize: stations that feed directly into union_terminal
x_union %>%
  with(neighborhood) %>%
  illustrate_unipartite(layout = "coords")

remove(x_union)

x_iron = g %>%
  morph(
    to_local_neighborhood,
    node  = which(.N()$name == "ironworks"),
    order = 1,
    mode  = "in"
  )

x_iron %>%
  with(neighborhood) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(name != "ironworks") %>%
  summarize(direct_feeders = n())

# Visualize: stations that feed directly into ironworks
x_iron %>%
  with(neighborhood) %>%
  illustrate_unipartite(layout = "coords")

remove(x_iron)
# union_terminal has more direct feeders than ironworks —
# it receives from the full set of rail terminuses plus several bus routes.
# ironworks is a secondary hub: important within the midtown bus ring,
# but fed by fewer distinct origins.

# Preview — Solution 2:
# Next week we'll learn centrality_betweenness(), which formally counts
# how many shortest paths between ALL pairs of nodes pass through each station.
# High betweenness = high structural criticality across the whole network.
g %>%
  activate("nodes") %>%
  mutate(betw = centrality_betweenness(weights = .E()$weight, directed = TRUE)) %>%
  as_tibble() %>%
  arrange(desc(betw)) %>%
  select(name, zone, accessibility, betw) %>%
  head(5)


# 4. Shortest Paths: How do riders get from A to B? #########

##
## to_shortest_path() extracts the subgraph of the single shortest path
## between two nodes. This lets us ask not just *how long* a journey is,
## but *what riders experience along the way*.


## LC 4.1 --------------------------------------------------------
# Find the shortest path (fewest hops) from oldbury to depot_north.
# How many legs does the journey involve?
# List all station names along the route, in order.

# Solution:
x = g %>%
  morph(
    to_shortest_path,
    from = which(.N()$name == "oldbury"),
    to   = which(.N()$name == "depot_north"),
    mode = "out"
    # No weights argument = fewest hops (unweighted shortest path)
  )

# Count the legs (each edge = one leg between adjacent stations)
x %>%
  with(shortest_path) %>%
  activate("edges") %>%
  as_tibble() %>%
  summarize(legs = n())

# List all stations on the route
x %>%
  with(shortest_path) %>%
  activate("nodes") %>%
  as_tibble() %>%
  select(name, zone, accessibility)

# Visualize: plot the route on the geographic map
x %>%
  with(shortest_path) %>%
  illustrate_unipartite(layout = "coords")

remove(x)


## LC 4.2 --------------------------------------------------------
# Find the shortest path from airside to greenway (fewest hops).
# How many of the intermediate stations (excluding start and end)
# have high accessibility?
# What does this suggest about the equity of this route for riders
# who depend on accessible infrastructure?

# Solution:
x = g %>%
  morph(
    to_shortest_path,
    from = which(.N()$name == "airside"),
    to   = which(.N()$name == "greenway"),
    mode = "out"
  )

x %>%
  with(shortest_path) %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(!name %in% c("airside", "greenway")) %>%
  group_by(accessibility) %>%
  summarize(count = n())
# If intermediate stops are mostly medium or low accessibility,
# a rider who depends on elevators or step-free access faces barriers
# mid-journey even if their origin and destination are accessible.
# Planners often focus on endpoint accessibility but overlook transfer points.

# Visualize: plot the airside -> greenway route
x %>%
  with(shortest_path) %>%
  illustrate_unipartite(layout = "coords")

remove(x)


# 5. Zone-level Analysis: How does ridership vary by zone? #####
##
## We can use .N()/.E() cross-referencing to pull node attributes onto edges,
## then use to_split() to compare network behavior across the
## downtown/midtown/uptown/suburb zones.


## LC 5.1 --------------------------------------------------------
# Add a new edge variable called from_zone that records the zone of each
# edge's origin station (using .N() and .E() indexing, as in 24C).
# Then summarize: what is the total outbound ridership leaving each zone,
# and how many routes leave each zone?
# Which zone generates the most outbound trips?

# Solution:
g %>%
  activate("edges") %>%
  mutate(from_zone = .N()$zone[ .E()$from ]) %>%
  as_tibble() %>%
  group_by(from_zone) %>%
  summarize(
    total_outbound_riders = sum(weight),
    n_routes              = n()
  ) %>%
  arrange(desc(total_outbound_riders))
# downtown generates the most outbound ridership by a large margin.
# suburb zones generate the least — consistent with a hub-and-spoke model
# where suburbs feed into the center but have few direct inter-suburb routes.
# Note that n_routes also varies: downtown has more routes leaving it,
# so some of its ridership advantage is structural, not just demand.

# Visualize: plot the full network — zone structure is visible geographically
illustrate_unipartite(graph = g, layout = "coords")


# CHALLENGE (OPTIONAL) ###########################################

# This question uses purrr::map(), which we haven't covered yet —
# give it a try if you're feeling adventurous!
# Alternatively, you could also use lapply or a for-loop to pull it off.
#
# Split the graph by zone using morph(to_split, zone, split_by = "nodes").
# For each zone subgraph, calculate the total ridership flowing INTO
# stations in that zone (sum of incoming edge weights).
# Which zone receives the most total ridership?
# Does this match what you found in LC 5.1, or is the pattern different?

# Solution:
g %>%
  activate("edges") %>%
  mutate(to_zone = .N()$zone[ .E()$to ]) %>%
  morph(to_split, to_zone, split_by = "edges") %>%
  map(~{
    .x %>%
      activate("edges") %>%
      as_tibble() %>%
      summarize(
        zone            = to_zone[1],
        total_in_riders = sum(weight),
        n_routes        = n()
      )
  }) %>%
  bind_rows() %>%
  arrange(desc(total_in_riders))
# downtown receives the most inbound ridership —
# union_terminal and depot_north pull in massive rail flows.
# Comparing LC 5.1 and 5.2: downtown both sends AND receives the most,
# reflecting its role as the dominant hub in both directions.
# suburb zones receive relatively little inbound ridership,
# suggesting most suburb-bound travel is return commuting on a few corridors.

# Visualize: the full network again as a reference for interpreting zone flows
illustrate_unipartite(graph = g, layout = "coords")


# Conclusion ####################################################

# In this script, you used the tidygraph toolkit to explore a directed,
# weighted urban transit network. You practiced pulling node attributes onto
# edges with .N()/.E() indexing, measuring reachability with
# to_local_neighborhood(), tracing routes with to_shortest_path(),
# and summarizing ridership by zone. Next week, we'll add centrality
# functions — degree, betweenness, and closeness — to make these
# structural comparisons even more precise.
