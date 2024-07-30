#' @name 26C_analytics.R
#' @title Lesson: Network Analytics with Big Datasets (Part II)
#' @author Tim Fraser
#' @description
#' 
#' Networks are everywhere in Social Systems.
#' Many networks in social systems are actually two-mode networks,
#' like ties between members and committees.
#' How can we evaluate key quantities of interest in these kinds of networks?


# 0. Setup #################################

## 0.1 Load Packages ##################################

# Let's load our packages 
library(dplyr) # data wrangling
library(readr) # reading data
library(ggplot2) # visualizing data

library(igraph) # working with graphs
library(tidygraph) # dplyr functions with graphs
library(ggraph) # for network layouts
library(purrr) # for iterative actions

#install.packages(c("igraph", "tidygraph", "ggraph", "purrr"))

## 0.2 Read in a tidygraph ######################

# Let's read in our tidygraph object
g = read_rds("data/committees/graph_bipartite.rds")


g

# Committee A - Member 1
# Committee A - Member 2
# Committee B - Member 3
# Committee B - Member 1

# View it
plot(g)

## 0.3 Viewing the Bipartite Graph #######################

# What does a bipartite network look like?

# We'd need to generate a graph layout - ggraph can help
layout = ggraph(graph = g, layout = "fr") %>%
  with(data)  %>%
  mutate(id = 1:n()) %>%
  # Return each node with its new x-y variables
  select(id, x, y, name, type)

layout %>% head(3)

# Extract edges, joining in coordinates from your layout.
edges = g %>%
  activate("edges") %>%
  as_tibble() %>%
  left_join(
    by = c("from" = "id"), 
    y = layout %>% select(id, from_x = x, from_y = y)) %>%
  left_join(
    by = c("to" = "id"),
    y = layout %>% select(id, to_x = x, to_y = y)
  )
  
# Plot edges with geom_segment() and nodes with geom_point()
ggplot() +
  geom_segment(
    data = edges, 
    mapping = aes(
      x = from_x, y = from_y,
      xend = to_x, yend = to_y)
  ) +
  geom_point(
    data = layout,
    mapping = aes(
      x = x, y = y, fill = type
    ),
    shape = 21, color = "white"
  )



# 1. Coaffiliation ###################################

## 1.1 Coaffilation Networks ###################

# We might want to make transformations to our graph,
# transforming our graph from one form into another,
# and then keep working with tidygraph.
# 
# For example, what if we want to make a coaffiliation graph?
# Eg. instead of committee - member edges,
# a graph of committee-committee edges, where edges = # of members in common
# or
# a graph of member-member edges, where edges = # of committees in common

# Load coaffiliation function
source("functions/coaffiliate.R")

# Get graph
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)


gco

# Let's quickly plot our coaffiliation network...
plot(gco)

# Let's see how many members in common
# for example,
# committee 1 and 23 share
gco %>%
  activate("edges") %>%
  filter(weight > 1)



## 1.2 Isolates ########################################

# Our biggest challenge is always graph size - 
# so we want to reduce our graph size as quickly as possible.

# Nodes that have no edges are called 'isolates'.
# We can remove the nodes using some custom filter functions from tidygraph.

# Narrow into just nodes that are isolated
gco %>%
  filter(node_is_isolated())

# This is equivalent to saying
# Narrow into just nodes with a weighted degree centrality of zero
gco %>%
  activate("nodes") %>%
  mutate(degree = centrality_degree(weights = .E()$weight )) %>%
  filter(degree == 0)




# 2. Quantities of Interest #######################################

# What are our main quantities of interest?

## 2.1 Distance To ###########################################

# How far apart are nodes from Node X?

# How far is each Committee from Committee 23?
gco %>% 
  activate("nodes") %>%
  mutate(steps = node_distance_to(nodes = which(.N()$name == "committee_23") )) %>%
  as_tibble()


# What's the average distance from a key node of interest?
gco %>% 
  activate("nodes") %>%
  filter(!node_is_isolated()) %>%
  mutate(steps = node_distance_to(nodes = which(.N()$name == "committee_23") )) %>%
  as_tibble() %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))


# If weighted, use this one.
# gco %>% mutate(steps = node_distance_to(nodes = 1, weights = .E()$weight))


# gco %>% 
#   mutate(steps = node_distance_to(nodes =  which(.N()$name == "committee_1"), 
#                                   weights = .E()$weight)) 



## 2.2 Centrality ###############################################

# How central is this node?
# How central is Committee 23?

### degree centrality - undirected #####################
# total edges going in or out of that node
gco %>%
  activate("nodes") %>%
  mutate(deg = centrality_degree(mode = "all"))



### weighted degree centrality - undirected #######################
# sum of edges (weights) going in or out of that node
gco %>%
  activate("nodes") %>%
  mutate(deg = centrality_degree(mode = "all", weights = .E()$weight ))



### betweenness centrality - undirected ##################
# number of shortest paths through graph that cross that node
# higher = more bridging capacity
# lower = lower bridging capacity
gco %>%
  activate("nodes") %>%
  mutate(betw = centrality_betweenness(directed = FALSE, weights = .E()$weight))

### VISUALIZE!! #####################

data = gco %>%
  activate("nodes") %>%
  mutate(
    deg = centrality_degree(mode = "all"),
    wdeg = centrality_degree(mode = "all", weights = .E()$weight),
    betw = centrality_betweenness(directed = FALSE, weights = .E()$weight)
  ) %>%
  as_tibble()

# What's the median degree?
data %>%
  summarize(median_deg = median(deg, na.rm = TRUE))

# Plot out the degree distribution and understand patterns.
ggplot() +
  geom_density(data = data, mapping = aes(x = deg, fill = "Degree"), alpha = 0.5) +
  geom_density(data = data, mapping = aes(x = wdeg, fill = "Weighted Degree"), alpha = 0.5) +
  geom_vline(mapping = aes(xintercept = 6, linetype = "Median Degree"))

#  geom_density(data = data, mapping = aes(x = betw, fill = "Betweenness")) 

# Get harder to calcualte on really big graphs





## 2.3 Clustering ############################################

# Do our points cluster together naturally?

# Fast-greedy algorithm is one of the faster clustering strategies -
# allows you to specify the number of groups, within limits
gco %>%
  mutate(community = group_fast_greedy(weights = .E()$weight, n_groups = 8))


plot(gco)

gco %>%
  filter(!node_is_isolated()) %>%
  mutate(community = group_fast_greedy(weights = .E()$weight, n_groups = 3))


# There are dozens of others. 
# Use a well established one, with clear justification,
# or don't use one at all. 
# A clustering algorithm without a clear justification is not useful.
gco %>%
  mutate(community = group_infomap() %>% factor() )


# Eg. How many nodes are in each community?
gco %>%
  mutate(community = group_infomap() %>% factor() ) %>%
  as_tibble() %>%
  group_by(community) %>%
  summarize(count = n())


### Tell me aggregate statistics by cluster! ##################

gco %>%
  filter(!node_is_isolated()) %>%
  mutate(community = group_fast_greedy(weights = .E()$weight, n_groups = 3)) %>%
  mutate(deg = centrality_degree(weights = .E()$weight, mode = "all")) %>%
  as_tibble() %>%
  group_by(community) %>%
  summarize(
    count = n(),
    mean = mean(deg, na.rm = TRUE),
    sd = sd(deg, na.rm = TRUE) )






# 3. Iterating Tidygraphs ###############################

# Suppose I have too much data.
# Maybe it's not feasible to analyze the whole network at once.
# I could create a variable to split up my edges...
# Then split the graph up into several smaller graphs

# Load helper functions
source("functions/bind_graphs_list.R")
source("functions/graph_join_list.R")

# Get graph
g = read_rds("data/committees/graph_bipartite.rds")



g2 = g %>%
  activate("edges") %>%
  mutate(from_geo = .N()$geography[ .E()$from  ]    ) %>%
  # Split the graph into multiple graphs, 
  # using the from_geo() edges variable
  morph(to_split, from_geo, split_by = "edges")


# g2 = g %>%
#   activate("edges") %>%
#   mutate(from_geo = .N()$geography[ .E()$from  ]    ) %>%
#   # Split the graph into multiple graphs, 
#   # using the from_geo() edges variable
#   to_split(from_geo, split_by = "edges")

g2

# Our graph has become a list of multiple graphs
g2 %>% names()
g2$`from_geo: iwate`

g2 %>% names()
class(g2)

# To do iterative actions on non-data.frame data,
# We can use the purrr package's map() function.
# This says, hey, perform some function/process on each item of my list, 
# and return a list of outputs.




## 3.1 Iterative Random Sampling ###############################

# Let's split the graph into a list of graphs,
# then map a sampling function to each graph.

glist = g %>%
  activate("edges") %>%
  mutate(from_geo = .N()$geography[ .E()$from  ]    ) %>%
  # Split the graph into multiple graphs, using the from_geo() edges variable
  morph(to_split, from_geo, split_by = "edges") %>%
  # For each graph, sample 30 edges randomly!
  map(~.x %>% activate("edges") %>% sample_n(size = 30) )


names(glist)

glist$`from_geo: iwate`



# Now bundle them back together into one cohesive graph
# bind_graphs_list() will repeat the nodes,
# and provide back every node variable.
# Note: you will get duplicate nodes from this method.
glist %>%
  bind_graphs_list(.id = "group")

# mimic the structure of bind_rows(.id = "group") in dplyr



# graph_join_list() will join every graph item together,
# using as many variables as provided in `by`.
# Note: you will only receive back the node variables in `by`.
# Note: you will not get duplicate nodes from this method.
glist %>%
  graph_join_list(by = c("name", "type"), .id = "group")






## 3.2 Iterative Coaffiliation ##############################


# Suppose I want to evaluate coaffiliation FOR MANY GROUPS.
# Maybe I can't evaluate it once for the ENTIRE network,
# but I could evaluate 'local' coaffiliation
# by narrowing into one geography or another.
# I could use our custom coaffiliate() function with purrr's map() function

# Load coaffiliation function
source("functions/coaffiliate.R")
source("functions/graph_join_list.R")

# Load graph
g = read_rds("data/committees/graph_bipartite.rds")

gmem = g %>%
  activate("edges") %>%
  mutate(from_geo = .N()$geography[ .E()$from  ]    ) %>%
  # Split the graph into multiple graphs, using the from_geo() edges variable
  morph(to_split, from_geo, split_by = "edges") %>%
  # For each subgraph, coaffiliate
  map(~coaffiliate(graph = .x, type = TRUE, names = TRUE, weight = "weight", diag = FALSE)) %>%
  # Join them all back together
  graph_join_list(by = "name", .id = "geography")

gmem

# Total committee-seats shared in common
gmem %>%
  activate("edges") %>%
  as_tibble() %>%
  summarize(total = sum(weight))


# Let's compare that against a standard coaffiliation graph
g %>%
  coaffiliate(type = TRUE, names= TRUE, weight = "weight", diag = FALSE) %>%
  activate("edges") %>%
  as_tibble() %>%
  summarize(total = sum(weight))
