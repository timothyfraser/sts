#' @name 21C_databases.R
#' @author Tim Fraser
#' @description 
#' 
#' 

#' Networks are everywhere in Social Systems.
#' 
#' Transit networks, mobility networks, social networks, 
#' food networks, distribution networks, electrical networks,
#' political networks, financial networks - you name it.
#' 
#' Any data that can be encoded as nodes and edges connecting those nodes is a network.
#' 
#' The problem is, networks are very large by definition,
#' and so storing and computing attributes about them
#' can be computationally intensive.
#' 
#' Each network is, essentially, an adjacency matrix
#' of n rows x n columns for n nodes,
#' where the values of each cell encode the edge weights.
#' 
#' This means, when working with big data and databases,
#' you will always be trying to figure out: 
#' - How do I work *efficiently** with network data?
#' - How do I take a meaningful sample of a network?
#' - How do I measure a meaningful quantity about my sample of the network?
#' - What am I missing about the network because I looked at just a sample?
#' 
#' Today, we're going to learn some tools for handling network data in R.

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

## 0.2 Data #############################################


# Today, we'll work with the Japanese Disaster Recovery Committees Dataset.

# This is a committee membership list, 
# for many committees set up to rebuild Japanese towns and cities 
# after the 3.11 (2011) tsunami, earthquake, and nuclear crisis.

# Membership lists are a common form of big data that make networks.

# eg. bank - account relationships
# eg. funder - contracter relationships
# eg. platform - user relationships

# These are all bipartite networks - 2 part networks.
# They involve nodes of 2 types.
# In a bipartite network, 
# you only have edges than link nodes of type A to nodes of type B.

# In our network, 
# type A - is committees.
# type B - is members.

# Let's load in their data.

# Read in committee traits
co = read_csv("data/committees/committees.csv") 
# Read in member traits
mem = read_csv("data/committees/members.csv")

# Read in the membership list - called the edge-list.
edges = read_csv("data/committees/edgelist.csv")


## 0.3 Making a tidygraph ###############################

# Bundle the nodes together
# make sure they share the same name for their unique ID column and type column
# distinguishing the committees as committees and the members as members
nodes = bind_rows(co, mem)

# Let's make a tidygraph object out of this.
g = tbl_graph(nodes = nodes, edges = edges, directed = FALSE, node_key = "name")

# Let's save this tidygraph object as a compressed .rds file
g %>% write_rds("data/committees/graph_bipartite.rds", compress = "gz")


# Cleanup
rm(list = ls())



## 0.4 Functions ########################################

# I've made some custom helper functions for us,
# that will simplify the process of working with tidygraph iteratively.
# You can run the code and load the functions in locally,
# using the source() command.

source("functions/coaffiliate.R")
source("functions/bind_graphs_list.R")
source("functions/graph_join_list.R")

# Check your environment. You should now have several custom functions loaded.


# 1. Using Tidygraph ######################################

## 0.1 Read in a tidygraph ######################

# Let's read in our tidygraph object
g = read_rds("data/committees/graph_bipartite.rds")

# The whole value added of tidygraph is to create a way to interact
# with graph objects made in 'igraph' - a common shared package in R and Python
# in a tidy, dplyr friendly way.

# Let's view our tidygraph
g

## 0.2 Activate() a tidygraph ######################

# It recognizes itself as a **bipartite** graph with:
# **nodes**
# and 
# **edges**

# If we activate() the nodes,
# we can do dplyr functions to them
g %>%
  activate("nodes") %>%
  # Filter to committees in Iwate OR members
  filter(geography == "iwate" | type == "member" )

# Notice how the number of edges has decreased too!

# If we activate() the edges,
# we can do dplyr functions to them
g %>%
  activate("edges") %>%
  # Filter to just committee 9
  filter(from == 9)

## 0.3 Querying a Tidygraph #################################

# Wouldn't it be much more helpful
# if we could filter edges based on the traits of nodes?

g %>%
  activate("edges") %>%
  mutate(from_geo = .E()$weight )

g %>%
  activate("nodes") %>%
  mutate(from_geo = .N()$type ) %>%
  select(name, type, from_geo)

# Create a new variable in the edges dataset, called 'from_geo'
# where we will get the geography variable from the nodes .N()
# but only for the nodes which are listed in the edges .E()$from

g %>%
  activate("edges") %>%
  mutate(from_geo =  .N()$geography[ .E()$from   ] ) 

## 0.4 as_tibble() for Quantities of Interest ###########################

# We could then use as_tibble() to turn the nodes or edges into a data.frame...
# and then data wrangle some quantities of interest

g %>%
  activate("edges") %>%
  mutate(from_geo =  .N()$geography[ .E()$from   ] )  %>%
  as_tibble() %>%
  group_by(from_geo) %>%
  summarize(
    # Count up total memberships
    memberships = sum(weight),
    # Count up the unique members  
    people = to %>% unique() %>% length())


# What about the gender distribution of these committees?
# Does geography make a difference?
g %>%
  activate("edges") %>%
  mutate(from_geo =  .N()$geography[ .E()$from   ],
         to_gender = .N()$gender[ .E()$to ])  %>%
  as_tibble() %>% 
  # For each committee geography and each member gender category...
  group_by(from_geo, to_gender) %>%
  # Count up quantities of interest
  summarize(
    memberships = sum(weight),
    people = to %>% unique() %>% length()) 

# 2. Graph Transformations #############################


# tidygraph is full of transformer functions
# that take a graph as input and do various processes to it,
# helping you get a new derivative graph from it.

# These are typically called to_split, to_components, to_local_neighborhood, etc.

# We're going to learn how to use them below.


## 2.1 to_split() #######################################

# to_split() splits a graph into multiple graphs,
# specifically, into a list() of graph objects.

# It splits them using a variable, either from the nodes or edges data.frame.

### split by nodes #########################################

# Let's use the existing 'geography' variable from the nodes data.frame
# we'll split into one subgraph per level of geography
x = g %>%
  morph(to_split, geography, split_by = "nodes")

class(x) # it's a list
names(x) # it has these items
x$`geography: iwate` # you can query them like so
x %>% with(`geography: iwate`) # equivalent to $ sign

# cleanup
remove(x)




### split by edges #############################

# Split the graph into multiple graphs, using the `from_geo` edges variable
x = g %>%
  activate("edges") %>%
  mutate(from_geo = .N()$geography[ .E()$from  ]    ) %>%
  morph(to_split, from_geo, split_by = "edges")

class(x) # it's a list
names(x) # see the names of the list items
x$`from_geo: iwate` # query one
x %>% with(`from_geo: iwate`)   # this is equivalent to the $ sign

# cleanup
remove(x)

## 2.2 to_subcomponent() #############################################

# Narrow into the subcomponent of the graph which contains a specific node ID
# A subcomponent is a chunk of the graph with all its nodes and edges,
# having filtered out any nodes and edges
# that do not connect to nodes in that subcomponent.

x = g %>%
  morph(to_subcomponent, node = which(.N()$name == "committee_23" ))

class(x) # still a list
names(x) # see the name of its list item
x$subgraph
x %>% with(subgraph)   # this is equivalent to the $ sign


## 2.3 to_local_neighborhood() ################################

# Suppose we want to know,
# Hey, for committee 23,
# how many nodes are connected to it?
# We can use morph() with to_local_neighborhood()

### first-degree ties ##############################

# Filter the graph to edges
# of first degree ties to your committee
x = g %>%
  morph(
    to_local_neighborhood, # function 
    node = which(.N()$name == "committee_23"),  # which node ids?
    order = 1, # how many degrees of distance?
    mode = "all" # in-ward edges, out-ward edges, or all edges?
  )

names(x) # View names
x$neighborhood # access it
x %>% with(neighborhood) # access it

# Quick plot it
x %>% with(neighborhood) %>% plot()


# We can then count up the number of neighbors 
x %>% 
  with(neighborhood) %>%
  activate("edges") %>%
  as_tibble() %>% #   # Transform to tibble
  summarize(persons = to %>% unique() %>% length()) # count up total edges to neighbors


### 2nd-degree ties ###############################

# What if I want to know, 
# not who is connected to node X,
# but who is connected to the nodes that are connected to node X?
# Eg. second-order / second-degree contacts / neighbors

# We can still use to_local_neighborhood to do this.
x = g %>%
  morph(to_local_neighborhood, node = which(.N()$name == "committee_23"), mode = "all",
    order = 2 # 2 degrees of separation
  )

x %>% with(neighborhood) 

# Quick plot!
x %>% with(neighborhood)  %>% plot()


# Quantities of Interest

# How many different committees is committee 23 connected to?
# these are second-degree ties, 
# so just filter to committees, exclude committee 23, and count them up!
x$neighborhood %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(type == TRUE & name != "committee_23") %>%
  summarize(count = n())

# Or, how many committees from Miyagi Prefecture are linked to committee 23?
x$neighborhood %>%
  activate("nodes") %>%
  as_tibble() %>%
  filter(type == TRUE & name != "committee_23") %>%
  group_by(geography) %>%
  summarize(count = n())


### nth-degree ties ######################################

# I'm working with a bipartite network,
# so in a network where the origin is committee 23,
# degree 1 ties will be members of committee 23
# degree 2 ties will be committees
# degree 3 ties will be members of degree-2 committees
# degree 4 ties will be committees

# We can still use to_local_neighborhood to do this.
x = g %>%
  morph(to_local_neighborhood, node = which(.N()$name == "committee_23"), mode = "all",
        order = 4 # 2 degrees of separation
  )

x %>% with(neighborhood) 

# Quick plot!
x %>% with(neighborhood)  %>% plot()


# These are the first-and-second degree member contacts of committee 23.
# How many are there?
x %>% 
  with(neighborhood) %>%
  activate("nodes") %>%
  filter(type == FALSE & name != "committee_23") %>%
  as_tibble() %>%
  group_by(gender) %>%
  summarize(count = n())


remove(x)

## 2.4 to_shortest_path() #########################################

# Often, we want to know the shortest path between 2 nodes.
# In mobility networks, it can help with routing problems.
# In social networks, it can tell us which nodes are major bridging agents.

x = g %>%
  morph(
    to_shortest_path, 
    from = which( .N()$name == "committee_23" ), 
    to = which( .N()$name == "committee_37"), 
    mode = "out", # edges going outward from the first node 
    weights = .E()$weight
  ) 

class(x) # it's a list
names(x) # names
x$shortest_path # query it
x %>% with(shortest_path) # this works too

# View it!
plot(x$shortest_path)


# Quantities of Interest 

# How many steps does it take
# to get from node A to B?
x %>% 
  with(shortest_path) %>%
  activate("edges") %>%
  as_tibble() %>%
  summarize(count = n())


# How many commmittees are on-route from A to B?
x$shortest_path %>%
  activate("nodes") %>%
  as_tibble() %>%
  # Filter to committees
  filter(type == TRUE) %>%
  # Filter out start/end nodes
  filter(!name %in% c("committee_23", "committee_37")) %>%
  summarize(count = n())


# 2. Coaffiliation ###################################

## 2.1 Coaffilation Networks ###################

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

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)

# Let's quickly plot our coaffiliation network...
plot(gco)



## 2.2 Isolates ########################################

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




# 3. Quantities of Interest #######################################


# What are our main quantities of interest?

## 3.1 Distance To ###########################################

# How far apart are nodes from Node X?

# How far is each Committee from Committee 23?
gco %>% 
  mutate(steps = node_distance_to(nodes = which(.N()$name == "committee_23") )) %>%
  as_tibble()

# If weighted, use this one.
# gco %>% mutate(steps = node_distance_to(nodes = 1, weights = .E()$weight))


## 3.2 Centrality ###############################################

# How central is this node?
# How central is Committee 23?

### degree centrality - undirected #####################
# total edges going in or out of that node
gco %>%
  mutate(deg = centrality_degree(mode = "all"))

### weighted degree centrality - undirected #######################
# sum of edges (weights) going in or out of that node
gco %>%
  mutate(deg = centrality_degree(mode = "all", weights = .E()$weight ))

### betweenness centrality - undirected ##################
# number of shortest paths through graph that cross that node
# higher = more bridging capacity
# lower = lower bridging capacity
gco %>%
  mutate(betw = centrality_betweenness(directed = FALSE, weights = .E()$weight))

# Get harder to calcualte on really big graphs


## 3.3 Clustering ############################################

# Do our points cluster together naturally?

# Fast-greedy algorithm is one of the faster clustering strategies -
# allows you to specify the number of groups, within limits
gco %>%
  mutate(community = group_fast_greedy(weights = .E()$weight, n_groups = 8))

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




# 4. Iterating Tidygraphs ###############################

# Suppose I have too much data.
# Maybe it's not feasible to analyze the whole network at once.
# I could create a variable to split up my edges...
# Then split the graph up into several smaller graphs

g2 = g %>%
  activate("edges") %>%
  mutate(from_geo = .N()$geography[ .E()$from  ]    ) %>%
  # Split the graph into multiple graphs, using the from_geo() edges variable
  to_split(from_geo, split_by = "edges")

# Our graph has become a list of multiple graphs
g2 %>% names()
g2$`from_geo: iwate`

# To do iterative actions on non-data.frame data,
# We can use the purrr package's map() function.
# This says, hey, perform some function/process on each item of my list, 
# and return a list of outputs.


## 4.1 Iterative Random Sampling ###############################

# Let's split the graph into a list of graphs,
# then map a sampling function to each graph.

glist = g %>%
  activate("edges") %>%
  mutate(from_geo = .N()$geography[ .E()$from  ]    ) %>%
  # Split the graph into multiple graphs, using the from_geo() edges variable
  morph(to_split, from_geo, split_by = "edges") %>%
  # For each graph, sample 30 edges randomly!
  map(~.x %>% activate("edges") %>% sample_n(size = 30) )


# Now bundle them back together into one cohesive graph
# bind_graphs_list() will repeat the nodes,
# and provide back every node variable.
# Note: you will get duplicate nodes from this method.
glist %>%
  bind_graphs_list(.id = "group")

# graph_join_list() will join every graph item together,
# using as many variables as provided in `by`.
# Note: you will only receive back the node variables in `by`.
# Note: you will not get duplicate nodes from this method.
glist %>%
  graph_join_list(by = c("name"), .id = "group")


## 4.2 Iterative Coaffiliation ##############################


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





