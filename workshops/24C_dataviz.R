#' @name 24C_dataviz.R
#' @title Lesson: Data Visualization for Networks with ggplot2
#' @author Tim Fraser

# Following up on our work with the Disaster Recovery Committees dataset,
# Let's learn some ways to visualize our networks in ggplot.

# 0. Setup #########################################


## 0.1 Load Packages ##################################

# Let's load our packages 
library(dplyr) # data wrangling
library(readr) # reading data
library(ggplot2) # visualizing data
library(igraph) # working with graphs
library(tidygraph) # dplyr functions with graphs
library(ggraph) # for network layouts

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

g = read_rds("data/committees/graph_bipartite.rds")

g

# 1. Basic Visualization #######################################

# There are lots of ways to visualize networks in R,
# but I find that the ones that use core ggplot functions
# tend to perform best, and work most clearly.

# Let's load an add_layout() function
source("functions/add_layout.R")
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)

# Add a layout to your graph, using your graph's node id and a specific layout
gco = gco %>% add_layout(by = "name", layout = "fr")

# Extract nodes and edges for visualization
nodes = gco %>% activate("nodes") %>% filter(!node_is_isolated()) %>% as_tibble()
edges = gco %>% activate("edges") %>% as_tibble()


ggplot() +
  geom_segment(
    data = edges, 
    mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y)) +
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y)
  )


# 2. Color & Centrality ##################################

# Let's load an add_layout() function
source("functions/add_layout.R")

co = read_csv("data/committees/committees.csv") 
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)

# Add a layout to your graph, using your graph's node id and a specific layout
gco = gco %>% add_layout(by = "name", layout = "fr") 

# Join in extra traits
gco = gco %>% left_join(by = "name", y = co)

# Calculate centrality
gco = gco %>% mutate(deg = centrality_degree(mode = "all", weights = .E()$weight))

# Drop isolates
gco = gco %>% filter(!node_is_isolated())

# Extract nodes and edges for visualization
nodes = gco %>% activate("nodes") %>% as_tibble()
edges = gco %>% activate("edges") %>% as_tibble()



ggplot() +
  geom_segment(
    data = edges, 
    mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
    color = "lightgrey", linewidth = 0.25) +
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, fill = geography, size = deg),
    shape = 21, color = "white"
  ) +
  theme_void(base_size = 14)

# 3. Circles ######################################

# Let's load an add_layout() function
source("functions/add_layout.R")
source("functions/coaffiliate.R")

co = read_csv("data/committees/committees.csv") 
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)
remove(g)

# Join in extra traits
gco = gco %>% left_join(by = "name", y = co %>% select(name, geography))

# Calculate centrality
gco = gco %>% mutate(deg = centrality_degree(mode = "all", weights = .E()$weight))

# Drop isolates
gco = gco %>% filter(!node_is_isolated())

# Add a layout to your graph, using your graph's node id and a specific layout
gco = gco %>% 
  arrange(geography, desc(deg)) %>%
  add_layout(by = "name", layout = "circle") 

gco

# Extract nodes and edges for visualization
nodes = gco %>% activate("nodes") %>% as_tibble()
edges = gco %>% activate("edges") %>% as_tibble()

ggplot() +
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, 
                  color = geography, 
                  size = deg)) +
  geom_segment(
    data = edges,
    mapping = aes(x = from_x, y = from_y,
                  xend = to_x, yend = to_y,
                  linewidth = weight),
    color = "lightgrey") +
  scale_linewidth_continuous(range = c(0.01, 2)) +
  theme_void(base_size = 14)


# 4 Better Circles #########################################

# Let's repeat everything from before,
# but this time, let's shade each line based on the geography it stems from...

# Let's load an add_layout() function
source("functions/add_layout.R")
source("functions/coaffiliate.R")

co = read_csv("data/committees/committees.csv") 
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)
remove(g)
# Join in extra traits
gco = gco %>% left_join(by = "name", y = co %>% select(name, geography))
# Calculate centrality
gco = gco %>% mutate(deg = centrality_degree(mode = "all", weights = .E()$weight))
# Drop isolates
gco = gco %>% filter(!node_is_isolated())
# Add a layout to your graph, using your graph's node id and a specific layout
gco = gco %>% 
  arrange(geography, desc(deg)) %>%
  add_layout(by = "name", layout = "circle") 

# **EDIT**
# Add the type of edge...
gco = gco %>% activate("edges") %>%
  mutate(from_geo = .N()$geography[  .E()$from  ],
         to_geo = .N()$geography[ .E()$to  ])

# Extract nodes and edges for visualization
nodes = gco %>% activate("nodes") %>% as_tibble()
edges = gco %>% activate("edges") %>% as_tibble()


# Now let's plot it.
# we'll add.
# - scale_fill_viridis()
# - scale_color_viridis()
# - scale_size_continuous()



ggplot() +
  # Plot edges...
  geom_segment(
    data = edges,
    mapping = aes(x = from_x, y = from_y,
                  xend = to_x, yend = to_y,
                  linewidth = weight,
                  color = to_geo) ) +
  scale_linewidth_continuous(range = c(0.01, 2)) +
  scale_color_viridis(option = 'plasma', discrete = TRUE, end = 0.8) +
  # Plot points over top...
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg, color = geography)) +
  # Plot a nice white outline over top
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg),
    fill = NA, shape = 21, color = "white") +
  scale_size_continuous(range = c(1, 10), name = "Weighted\nDegree") +
  scale_fill_viridis(option = 'plasma', discrete = TRUE, end = 0.8) +
  theme_void(base_size = 14) +
  # Cut most of the legends
  guides(color = "none", fill = "none", linewidth = "none") +
  # Annotate
  geom_text(mapping = aes(x = 0.80, y = 0.80, 
                          label = "Iwate", color = "iwate"),
            size = 5) +
  geom_text(mapping = aes(x = -0.8, y = -0.8, 
                          label = "Miyagi", color = "miyagi"),
            size = 5) +
  geom_text(mapping = aes(x = 0.85, y = -0.85, 
                          label = "National", color = "national"),
            size = 5)


