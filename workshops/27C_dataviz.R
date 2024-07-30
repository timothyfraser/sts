#' @name 27C_dataviz.R
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
library(viridis) # for color palettes
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


# 1. Traditional Bipartite Network ####################################


## 1.1 Plain Plotting ###############################

# We could try a basic plot, like 
plot(g)

# Let's try this with a quick edit, to remove the labels and shrink nodes.
# That's a lot clearer.
plot(g, vertex.label=NA, vertex.size = 5)

# Still not great, but a little clearer.
# It would look terrible, but we could!





## 1.2 Using ggplot ################################

# There are lots of ways to visualize networks in R,
# but I find that the ones that use core ggplot functions
# tend to perform best, and work most clearly.

# How would we plot this better in ggplot?

# Load data and functions
source("functions/add_layout.R") # get add_layout()
g = read_rds("data/committees/graph_bipartite.rds")

# We can try a Fruchterman Reingold layout,
# which arranges it in a petri dish shape.
g = g %>% 
  add_layout(
    by = c("name", "type", "geography"), 
    layout = "fr")

g %>%
  activate("nodes") %>%
  select(name, x, y)



# Evaluate degree centrality. We'll use this to size our nodes
g = g %>%
  activate("nodes") %>%
  mutate(deg = centrality_degree(mode = "all"))

g %>%
  activate("nodes") %>%
  select(name, deg)


# Classify nodes into groups by type and geography.
# We'll use this to color our nodes.
g = g %>%
  activate("nodes") %>%
  mutate(group = case_when(
    geography == "iwate" ~ "Iwate Committees",
    geography == "miyagi" ~ "Miyagi Committees",
    geography == "national" ~ "National Committees",
    type == FALSE ~ "Members"
  ))

g %>%
  activate("nodes") %>%
  select(name, group)


# Classify each edge by the geography of its committee.
g = g %>%
  activate("edges") %>%
  mutate(from_geo = .N()$geography[ .E()$from ] )
  

# Extract nodes and edges as data.frames for visualization
nodes = g %>% activate("nodes") %>% as_tibble()
edges = g %>% activate("edges") %>% as_tibble()


# Plot edges with geom_segment() and nodes with geom_point()
gg = ggplot() +
  geom_segment(
    data = edges, 
    mapping = aes(
      x = from_x, y = from_y,
      xend = to_x, yend = to_y)
  ) +
  geom_point(
    data = nodes,
    mapping = aes(
      x = x, y = y, 
      fill = group, size = deg
    ),
    shape = 21, color = "white", stroke = 1
  )

gg # view



## 1.3 Revised ggplot #########################

# Can we beautify this plot and make it really useful?


# Plot edges with geom_segment() and nodes with geom_point()
gg = ggplot() +
  geom_segment(
    data = edges, 
    mapping = aes(
      x = from_x, y = from_y,
      xend = to_x, yend = to_y,
      # EDIT:
      # Let's color edges by geography
      color = from_geo),
    alpha = 0.75 # add some transparency
  ) +
  geom_point(
    data = nodes,
    mapping = aes(
      x = x, y = y, 
      fill = group, size = deg
    ),
    shape = 21, color = "white", stroke = 1
  )

gg

# Let's add scales
gg = gg +
  # Range the size of nodes visibly
  scale_size_continuous(range = c(1, 10)) +
  # Coordinate the node colors
  scale_fill_manual(
    breaks = c("Members", "Iwate Committees", 
               "Miyagi Committees", "National Committees"),
    values = c("#648FFF", "#FFB000", "#FE6100",  '#DC267F')
  ) +
  # Coordinate the edge colors too
  scale_color_manual(
    values = c("#FFB000", "#FE6100",  '#DC267F'),
    breaks = c("iwate", "miyagi", "national")
  )

gg # view it


# Finally, let's put some polish on it.
gg = gg + 
  theme_void(base_size = 14) +
  guides(color = "none") +
  labs(size = "# Memberships", fill = "Type",
       title = "Tohoku Disaster Recovery Committees Member Network")

gg # view it

# Not bad, right?

# Okay, let's clean up
rm(list = ls())




# 2. Traditional Coaffiliation Visual #######################################


## 2.2 Basic ggplot ##########################

# Alternatively, we might want to narrow into a coaffiliation network,
# so we have just 1 type of node to deal with.

# Let's load some functions
source("functions/add_layout.R") # get add_layout()
source("functions/coaffiliate.R") # get coaffiliate()

g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)

gco


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




## 2.2. Color & Centrality ##################################

# Let's load an add_layout() function
source("functions/add_layout.R")

co = read_csv("data/committees/committees.csv") 
co
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)

# Add a layout to your graph, using your graph's node id and a specific layout
gco = gco %>% add_layout(by = "name", layout = "fr") 

# Join in extra traits
gco = gco %>% left_join(by = "name", y = co)

gco

# Calculate centrality
gco = gco %>% mutate(deg = centrality_degree(mode = "all", weights = .E()$weight))

gco %>%
  activate("edges") %>%
  as_tibble() %>%
  with(weight) %>%
  hist()

# Drop isolates
gco = gco %>% filter(!node_is_isolated())


# Extract nodes and edges for visualization
nodes = gco %>% activate("nodes") %>% as_tibble()
edges = gco %>% activate("edges") %>% as_tibble()



# Visualize, with node color by geography
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









# 3. Two Sided Bipartite Visual ##############################

## 3.1 Two-Sided Layout ############################

# Alternatively, we could also try a two-sided bipartite visual. 
# This uses a "bipartite" layout, sometimes called a railway layout.

# Read in network and functions
source("functions/add_layout.R") # get add_layout()
g = read_rds("data/committees/graph_bipartite.rds")

# Let's calculate the degree
g = g %>%
  activate("nodes") %>%
  mutate(deg = centrality_degree(mode = "all"))

# Make a labeling variable, that incorporates geography
g = g %>%
  activate("nodes") %>%
  mutate(group = case_when(
    geography == "iwate" ~ "Iwate Committees",
    geography == "miyagi" ~ "Miyagi Committees",
    geography == "national" ~ "National Committees",
    is.na(geography) ~ "Members"
  ))


# Now add a layout
g = g %>%
  add_layout(by = "name", layout = "bipartite")

g

# Get nodes and edges with coordinates
nodes = g %>% activate("nodes") %>% as_tibble() %>% 
  select(name, type, deg, group, x, y)
edges = g %>% activate("edges") %>% as_tibble()

nodes

# Let's plot!
gg = ggplot() +
  # Plot edges...
  geom_segment(
    data = edges,
    mapping = aes(
      x = from_x, y = from_y,
      xend = to_x, yend = to_y),
    color = "lightgrey",
    alpha = 0.5) +
  # Plot points over top...
  geom_point(
    data = nodes,
    mapping = aes(
      x = x, y = y, 
      color = group, size = deg))

gg # view it


# Add scaling 
gg = gg +
  scale_color_viridis(
    option = "viridis", discrete = TRUE,
    breaks = c("Members", "Iwate Committees", 
               "Miyagi Committees", "National Committees")) +
  scale_size_continuous(range = c(1,10))

gg


## 3.2 Highlighting #################################

# Find the member with the most memberships

# find the member and committee with the most ties
highlight = nodes %>%
  # Add a unique id, which will help us filter edges later
  mutate(id = 1:n()) %>%
  # For each type of node
  group_by(type) %>%
  # Find the node with the max degree
  filter(deg == max(deg, na.rm = TRUE))  %>%
  select(id, name, type, deg, x, y) 

highlight

# Can you find the edges from just those highlighted nodes?
highlight_edges = edges %>%
  # Grab just edges running FROM that committee
  filter(from %in% highlight$id | 
           # or running TO that member
           to %in% highlight$id)


highlight_edges


# Make the blank plot
gg = ggplot()

gg

# Plot edges...
gg = gg + 
  geom_segment(
    data = edges,
    mapping = aes(
      x = from_x, y = from_y,
      xend = to_x, yend = to_y),
    color = "lightgrey",
    alpha = 0.5) 

gg

# Plot the highlighted edges
gg = gg + 
  geom_segment(
    data = highlight_edges,
    mapping = aes(x = from_x, y= from_y,
                  xend = to_x, yend = to_y),
               color = "black", alpha = 0.5)

gg

# Plot the points over top
gg = gg +
  geom_point(
    data = nodes,
    mapping = aes(
      x = x, y = y, 
      color = group, size = deg))

gg

# Highlight the nodes of interest
gg = gg +
  # Plot an outline around the member or committee of interest..
  geom_point(
    data = highlight,
    mapping = aes(x = x, y = y, size = deg),
    color = "black", shape = 21, fill = NA 
  )

gg # view it


highlight

# Add storytelling!
gg = gg + 
  geom_label(
    data = highlight %>% filter(type == FALSE),
    mapping = aes(x = x, y = y - 0.15, 
                  label = "1 member sat on \n12 different committees!"),
    alpha = 0.75,# Make it transparent, so we see the graph below    
    vjust = 1 # vertically justify, so we can send a line straight to that coordinate.
  ) +
  # Next, let's draw a line to our highlight point.
  geom_segment(
    data = highlight %>% filter(type == FALSE),
    mapping = aes(x = x, y = y,
                  xend = x, yend = y - 0.15)
    #color = "dodgerblue"
  )

gg # view it



# Add our second highlight text!
gg = gg +
  geom_label(
    data = highlight %>% filter(type == TRUE),
    mapping = aes(x = x, y = y + 0.15, 
                  label = "1 committee had\n55 different members!"),
    alpha = 0.75,# Make it transparent, so we see the graph below    
    vjust = 0 # vertically justify, so we can send a line straight to that coordinate.
  ) +
  # Next, let's draw a line to our highlight point.
  geom_segment(
    data = highlight %>% filter(type == TRUE),
    mapping = aes(x = x, y = y, xend = x, yend = y + 0.15)
  )

gg # view it


# Finally, let's add some labels and themes
gg = gg + 
  # simple theme
  theme_void(base_size = 14) +
  # scales
  scale_color_viridis(
    option = "plasma", discrete = TRUE,
    breaks = c("Members", "Iwate Committees", 
               "Miyagi Committees", "National Committees"),
    end = 0.8) +
  scale_size_continuous(range = c(2,8)) +
  labs(size = "# Memberships", color = "Type")  

gg # view it

# Great work! Let's clean up.
rm(list = ls())


# 4. Clustering Visuals ###########################


# Finally, we might want to explore larger network visuals.
# Large networks often need some devices for **showing** 
# structure to the reader,
# particularly structure that might not be immediately apparent.
# Clustering can do a good job of this.

# To make a pretty big network, let's grab
# the coaffiliation network where nodes are MEMBERS.

# Let's load some functions
source("functions/add_layout.R") # get add_layout()
source("functions/coaffiliate.R") # get coaffiliate()

g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of members
gmem = coaffiliate(graph = g, type = TRUE, names = TRUE, weight = "weight", diag = FALSE)

remove(g)

656^2
object.size(gmem)/1e6 # 0.3 MB

gmem

# Add a layout to your graph, using your graph's node id and a specific layout
gmem = gmem %>% add_layout(by = "name", layout = "fr")


gmem


# Quick plot...
plot(gmem, vertex.label=NA, vertex.size = 5)


# Looks like we might want to narrow into just the big subcomponent.

# I happen to know that node 1 is really well connected,
# so I'm going to narrow into the subcomponent that contains node 1.
# This will get rid of our isolates.
gmem = gmem %>%
  morph(to_subcomponent, node = 1) %>%
  with(subgraph)

gmem

# Try again... 
plot(gmem, vertex.label=NA, vertex.size = 5)
# yay! we see the main graph.

# Let's try to describe this network a little.

# Get degree for each node...
gmem = gmem %>%
  activate("nodes") %>%
  mutate(deg = centrality_degree(weights = .E()$weight, mode = "all"))



# Classify into groups
gmem = gmem %>%
  activate("nodes") %>%
  # Try a 3 group classification...
  mutate(group3 = group_fast_greedy(
    weights = .E()$weight, n_groups = 3) %>%
      factor()) 

gmem %>%
  activate("nodes") %>%
  as_tibble() %>%
  group_by(group3) %>%
  summarize(count = n())


# Extract nodes and edges for visualization
nodes = gmem %>% activate("nodes") %>% 
  filter(!node_is_isolated()) %>% as_tibble() %>%
  select(name, x,y,deg, group3)

edges = gmem %>% activate("edges") %>% as_tibble() %>%
  select(from, to, weight, from_x, to_x,from_y, to_y)


nodes
edges

# Options:
# Save it to file.
# ggsave()

# Threshold your network.
# Filter out edges with a weight less than X.

# Let's try a base plot!
gg = ggplot() +
  geom_segment(
    data = edges, 
    mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
    color = "lightgrey", alpha = 0.5) +
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg, fill = group3),
    shape = 21, color = "white"
  ) +
  theme_void(base_size = 14) +
  scale_size_continuous(range = c(1,5))



# With BIG visuals, often better to write it to file
ggsave(gg, filename = "workshops/27C_visual1.png", dpi = 300, height = 6, width = 6)
browseURL("workshops/27C_visual1.png")
gg # view it 


# Or, we could THRESHOLD to just edges with a weight greater than 1.

# Let's try a base plot!
gg = ggplot() +
  geom_segment(
    data = edges %>%
      filter(weight == 1), 
    mapping = aes(x = from_x, y = from_y, xend = to_x, yend = to_y),
    color = "lightgrey", alpha = 0.5) +
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg, fill = group3),
    shape = 21, color = "white"
  ) +
  theme_void(base_size = 14) +
  scale_size_continuous(range = c(1,5))

ggsave(gg, filename = "workshops/27C_visual2.png", dpi = 300, height = 6, width = 6)
browseURL("workshops/27C_visual2.png")
gg # view it 



# Voila!

rm(list = ls())





# 5. Circles ######################################

## 5.1 Using a Circle Layout ###############################

# Let's load an add_layout() function
source("functions/add_layout.R")
source("functions/coaffiliate.R")

co = read_csv("data/committees/committees.csv") 
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)
remove(g)

gco

# Join in extra traits
gco = gco %>% left_join(by = "name", y = co %>% select(name, geography))

gco

# Calculate centrality
gco = gco %>% mutate(deg = centrality_degree(mode = "all", weights = .E()$weight))


# Drop isolates
gco = gco %>% filter(!node_is_isolated())

gco

# Add a layout to your graph, using your graph's node id and a specific layout
gco = gco %>% 
  activate("nodes") %>%
  # arranging here is key - it will order the nodes by degree, in groups
  arrange(geography, desc(deg)) %>%
  add_layout(by = "name", layout = "circle") 

gco

# Extract nodes and edges for visualization
nodes = gco %>% activate("nodes") %>% as_tibble()
edges = gco %>% activate("edges") %>% as_tibble()



# Try chord diagram style, with a circle layout
ggplot() +
  # edges, with weight by lineweight
  geom_segment(
    data = edges,
    mapping = aes(x = from_x, y = from_y,
                  xend = to_x, yend = to_y,
                  linewidth = weight),
    color = "lightgrey") +
  # add a size range for linewidth
  scale_linewidth_continuous(range = c(0.3, 2)) +
  # and nodes, colored by geography
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, 
                  color = geography, 
                  size = deg)) +
  theme_void(base_size = 14)




## 5.2. Better Circles #########################################

# Let's repeat everything from before,
# but this time, let's shade each line based on the geography it stems from...

# Let's load an add_layout() function
source("functions/add_layout.R")
source("functions/coaffiliate.R")

co = read_csv("data/committees/committees.csv") 
g = read_rds("data/committees/graph_bipartite.rds")

# Let's get a coaffiliation graph of committees
gco = coaffiliate(graph = g, type = FALSE, names = TRUE, weight = "weight", diag = FALSE)

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



gg = ggplot() +
  # Plot edges...
  geom_segment(
    data = edges,
    mapping = aes(x = from_x, y = from_y,
                  xend = to_x, yend = to_y,
                  linewidth = weight,
                  color = to_geo) ) +
  # Plot points over top...
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg, color = geography)) +
  # Plot a nice white outline over top
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg),
    fill = NA, shape = 21, color = "white")

gg # view it



# Adjust scales
gg = gg + 
  scale_linewidth_continuous(range = c(0.1, 2)) +
  scale_color_viridis(option = 'plasma', discrete = TRUE, end = 0.8) +
  scale_size_continuous(range = c(1, 10), name = "Weighted\nDegree") +
  scale_fill_viridis(option = 'plasma', discrete = TRUE, end = 0.8)  +
  theme_void(base_size = 14) 

gg # View it

# Cleanup
gg = gg +  
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

gg # view it







## 5.3. Adding Plotly #################################

# Let's try taking our gg network visual from above, 
# and convert it into plotly now.

library(plotly)

# Okay, we got something...
ggplotly(gg)

# Okay first try. How can we make this a better interactive visual?

# Let's add hoverlabels.

# First, we'll make our hoverlabels
nodes = gco %>% activate("nodes") %>% as_tibble() %>%
  # Let's join in some key traits
  left_join(by = "name", 
            y = co %>% select(name, town, committee_type)) %>%
  # Let's revise the geography variable
  mutate(geography = case_when(
    geography == "iwate" ~ "Iwate Prefecture",
    geography == "miyagi" ~ "Miyagi Prefecture",
    geography == "national" ~ "National Level"
  )) %>%
  # And let's make a hoverlabel that incorporates all of it.
  mutate(hoverlabel = paste0(
    "<b>Committee: ", town, "</b>",
    "<br>",
    "<b>Region</b>: ", geography, 
    "<br>",
    "<b>Type</b>: ", committee_type,
    "<br>",
    "<b>Members Shared</b>: ",  deg
  ))



nodes



edges = gco %>% activate("edges") %>% as_tibble() %>%
  mutate(hoverlabel = paste0("Shared Members: ", weight))

# Then, we'll repeat our plot construction process, 
# but this time we will add in hoverlabels as a text aesthetic

gg = ggplot() +
  # Plot edges...
  geom_segment(
    data = edges,
    mapping = aes(
      x = from_x, y = from_y,
      xend = to_x, yend = to_y,
      linewidth = weight,
      color = to_geo,
      # Add a hoverlabel
      text = hoverlabel
    ) ) +
  # Plot points over top...
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg, color = geography)) +
  # Plot a nice white outline over top
  geom_point(
    data = nodes,
    mapping = aes(x = x, y = y, size = deg,
                  text = hoverlabel),
    fill = NA, shape = 21, color = "white")

gg # view it

# Adjust scales
gg = gg + 
  # Let's up the linewidth a tad here
  scale_linewidth_continuous(range = c(0.1, 5)) +
  scale_color_viridis(option = 'plasma', discrete = TRUE, end = 0.8) +
  scale_size_continuous(range = c(1, 10), name = "Weighted\nDegree") +
  scale_fill_viridis(option = 'plasma', discrete = TRUE, end = 0.8)  +
  theme_void(base_size = 14) 

gg # View it

# Cleanup
gg = gg +  
  # Cut most of the legends
  guides(color = "none", fill = "none", linewidth = "none") +
  # Annotate
  geom_text(mapping = aes(x = 0.80, y = 0.80, label = "Iwate", color = "iwate"), size = 5) +
  geom_text(mapping = aes(x = -0.8, y = -0.8, label = "Miyagi", color = "miyagi"), size = 5) +
  geom_text(mapping = aes(x = 0.85, y = -0.85, label = "National", color = "national"), size = 5)

gg # view it

# Convert into plotly, keeping 'text' as the tooltips
pp = ggplotly(gg, tooltip = "text")
pp



# 6. Conclusion ##################################

# These are just some of the many excellent ways to visualize networks.
# Lots of helper packages exists, with various tradeoffs.
# But, with your ggplot toolkit, you now know how to 
# make any most kinds of network visual from scratch.
# Great work!


