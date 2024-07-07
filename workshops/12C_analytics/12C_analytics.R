# Workshop: Using GIS to find the Nearest Polling Place in Boston
# Tim Fraser

# Load Packages

# Data wrangling packages 
library(dplyr) # for tidy data wrangling
library(readr) # for reading in data
library(viridis) # for visualization
# Mapping packages
library(sf) # for spatial data.frames
library(rgdal) # for background geospatial operations


# Next, let's also save several mapping projections as objects:

# Equal Area projection
aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Equal Distance projection
aed <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Look up more projections at spatialreference.org - proj4
#https://spatialreference.org/ref/epsg/wgs-84/


# In this lesson, you're going to learn several types of spatial analytics,
# specifically geared towards big databases.
# 
# While there are hundreds of excellent geospatial analytical techniques,
# some are better suited to big data than others.
# Specifically, with a big geospatial database,
# you're probably not going to be able to analyze the whole thing at once.
# So you need smaller scale tools you can repeat iteratively.

# Let's learn a few:

# - Distance
# - Adjacency
# - Service Areas

# Distance ##########################################

# You have been commissioned by the Secretary of State in Boston
# to maximize turnout in local elections.
# You want to know, how far are polls from local metro stops?

library(dplyr)
library(readr)
library(sf)
library(stringr)

# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Let's read in the precincts data we have
polls = read_sf("data/boston_voting/polling_places.geojson")
# Read in T Metro Stops
tstops = read_sf("data/boston_voting/train_stops_boston.geojson")


# Which metro stops are closest to each poll?
closest = polls %>%
  st_join(
    # Join in t-stop station name
    y = tstops %>% select(station, geometry), 
    # Joining by nearest feature
    join = st_nearest_feature) %>%
  # Let's turn this into a tibble
  as_tibble() %>%
  select(id, station)

# I'd love to get a distance measure for these. How could we do this?

# First, we need to create lines between each poll and each tstop
# but not ALL of them - that's a lot of lines!
# Not big data friendly.
# Just the closest poll-and-tstop pairs
# Let's say each poll-and-tstop pair are in a group, 
# with a specific group ID - the poll's ID

# Let's bundle these points 
points = bind_rows(
  # Getting all the polls, identified by poll and closest station
  polls %>%
    inner_join(by = c("id"), y = closest) %>%
    select(id, station, geometry),
  # And all the t-stops, identified by station and closest poll
  tstops %>%
    inner_join(by = "station", y = closest, multiple = "all") %>%
    select(id, station, geometry)
) 



# Iteratively, get linestrings
lines = points %>%
  # For each closest-station-poll pair, 
  group_by(id, station) %>%
  # Bundle points into a multiple, then draw a line through the points
  summarize(geometry = geometry %>% st_union() %>% st_cast(to = "LINESTRING")) %>%
  # Measure distance in km
  mutate(km = geometry %>% st_length() %>% as.numeric() %>% {./ 1000})


# How far is the nearest polling place from each T stop, on average?
# Eg. At Which t-stops should we consider adding a shuttle to the polls?
lines %>%
  as_tibble() %>%
  group_by(station) %>%
  summarize(mean_dist = mean(km))


# Which precincts are FURTHEST from their nearest t-stop?
lines %>%
  as_tibble() %>%
  select(id, km) %>%
  # Get top tenth furthest 
  arrange(desc(km)) %>%
  slice(1:10)



rm(list = ls())



# Spatial Aggregation #####################################


# You run logistics engineering for a nationwide logistics and deliveries firm,
# responsible for delivering/collecting ballots to polling places across the country.
# You have just downloaded a subset of data from your firm database.

# You have a spatial dataset of Boston voting precincts. 
# There are 5~10 precincts in each ward.
# You want to calculate the total size of each ward that your company administers.

# Your company makes deliveries for wards 01, 04, and 08.

library(dplyr)
library(readr)
library(sf)
library(stringr)

# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Equal Area projection
aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Let's read in the precincts data we have
data = read_sf("data/boston_voting/precincts.geojson")


# We want to be as efficient as possible for our query.
# So, let's narrowing into just the precincts in the wards we care about.


# The `ward_precinct` column shows the ward id, eg. `01`, 
# followed by the precinct id eg. `13`

# Let's use string substitution to extract each ward.
# Let's get the ward group for each - first 2 characters
precincts = data %>%
  mutate(ward = str_sub(ward_precinct, 1,2)) 

# Let's filter to just the wards your company administers
precincts = precincts %>% 
  filter(ward %in% c("01", "04", "08"))



# Now let's spatially aggregate for each ward,
# combining the precinct polygons for every ward.
wards = precincts %>%
  group_by(ward) %>%
  summarize(geometry = st_union(geometry))

# Finally, let's measure the area in each polygon
wards = wards %>%
  # First, transform to an equal area projection...
  st_transform(crs = aea) %>%
  # Calculate area, make numeric, and convert from m^2 to km^2
  mutate(area = st_area(geometry) %>% as.numeric() %>% {. / 1000^2} ) %>%
  # Go back to wgs projection
  st_transform(crs = wgs)

# Looks like ward 1 has by far the largest area of coverage
# (a lot of it is water.)
wards



# Let's plot them!
ggplot() +
  geom_sf(data = data, fill = "grey", color = "white") +
  geom_sf(data = precincts, mapping = aes(fill = ward), alpha = 0.5, color = "white") +
  geom_sf(data = wards, fill = NA, color = "#373737", linewidth = 1.5)  +
  theme_void()

rm(list = ls())



# Spatial Joins #######################################

# You run logistics engineering for a nationwide logistics and deliveries firm,
# responsible for delivering/collecting ballots to polling places across the country.
# You have just downloaded a subset of data from your firm database.

# You have a spatial dataset of Boston voting precincts. 
# There are 5~10 precincts in each ward.
# You want to know how many polling places are in each of the wards that your company administers.
# Your company administers wards 01, 04, and 08.

# Suppose we only know the id and geometry of each polling place
polls <- read_sf("data/boston_voting/polling_places.geojson") %>%
  select(id, geometry)

# We can use a spatial join as a kind of spatial filter,
# keeping only the points that are within these wards.

# Give me precincts
zone = read_sf("data/boston_voting/precincts.geojson") %>%
  # filter to just precincts whose ward_precinct id starts with 01, 04, or 08
  # String detect uses regex, a number sorting method.
  # We're saying here, say TRUE if it contains 01 OR 04 OR 08 AND THEN 2 digits from 0 to 9.
  filter(str_detect(ward_precinct, pattern = "(01|04|08)[0-9]{2}")) %>%
  # Create a spatial filter
  summarize(geometry = st_union(geometry))

# Give me just the polls that are within the zone.
zonepolls = polls %>%
  # Join in the zone, but keep only the rows with a valid join (left = FALSE)
  st_join(zone, left = FALSE)

# How many are there?
zonepolls %>%
  as_tibble() %>%
  summarize(count = n())

# Plot them
ggplot() +
  geom_sf(data = zone, fill = "dodgerblue", alpha = 0.5) +
  geom_sf(data = polls, size = 4, color = "grey") +
  geom_sf(data = zonepolls, size = 3, color = "black") +
  theme_void()


rm(list = ls())


# Service Areas #####################################

# You run logistics engineering for a nationwide logistics and deliveries firm,
# responsible for delivering/collecting ballots to polling places across the country.
# You have just downloaded a subset of data from your firm database.

# You have to decide which polling places will get visited by which trucks.
# This means grouping polling places together by proximity into **service areas.**

library(dplyr)
library(readr)
library(tidyr)
library(broom)
library(sf)
library(stringr)

wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Background for plotting
precincts = read_sf("data/boston_voting/precincts.geojson")

# Load polling place points
polls <- read_sf("data/boston_voting/polling_places.geojson") %>%
  # Let's get the ward group for each - first 2 characters
  mutate(ward = str_sub(ward_precinct, 1,2)) %>%
  # Let's split up the wards into inner and outer Boston
  mutate(group = case_when(
    ward %in% c("01", "02", "03", "05", "06", "04", "07", "08", "09") ~ "inner", 
    TRUE ~ "outer"))

# View the CRS (coordinate reference system) = should be WGS 84
st_crs(polls)

## Cluster Points 1 Time #####################################

# First, we need to step out of spatial data into a simple tibble,
# with an id for joining, and an x and y column

# Get point data
points = polls %>%
  # Let's make it into a tibble
  as_tibble() %>%
  # We can use st_coordinates() to get a matrix of X and Y pairs
  # Then we need to turn those back into a data.frame
  # since mutate only takes data.frames
  # and we can name the first column x and the second column y
  mutate(geometry %>% st_coordinates() %>% as_tibble() %>% select(x = 1, y = 2)) %>%
  # Grab just the point identifier, x, and y.
  select(id, x, y)

# We can use k-means clustering to find the clusters that minimize the sum of squared residuals
# Suppose we have 3 trucks, so we want 3 service areas.
# Cluster into 3 areas
m = points %>%
  kmeans(x = ., centers = 3)

# View cluster details
broom::tidy(m)
broom::glance(m)

# Add the cluster ID in
polls = polls %>% 
  mutate(cluster = m$cluster)


# Finally, we could create polygons for these clusters using some sf magick too.
cluster_polygons = polls %>%
  # For each cluster...
  group_by(cluster) %>%
  # union points into multipoints, then find the boundaries of the multipoints
  summarize(geometry = geometry %>% st_union() %>% st_convex_hull())

# Visualize it!
ggplot() +
  # Plot a simple background of precincts
  geom_sf(data = precincts, fill = "black", color = "#373737") +
  # Plot polls over top
  geom_sf(data = polls, 
          mapping = aes(fill = factor(cluster) ),
          size = 3, shape = 21,  color = "white") + 
  # Visualize the clusters 
  geom_sf(data = cluster_polygons,
          mapping = aes(fill = factor(cluster) ),
          alpha = 0.5, color = "white") +
  labs(fill = "Service Area") +
  theme_void()


## Cluster Points Iteratively  #####################################

# But what if we have MANY areas we want to establish clusters for?
# We can use our group_by() / reframe() routine with a grouping variable, like 'group',
# which describes inner vs. outer areas of Boston relative to the city downtown.


# Let's grab the coordinates, in GROUPS for inner and outer boston
points = polls %>%
  as_tibble() %>%
  mutate(geometry %>% st_coordinates() %>% as_tibble() %>% select(x = 1, y = 2)) %>%
  # Grab just the point identifier, x, and y.
  select(id, group, x, y) 


points2 = points %>%
  group_by(group) %>%
  reframe(
    # Keep the ID
    id = id,
    # Run kmeans with a data.frame of numeric inputs, and return the cluster id
    cluster2 = kmeans(tibble(x, y), centers = 3)$cluster
  ) %>%
  # Make a cluster id respective of the group
  mutate(clusterid = paste0(group, "-", cluster2))

# Let's join this cluster ID back into polls
polls = polls %>%
  left_join(by = c("id", "group"), y = points2)



# How many polling places were allocated to each group and cluster?
points2 %>%
  group_by(clusterid, group, cluster2) %>%
  summarize(count = n())

# Make new cluster polygons for these clusters-by-group
cluster_polygons2 = polls %>%
  # For each cluster...
  group_by(clusterid, group, cluster2) %>%
  # union points into multipoints, then find the boundaries of the multipoints
  summarize(geometry = geometry %>% st_union() %>% st_convex_hull())

# Let's visualize them.
ggplot() +
  # Plot a simple background of precincts
  geom_sf(data = precincts, fill = "black", color = "#373737") +
  # Plot polls over top
  geom_sf(data = polls, 
          mapping = aes(fill = factor(clusterid) ),
          size = 3, shape = 21,  color = "white") + 
  # Visualize the clusters 
  geom_sf(data = cluster_polygons2,
          mapping = aes(fill = factor(clusterid) ),
          alpha = 0.5, color = "white") +
  labs(fill = "Service Area") +
  theme_void()

# Clean up
rm(list = ls())


# Adjacency #########################################

## Degrees of Separation ############################

# Get precinct polygons
precincts = read_sf("data/boston_voting/precincts.geojson") %>%
  # Fix invalid polygons
  mutate(geometry = st_make_valid(geometry))

# How many precincts can one T stop serve?
poi = read_sf("data/boston_voting/train_stops_boston.geojson") %>%
  # Let's investigate Mass Ave T Stop
  filter(station == "Massachusetts Ave", line == "ORANGE")

# Get the polygons that are 0 degrees of separation away (contains the poi)
p0 = precincts %>%
  st_join(y = poi, left = FALSE) %>%
  select(ward_precinct, geometry) %>%
  mutate(degree = 0)

# Get the polygons that are 1 degree of separation away
p1 = precincts %>%
  # By inner joining into the 0 degree of separation polygons
  st_join(y = p0 %>% select(geometry), join = st_touches, left = FALSE) %>%
  distinct() %>%
  # Filter out those already within
  filter(!ward_precinct %in% c(p0$ward_precinct) ) %>%
  mutate(degree = 1)

# Get the polygons that are 2 degrees of separation away
p2 = precincts %>%
  # By inner joining into the 1 degree of separation polygons
  st_join(y = p1 %>% select(geometry), join = st_touches, left = FALSE) %>%
  distinct() %>%
  # Filter out those already within
  filter(!ward_precinct %in% c( p1$ward_precinct, p0$ward_precinct) ) %>%
  mutate(degree = 2)

# Get the polygons that are 3 degrees of separation away
p3 = precincts %>%
  # By inner joining into the 0 degree of separation polygons
  st_join(y = p2 %>% select(geometry), join = st_touches, left= FALSE) %>%
  distinct() %>%
  # Filter out those already within
  filter(!ward_precinct %in% c(p2$ward_precinct, p1$ward_precinct, p0$ward_precinct) ) %>%
  mutate(degree = 3)


# Bundle them together!
neighbors = bind_rows( p0,p1,p2,p3 )


# Visualize it!
ggplot() +
  geom_sf(data = neighbors, mapping = aes(fill = factor(degree) ), color = "white", alpha = 0.75) +
  geom_sf(data = poi) 

## Iterative Adjacency ############################

# Get precinct polygons
precincts = read_sf("data/boston_voting/precincts.geojson") %>%
  # Fix invalid polygons
  mutate(geometry = st_make_valid(geometry))

# How many precincts can a T stop serve?
poi = read_sf("data/boston_voting/train_stops_boston.geojson") %>%
  # Let's investigate Mass Ave T Stop AND Cambridge T Stop
  filter( (station == "Massachusetts Ave" & line == "ORANGE") | (station == "Haymarket" ) )

# For each station, find the precincts for that station
p0 = precincts %>%
  st_join(y = poi, left = FALSE) %>%
  mutate(degree = 0)

# For each station's precinct, find the precincts adjoining that precinct
p1 = precincts %>% 
  select(geometry) %>%
  st_join(y = p0, join = st_touches, left = FALSE) %>%
  mutate(degree = 1)

rm(list = ls())


