# 


# How close is your nearest polling place? 
# Questions like these are vital to the democratic process,
# and enabling voter turnout,
# but are perniciously difficult to answer! 
# This tutorial uses the case of polling places 
# in Boston precincts to demonstrate how to 
# identify your distance from your nearest polling place, 
# focusing on the sf and dplyr packages in R.




# 0. Import Data ############################

# In this workshop, we’ll use a few files. This includes:
  
# - precincts.rds: a dataset of 255 precinct polygons.
#
# - polling_places.rds: a dataset of 256 polling place locations.
#
# - train_stops_boston.rds: a dataset of train stops 
#                          on the Boston T (subway/metro system).
#
# - train_lines_boston.rds: a dataset of train lines 
#                           on the Boston T (subway/metro system).
#

# Please click through the buttons below 
# and follow the instructions to make each file!

# First, let’s load our four main packages, 
# and then we’ll load the projections
# we’ll use for our maps 
# - aea = Albers Equal Area Conic in meters, 
# - aed = Lambert Equidistant Conic in meters,
#      and 
# - wgs = World Global System in degrees


# Data wrangling packages 
library(dplyr) # for tidy data wrangling
library(readr) # read rds data
library(ggplot2) # for visualization
library(viridis) # for color palettes
# Mapping packages
library(sf) # for spatial data.frames
# library(rgdal) # for background geospatial operations - might not be necessary anymore.


# Next, let's also save several mapping projections as objects:

# Equal Area projection
aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Equal Distance projection
aed <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Look up more projections at spatialreference.org - proj4
#https://spatialreference.org/ref/epsg/wgs-84/


# Load precinct polygons
precincts <- read_rds("data/boston_voting/precincts.rds")

# Load polling place points
polls <- read_rds("data/boston_voting/polling_places.rds")
# Load t-lines
tlines <- read_rds("data/boston_voting/train_lines_boston.rds")
# Load t-stops
tstops <- read_rds("data/boston_voting/train_stops_boston.rds")

# All of these already have the Albert Equal Area Conic projection



## View Data ########################

# View first 3 rows of dataset
precincts %>% head(3)


# View first 3 rows of dataset
polls %>% head(3)


# View first 3 rows of dataset
tlines %>% head(3)


# View first 3 rows of dataset
tstops %>% head(3)


## Codebook ##########################

# In these datasets, our variables mean:
  
# -  ward_precinct: the 4-digit code of each voting precinct
#                   (and the city ward it is located within). 
#                   Each precinct has one polling place.
#
# - id: the unique ID number for each polling place.
#                  (Appears in polls)
#
# - line: the name of the Boston T (metro/subway) line.
#                  (Appears in tlines and tstops)
#
# - stop: the name of the Boston T (metro/subway) stop.
#                  (Appears in tstops)
# 
# - geometry: the row containing the points, polygons, or lines
#             in our spatial data.frames. Appears in all objects.


# Task 1: Visualize ################################

# First, we should always visualize our data to 
# understand what’s going on there. 
# We can visualize sf objects using the geom_sf() command 
# in ggplot syntax, from the tidyverse ecosystem of data packages.


# Make and save the visual as 'g1'
g1 <- ggplot() +
  # Visualize a nice background outline
  geom_sf(data = precincts, color = "darkgrey", size = 5) +
  # Visualize tan precinct polygons
  geom_sf(data = precincts, color = "white", fill = "tan") +
  # Visualize polling places as squares (shape = 22)
  geom_sf(data = polls, fill = "black", size = 1, shape = 22) +
  # visualize T-lines
  geom_sf(data = tlines, mapping = aes(color = line), size = 0.5) +
  # Vsiaulize t-stops
  geom_sf(data = tstops, mapping = aes(color = line), size = 1.15) +
  # Assign a color scheme to the t-lines and stops
  scale_color_manual(
    # Order the levels as desired
    breaks = c("GREEN", "ORANGE", "RED", "BLUE", "SILVER", "MULTIPLE"),
    # Now add the labels you want
    labels = c("Green", "Orange", "Red", "Blue", "Silver", "Multiple"),
    # And give each a color
    values = c("seagreen", "orangered", "firebrick", 
               "dodgerblue", "grey","purple")) +
  # Add a clear, simple theme
  theme_void(base_size = 30) +
  # And a basic legend
  labs(color = "Public\nTransit\nLine",
       subtitle = "Polling Places vs. Public Transit\nin Boston Precincts")

# Display the visual
g1


# Task 2: Measure ##############################

# Next, let’s figure out how to calculate the distance
# between each polling place and the nearest t-stop!
  
polls <- read_rds("data/boston_voting/polling_places.rds") %>%
  # Transform to Equal Distance Projection
  st_transform(crs = aed) 


tstops <- read_rds("data/boston_voting/train_stops_boston.rds") %>%
  # Transform to Equal Distance Projection
  st_transform(crs = aed) %>%
  # Save a copy of these train stop's geometry
  # in a new column, called geometry_stop
  mutate(geometry_stop = geometry)

# Identify which stops are closest to each polling places
nearest <- polls %>%
  # Join together the stops nearest
  st_join(tstops, join = st_nearest_feature) %>%
  # Extract the coordinates of our polls and our new t-stops'
  # st_coordinates returns a two-column matrix (x and y),
  # so we grab the first column for the x-coordiante,
  # and the second column for the y-coordinate
  mutate(x_poll = st_coordinates(geometry)[,1],
         y_poll = st_coordinates(geometry)[,2],
         x_stop = st_coordinates(geometry_stop)[,1],
         y_stop = st_coordinates(geometry_stop)[,2]) %>%
  # Convert to data.frame
  as.data.frame() %>%
  # Get rid of our now unnecessary geometry fields
  select(-geometry, -geometry_stop)

# Let’s take a peek at our first few rows.

# Next, let’s turn these into a line-string object.

mydist <- nearest %>%
  # Zoom into just precincts where their coordinates
  # and their nearest t-stop coordinates could be obtained
  filter(!is.na(x_poll), !is.na(y_poll),
         !is.na(x_stop), !is.na(y_stop)) %>%
  # For each polling station and its precinct,
  group_by(id, ward_precinct) %>%
  # Build a new geometry column, with a 4-cell matrix
  mutate(geometry = matrix(
    # listing our polling place x and our t-stop x coordinate
    c(x_poll,  x_stop, 
      # and listing our polling place y and our t-stop y coordinate
      y_poll,  y_stop),
    # Make the matrix have two-rows and two-columns
    ncol = 2, nrow = 2) %>%
      # convert it to a line-string geometry
      st_linestring(dim = "XY") %>%
      # And bind it as a spatial data.frame,
      # with an Equal Distance Conic projection
      st_sfc(crs = aed)) %>%
  # Turn the whole data.frame into a spatial data.frame
  # with an Equal Distance Conic projection
  st_as_sf(crs = aed) %>%
  # Finally, calculate the distance of our linestrings!
  # It will calculate in the original units of our projections,
  # so meters, in this case.
  mutate(distance = st_length(geometry) %>% as.numeric())


# Let’s take a peek at our new distance column, 
# which measures the distance between each polling place 
# and their nearest t-stop in meters!
  
  

# Task 3: Analyze #########################################

# Great! We can visualize these lines too, like this!

g1 +
  # Let's layer on top our new lines!
  geom_sf(data = mydist %>%
            # Remember to transform it to equal area conic
            st_transform(crs = aea), color = "black")
  
# Or we could even analyze them, like this!
  
mydist %>%
  as.data.frame() %>%
  ggplot(mapping = aes(x = distance)) +
  geom_histogram(mapping = aes(y = ..density..), 
                 fill = "dodgerblue", color = "white") +
  geom_density(fill = "dodgerblue", alpha = 0.5) +
  labs(x = "Distance between Polling Place\nand Nearest T-Stop (meters)",
       y = "(%) Frequency\n(n = 256 polling places)") +
  theme_bw(base_size = 24)
  

# And that’s a wrap!
  
# Task 4: Export ###############################


# Finally, we need to export our file! We can use write_sf() for this.
  
mydist %>% write_sf("data/boston_voting/mydistances.geojson")



# EXTRA #######################################

# How did we get this geospatial data? See below.

## Precincts ###########################

# Next, let’s gather our precinct polygon data, 
# straight from source on Boston’s open data platform.

# We will save most files in this tutorial as .rds files,
# because it’s a convenient way to compress and work 
# with geospatial data in R, without having to reproject
# each time we load in the data.
# However, you could just as easily save them as .kml files, 
# which we will do at the end of this tutorial, 
# using the sf package’s write_sf() function.

# Load in a kml file straight from source.
# You could replace this with your kml file name
# data = read_sf("https://bostonopendata-boston.opendata.arcgis.com/datasets/2bc185ec9b0b478d8c2f7e720d116863_0.kml?outSR=%7B%22latestWkid%22%3A2249%2C%22wkid%22%3A102686%7D") %>%
#   # grab (and rename) just relevant files
#   select(ward_precinct = WARD_PRECINCT, geometry) %>%
#   # transform to an Equal Area projection
#   st_transform(crs = aea) %>%
#   # save as a .rds file for easy access
#   saveRDS("data/boston_voting/precincts.rds")

## Polling Places ########################


# Download Polling Places in Boston as Points
# read_sf("https://bostonopendata-boston.opendata.arcgis.com/datasets/f7c6dc9eb6b14463a3dd87451beba13f_5.kml?outSR=%7B%22latestWkid%22%3A2249%2C%22wkid%22%3A102686%7D") %>%
#   # Convert to Equal Area Projection
#   st_transform(crs = aea) %>%
#   # Turn our numeric Ward and Precinct codes into 2-digit codes
#   mutate(Ward = str_pad(Ward, width = 2, side = "left", pad = "0"),
#          Precinct = str_pad(Precinct, width = 2, side = "left", pad = "0"),
#          # And then bind them together into a ward-precinct identier
#          ward_precinct = paste(Ward, Precinct, sep = "")) %>%
#   # keep (and rename) just columns we need and get rid of extraneous data
#   select(id = OBJECTID, ward_precinct, geometry) %>%
#   # Save to file
#   saveRDS("data/boston_voting/polling_places.rds")


# Train Lines ##########################

# Finally, I downloaded and converted the shape files 
# for the Boston T (metro/subway system here). 
# You can download them yourself from MassGIS, 
# or import my processed version, 
# saved as train_lines_boston.rds 
# and train_stops_boston.rds.


