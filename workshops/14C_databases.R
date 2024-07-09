# 14C_analytics.R
# Databases V: Gridding and Heatmaps
# Topic: Using GIS to Map Voter Turnout for every City Block in Boston
# Tim Fraser

# Setup ############################################

# Load Packages
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(viridis)
library(gstat) # new package for spatial smoothing
library(ggpubr) # new package for bundling charts

# Equal Area projection
aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Equal Distance projection
aed <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Look up more projections at spatialreference.org - proj4
#https://spatialreference.org/ref/epsg/wgs-84/


# Heatmaps #########################################################




# Let's filter to the percentage of all possible ballots that were cast -
# commonly known as 'voter turnout'.
data = read_csv("data/boston_voting/boston_votes.csv") %>%
  # We have voting data for MOST precincts - lets narrow into a cohesive chunk
  filter(!ward %in% c("22", "21", "18", "20"))

# Let's get precinct polygons
precincts = read_sf("data/boston_voting/precincts.geojson") %>%
  # Make the geometries valid, if they are not
  st_make_valid() %>%
  # Make a joined spatial dataset of variables 'precincts'
  inner_join(by = "ward_precinct", y = data)

# Let's plot it.
ggplot() +
  geom_sf(data = precincts, mapping = aes(fill = voter_turnout), color = "white") +
  scale_fill_viridis(option = "plasma") +
  theme_void()

# Some of these precincts are larger than others.
# If we mapped boston in equal sized grid cells,
# we might be able to smooth these values out.

# Here are two methods:
# - spatial averaging
# - spatial smoothing (interpolation) using a distance function

# Both require we make a grid.

## Gridding #############################################

# To spatially average, we need to make a grid!
# To approximate a city block, let's make a 1 square kilometer grid.

# Make boundaries for boston
boston = precincts %>%
  summarize(geometry = st_union(geometry))


# Let's make a grid
grid = boston %>% 
  # Transform to equal area conic projection for area calculations
  st_transform(crs = aea) %>%
  # Make a grid of exactly 1000 x 1000 meters
  st_make_grid(cellsize = c(1000,1000), square = TRUE, crs = aea) %>%
  # Transform it back to wgs projection
  st_transform(crs = wgs) %>%
  # Make it into an spatial data.frame
  st_as_sf() %>% rename(geometry = x) %>% 
  # Give each cell a unique ID
  mutate(cell = 1:n())

# View it!
ggplot() +
  geom_sf(data = boston, fill = "grey") +
  geom_sf(data = grid, fill = NA)


## Spatial Averaging ###################################

# Next, we could spatially average our data onto our grid
# using an st_join() plus our group_by() / summarize() magick.

# To start, we have 238 grid cells.
nrow(grid)

# Join in voter turnout to each cell, using the geometry of precincts
grid %>%
  st_join(y = precincts %>% select(geometry, voter_turnout),
          # Join by which geometries overlap
          join = st_overlaps)

# After joining, we have 665 observations
# many cells have multiple precincts overlapping them

# Let's add another step, aggregating
averages = grid %>%
  st_join(precincts %>% select(geometry, voter_turnout)) %>%
  # Turn it into a tibble, so we can data.wrangle quickly
  as_tibble() %>%
  # For each cell, give me the mean voter turnout
  group_by(cell) %>%
  summarize(voter_turnout = mean(voter_turnout, na.rm = TRUE))

# Back to 238 grid cells
averages

# But we need the geometry back in for mapping, right?

# So let's take our spatial data.frame and join our estimates into it,
# using the shared grid 'cell' id
gridded_averages = grid %>%
  left_join(by = "cell", y = averages) %>%
  # And let's just drop the NAs
  filter(voter_turnout != "NaN")

# view it
gridded_averages

# Plot it!
ggplot() +
  # Map the gridded averages!
  geom_sf(data = gridded_averages, mapping = aes(fill = voter_turnout), color = "white") +
  # Overlay boston overtop, with a blank fill
  geom_sf(data = boston, fill = NA, color = "black", linewidth = 1.5) +
  scale_fill_viridis(option = "plasma") +
  theme_void()


## Spatial Smoothing ##########################

# Alternatively, a different way to approximate change in values over space
# is to use a model.
# We could use a spatial interpolation model 
# to spatially smooth our estimates,
# using a distance function.

# gstat allows us to do lots of spatial statistics.

# Inverse Distance Weighting is particularly helpful for big datasets,
# because it's quite simple and quick.
# It says, I assume that values closer together should have similar values.
# If I have Inverse Distance, I assume values differ as distance increases linearly.
# If I have Inverse Distance Squared Weighting, I assume values differ via a squared function as distance increases.

# We CAN supply polgyons, but it'll take more time.

# Spatial smoothing actually occurs using points,
# so it's always fastest to provide exact grid cell centroid points for predictions,
# and then join the predictions back into the grid polygons.


### using polygons ##########################

# check how long it takes 
system.time({
  m = gstat::gstat(formula = voter_turnout ~ 1, locations = precincts, nmax = 10, set = list(idp = 2))
  predict(m, newdata = grid)
})
# About 6 seconds for me with polygons
# OOPH! That's bad.

### using points ############################

# Let's get centroid points per grid cell
gridded_points = grid %>% mutate(geometry = geometry %>% st_centroid()  )
# Let's get centroid points per precinct
precinct_points = precincts %>% mutate(geometry = geometry %>% st_centroid())
# Check how long it takes
system.time({
  # Make the model object
  m = gstat::gstat(
    # What's the formula?
    formula = voter_turnout ~ 1, 
    # What's the raw data we're building the model off of?
    locations = precinct_points, 
    # What's the max number of nearby points to draw an estimate from?
    nmax = 10, 
    # What's the level of inverse distance weighting? 1? 2 (squared)? 3 (cubed)?
    set = list(idp = 2))
  predict(m, newdata = gridded_points)
})
# Takes about 0.03 seconds for me with points.
# Wow! That's fast.


### weighting ############################################

# Let's compare several different types of inverse distance weighting.

# Let's get centroid points per grid cell
gridded_points = grid %>% mutate(geometry = geometry %>% st_centroid()  )
# Let's get centroid points per precinct
precinct_points = precincts %>% mutate(geometry = geometry %>% st_centroid())
# Make a model with IDW of 1 (linear)
m1 = gstat::gstat(formula = voter_turnout ~ 1, locations = precinct_points, 
                  nmax = 10, set = list(idp = 1))
# Make a model with IDW of 2 (squared)
m2 = gstat::gstat(formula = voter_turnout ~ 1, locations = precinct_points, 
                  nmax = 10, set = list(idp = 2))
# Make a model with IDW of 3 (cubed)
m3 = gstat::gstat(formula = voter_turnout ~ 1, locations = precinct_points, 
                  nmax = 10, set = list(idp = 3))

# Make predictions
point_estimates = gridded_points %>%
  # Let's just run the function and extract the variable var1.pred from it
  mutate(y1 = predict(m1, newdata = .)$var1.pred,
         y2 = predict(m2, newdata = .)$var1.pred,
         y3 = predict(m3, newdata = .)$var1.pred)
# Now you can compare them!
point_estimates

# Last, let's join these back into your grid
gridded_estimates = grid %>%
  left_join(
    by = "cell", 
    y = point_estimates %>% as_tibble() %>% select(cell, y1,y2,y3))

# Let's plot each different level of weighting, for comparison

# Inverse Distance Weighting x 1
g1 = ggplot() +
  geom_sf(data = gridded_estimates, mapping = aes(fill = y1), color = "white") +
  geom_sf(data = boston, fill = NA, color = "black") +
  scale_fill_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Inverse Distance\nWeighting = 1")

# Inverse Distance Weighting x 2 
g2 = ggplot() +
  geom_sf(data = gridded_estimates, mapping = aes(fill = y2), color = "white") +
  geom_sf(data = boston, fill = NA, color = "black") +
  scale_fill_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Inverse Distance\nWeighting = 2")

# Inverse Distance Weighting x 3
g3 = ggplot() +
  geom_sf(data = gridded_estimates, mapping = aes(fill = y3), color = "white") +
  geom_sf(data = boston, fill = NA, color = "black") +
  scale_fill_viridis(option = "plasma") +
  theme_void() +
  labs(title = "Inverse Distance\nWeighting = 3")

# Let's use the ggpubr package to compare these maps side by side
library(ggpubr)

ggarrange(plotlist = list(g1,g2,g3), ncol = 3, legend = "bottom", common.legend = TRUE) 
