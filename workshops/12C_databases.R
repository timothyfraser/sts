# 12C_databases.R
# Databases IV: Spatial Joins & Aggregation
# Topic: Using GIS to Make Spatial Joins, Spatially Aggregate, and Make Area Calculations
# Tim Fraser

# Setup ###########################################

# Load Packages
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(viridis)

# Equal Area projection
aea <- "+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

# Equal Distance projection
aed <- "+proj=eqdc +lat_0=0 +lon_0=0 +lat_1=33 +lat_2=45 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "

# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Look up more projections at spatialreference.org - proj4
#https://spatialreference.org/ref/epsg/wgs-84/

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


