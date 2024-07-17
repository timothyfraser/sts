#' @name 18C_dataviz.R
#' @title Lesson 18: Data Visualization - Making Better Maps
#' @author Tim Fraser
#' @description
#' This script walks through examples of how we can improve our maps,
#' referring to the R tools we outlined in slides.


# 0. Setup #########################

## 0.1 Packages ########################

library(dplyr) # for data wrangling
library(readr) # for reading data
library(ggplot2) # for visualization
library(viridis) # for color palettes
library(sf) # for mapping 
library(ggspatial) # for map annotations
library(ggpubr) # for combining visuals

## 0.2 Data ############################

# For this analysis, let's use the New York Counties emissions datasets,
# in data/transportation/

# Get sulfur dioxide SO2 emissions by vehicle type per county per year (id code 31)
db = read_rds("data/transportation/emissions.rds") %>%
  filter(pollutant == 31) %>% # filter by pollutant
  filter(by == 8) %>% # filter to total per vehicle type (sourcetype)
  select(year, geoid, sourcetype, emissions)

# Get population estimates for that county
mypop = read_rds("data/transportation/projections.rds") %>%
  select(geoid, year, pop) %>%
  filter(year == 2025)

# Now calculate some variable of interest
# Let's find... SO2 emissions for trucks vs. cars in 2025 per county, per capita
data = db %>%
  filter(year == 2025) %>%
  group_by(year, geoid) %>%
  reframe(cars = emissions[sourcetype == 21],
          trucks = emissions[sourcetype == 31]) %>%
  # Join in population by year and geoid
  left_join(by = c("geoid", "year"), y = mypop) %>%
  # Calculate rates per million residents
  mutate(rate_cars = cars / pop * 1e6,
         rate_trucks = trucks / pop * 1e6) %>%
  # Calculate difference in rate per capita
  mutate(rate_diff = rate_trucks - rate_cars)

# View the result
data

# Next, let's join this information into some county polygons

# Get just NY counties
counties = read_sf("data/transportation/counties.geojson") %>%
  # Filter to new york
  filter(state == "NY") %>%
  # Join in dta by county
  left_join(by = "geoid", y = data)

# Get US states surrounding NY
states = read_sf("data/transportation/states.geojson")  %>%
  filter(state %in% c("NY", "PA", "NJ", "VT", "NH", "CT", "MA"))

# We've even got roads in New York state, by county
# (pretty big file)
roads = read_rds("data/transportation/roads.rds")



# 1. Basic Map ########################################

# Let's start out by making the basic map.

ggplot() +
  # Blank background
  theme_void(base_size = 14) +
  # Map the main trend - chloropleth heatmap
  geom_sf(data = counties, mapping = aes(fill = rate_cars),
          # Do ourselves a favor and make the outlines white and thin
          color = "white", linewidth = 0.5)
  

# 2. Sense of Space ########################################

# Clarify the sense of space,
# by adding a state background
# and by adding other states, in grey

poly_ny = states %>% filter(state == "NY")

gg0 = ggplot() +
  # Blank background
  theme_void(base_size = 14) + 
  # Very bland, light background of states
  geom_sf(data = states, fill = "lightgrey", 
          color = "white", linewidth = 1.25)

gg0

# Let's add NY on top of that
gg0 = gg0 +
  # Highlight new york state over it, but not too much
  # Add a thick linewidth - that'll help later.
  geom_sf(data = poly_ny, fill = NA, color = "grey", linewidth = 3)
gg0


# Overlay the map
gg0 = gg0 +
  # Map the main trend - chloropleth heatmap
  geom_sf(data = counties, mapping = aes(fill = rate_cars),
          # Do ourselves a favor and make the outlines white and thin
          color = "white", linewidth = 0.5)

gg0

# 3. Cropping ###################################

# Let's crop it to the NY extent, so it looks really nice.
# We could do just counties, but to give a sense of place,
# let's show the surrounding states.

# Why guess? Just take the new york state polygon and get its bounding box
st_bbox(poly_ny)


# Or, test out coordinates until you get it.
gg1 = gg0 +
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 

gg1


# 2. Color Palette ###################################

# Let's choose our color palette
# We're showing the rate of cars per capita.

# We could do a few different color palettes.

# Get a colorblind friendly blue and red
# https://davidmathlogic.com/colorblind/#%23648FFF-%23785EF0-%23DC267F-%23FE6100-%23FFB000
# I like these:
# "#648FFF" - blue
# "#DC267F" - red

# Split the scale at zero, and specify the high and mid color...
gg1 +
  scale_fill_gradient2(high = "#DC267F", mid = "white",
                       midpoint = 0, na.value = "grey")

# Or, find a meaningful quantity of interest, like the median rate of cars
# use that to split the color scale,
# so we tell a story about hotspots and coldspots

# Let's save m as our median
m = median(counties$rate_cars)
m # 6.97

# Now plot points in reference to this midpoint.
gg1 +
  scale_fill_gradient2(low = "#648FFF", high = "#DC267F", 
                       mid = "white", midpoint = 6.975,
                       na.value = "grey")




# 3. Transformations ##################################

# It would be better if our color palette
# followed a log scale
# while letting us still see the normal values

# There's almost definitely right skew happening here.
# Let's try a log transformation.
gg1 +
  scale_fill_gradient2(low = "#648FFF", high = "#DC267F", 
                       mid = "white", midpoint = log(6.975),
                       trans = "log", na.value = "grey")

# There's clearer distinctions here now. That's good.



# 4. Scale Labels ###########################################

# Can we make the labels for our breaks nicer?
# We can use scales::label_number()
# this function can only be used inside ggplot.
# we'll set the accuracy level to 1 decimal place, as 0.1
# scales::label_number(accuracy = 0.1)

gg1 +
  scale_fill_gradient2(
    low = "#648FFF", high = "#DC267F", 
    mid = "white", midpoint = log(6.975),
    trans = "log", na.value = "grey",
    # Add prettier labels
    labels = scales::label_number(accuracy = 0.1)
  )

# Alternatively, you can manually do it yourself
gg1 +
  scale_fill_gradient2(
    low = "#648FFF", high = "#DC267F", 
    mid = "white", midpoint = log(6.975),
    trans = "log", na.value = "grey",
    # Add prettier labels
    breaks = c(2.7, 7.4, 20.1),
    labels = c("2.7", "7.4", "20.1")
  )



# 5. Scale Breaks ###########################################

# These breaks feel a little arbitrary. Can we get something that
# will feel more normal to the reader?

# Find range
counties$rate_cars %>% range()
# What's the median - that's our midpoint. Let's label it.
counties$rate_cars %>% median()

# With log scales, I usually do 0.1,0.3,1,3,10,30,100,300, etc.
# This one doesn't work so well, but I encourage repetitiveness.
# Here's one that fits okay.

gg1 +
  scale_fill_gradient2(
    low = "#648FFF", high = "#DC267F", 
    mid = "white", midpoint = log(6.975),
    trans = "log", na.value = "grey",
    labels = scales::label_number(accuracy = 1),
    # Add breaks that capture variation on a natural log scale okay
    breaks = c(1, 2, 4, 7, 14, 21)
  )


# 6. Color steps ################################

# We can also customize the color bar 
# to be an interval vs. discrete categories,
# using the `guide` term in the scale_fill_ functions.

# You have two options
# - guide_colorbar()   --> interval
# - guide_colorsteps() --> discrete steps

# We can also customize lots of attributes about the legend here.
# -- show.limits = TRUE --> turns on the limits on the scale
# -- barwidth = 10 --> makes the color legend wider
# -- barheight = 10 --> makes the color legend taller

gg2 = gg1 +
  scale_fill_gradient2(
    low = "#648FFF", high = "#DC267F", 
    mid = "white", midpoint = log(6.975),
    trans = "log", na.value = "grey",
    labels = scales::label_number(accuracy = 1),
    breaks = c(1, 2, 4, 7, 14, 21),
    # Specify a discrete colorbar (steps)
    guide = guide_colorsteps(barheight = 10, show.limits = TRUE),
    # And add a name
    name = "Passenger Car\nEmissions\nper capita",
  )

gg2

# 7. Labelling ######################################

# Let's do some basic storytelling using the labels.

# How many counties? 
counties %>% as_tibble() %>% 
  select(geoid) %>% distinct() %>% summarize(count = n())
# How many years 
counties %>% as_tibble() %>% 
  select(year) %>% distinct() %>% summarize(count = n())
# What year? - 2025
counties %>% as_tibble() %>% 
  select(year) %>% distinct()
# Data source? - EPA MOVES software

gg3 = gg2 + 
  labs(title = "Expected Sulfur Dioxide Emissions per Capita",
       subtitle = "in New York Counties in 2025",
       caption = paste0(
         "Your Name Here, Affiliation",
         "\n", # linebreak
         "Predicted using EPA MOVES 3.1 software."))

# Good clear labelling
gg3

# 8. Theming #############################################

# Can we move around some things?
gg4 = gg3 +
  # Center the plot title
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  # Center the plot subtitle
  theme(plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
  # Move the caption
  theme(plot.caption = element_text(hjust = 0)) 

gg4


# Can we actually move the legend a little, 
# to share some of the white space?
# Sure can. It's an x, y scale from 0 to 1 each
gg4 +
  theme(legend.position = c(0.5, 0.5))

# This could work, if not for the states
gg4 +
  theme(legend.position = c(0.95, 0.6))

# Let's stick with this
gg4

# You can add a background color
gg4 +
  theme(legend.position = c(0.95, 0.6)) +
  theme(legend.background = element_rect(fill = "grey", color = NA))


# You can make it transparent, if you add 2 digits to the hexadecimal code 
# for the fill
# eg for #373737 I added 33 (as in 33% opaque)
gg4 +
  theme(legend.position = c(0.5, 0.6)) +
  theme(legend.background = element_rect(fill = "#37373733", color = NA))




# 9. Highlighting #######################################

# Let's start highlighting locations,
# to do some storytelling.

# Find me the polygon with the max 
poly_max = counties %>%
  filter(rate_cars == max(rate_cars))
# The polygon with the min
poly_min = counties %>% 
  filter(rate_cars == min(rate_cars))
# And the polygon nearest the median
poly_med = counties %>%
  arrange(rate_cars) %>%
  mutate(rank = 1:n() ) %>%
  mutate(distance = rank - round(n()/2, 0) ) %>%
  filter(distance == 0)

# Bundle them into one data.frame...
highlights = bind_rows(poly_max, poly_min, poly_med)

# Let's use outlining to highlight that these are 
# noteworthy cases
gg4 +
  geom_sf(data = highlights, fill = NA, color = "black", linewidth = 1.25) +
  # We'll need to reapply coord_sf anytime we change the geometry.
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 


# You could even add to the legend an indication of what these are.
# The color legend is free - let's use that.
gg4 +
  geom_sf(data = highlights, fill = NA, linewidth = 1.25,
          mapping = aes(color = "Max / Median / Min")) +
  scale_color_manual(values = "black",
                     name = "Cases") +
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 

# I don't think the label adds much right now.



# 10. Neighbors ###########################################################

# Maybe we might want to highlight a pattern of diffusion
# Eg. where Albany (max) has high emissions, 
# and the counties surrounding it do it.

# Find our polygon with the max
poly_max = counties %>%
  filter(rate_cars == max(rate_cars))

# Get the neighbors
poly_neighbors = counties %>%
  select(geoid, geometry) %>%
  st_join(y = poly_max, join = st_touches, left = FALSE)


# Then highlight both, using black and grey
gg4 +
  geom_sf(data = poly_neighbors, fill = NA, linewidth = 1.25,
          mapping = aes(color = "Neighbors")) +
  geom_sf(data = poly_max, fill = NA, linewidth = 1.25,
          mapping = aes(color = "Highest Rate")) +
  scale_color_manual(values = c("black", "darkgrey")) +
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 






