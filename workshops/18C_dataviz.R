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
library(plotly) # for interactive visuals

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

counties

# Get US states surrounding NY
states = read_sf("data/transportation/states.geojson")  %>%
  filter(state %in% c("NY", "PA", "NJ", "VT", "NH", "CT", "MA"))

states

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
# We're showing the rate of cars per million residents

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
    name = "Passenger Car\nEmissions\nper million\nresidents",
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
       subtitle = "in New York Counties (n = 62) in 2025",
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
  # Clarify outline colors
  scale_color_manual(values = c("black", "darkgrey")) +
  # ditch the name of the legend; don't really need it.
  labs(color = NULL) +
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 



# 11. Buffer Zone ####################################

# Although, it would be nice to be able to show instead, 
# here's the general range we think might be affected.

# the coordinate reference system appears 
# to be in LENGTHUNIT = "meters" 
# So we'll write the distance for our buffer radius in meters
st_crs(poly_max)

# Create a buffer of 100 km around the centroid of the max polygon
poly_buffer = poly_max %>%
  # Get centroid of polygon
  summarize(geometry = st_centroid(geometry)) %>%
  # Turn centroid into polygon buffer of 100 km (100 m * 1000)
  summarize(geometry = st_buffer(geometry, dist = 100*1000))

# Draw a buffer,
# and let's use the color aesthetic as a trick 
# to add a label to the legend.
gg4 +
  geom_sf(data = poly_max, fill = NA, linewidth = 1.25,
          mapping = aes(color = "Highest Rate")) +
  geom_sf(data = poly_buffer, fill = "pink", alpha = 0.25,
          mapping = aes(color = "100 km Zone\nof Influence")) +
  scale_color_manual(values = c("pink", "black")) +
  labs(color = NULL) +
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 
# We're implying with the buffer that the surrounding polygons
# are influenced by the max polygon.


# 12. Bounding Box ###################################

# Alternatively, we could make a bounding box to focus everyone's attention.

# Find our polygon with the max
poly_max = counties %>%
  filter(rate_cars == max(rate_cars))

# Get the neighbors
poly_neighbors = counties %>%
  select(geoid, geometry) %>%
  st_join(y = poly_max %>% select(geometry),
          join = st_touches, left = FALSE)

# Get the bounding box around the neighboring polygons
poly_nbox = poly_neighbors %>%
  st_bbox() %>%
  st_as_sfc() %>%
  tibble(geometry = .) %>%
  st_as_sf(crs = 4326)

# Visualize the bounding box as a study region
gg4 +
  geom_sf(data = poly_nbox, fill = NA,  linewidth = 1,
          mapping = aes(color = "Study Region")) + 
  # Make the box black, with no legend title
  scale_color_manual(values = "black") +
  labs(color = NULL) +
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 


# 13. Magnify ###################################

# Alternatively, we could magnify a section of the map,
# using a bounding box. Let's try it, narrowing into NYC counties.

# Suppose we want to see New York City up close.
poly_nyc = counties %>%
  # Filter by county name to the 5 NYC boroughs
  filter(name %in% c("Queens County", "Kings County", 
                     "Bronx County", "New York County",
                     "Richmond County"))
# Get bounding box coordinates around nyc
bb = poly_nyc %>%
  st_bbox() 
# Get a magnification box from those coordinates
poly_magnify = bb %>%
  st_as_sfc() %>%
  tibble(geometry = .) %>%
  st_as_sf(crs = 4326)

# Let's make a box around it!
gg5a = gg4 + 
  geom_sf(data = poly_magnify, fill = NA, color = "black", linetype = 1.5) +
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 

gg5a




# Now, make the magnified version
gg5b = gg4 + 
  # Plot the box...
  geom_sf(data = poly_magnify, fill = NA, color = "black", 
          # Increase linewidth, since you'll be zoomed in
          linewidth = 2) +
  # Crop the plot to the bounding box extend
  coord_sf(
    xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), 
    # say expand = FALSE to crop EXACTLY to the bounding box, with no wiggle room.
    expand = FALSE)  +
  # Overwrite the labels, to better fit the new content
  labs(title = "New York City Counties (n = 5)",
       caption = NULL, subtitle = NULL) +
  # Drop the legend
  guides(fill = "none")

gg5b
# Okay, this is more boring than I expected,
# but still a fine proof of concept

gg5 = ggarrange(
  plotlist = list(gg5a, gg5b), 
  ncol = 2, nrow = 1,
  # Size the plot widths as 2-to-1
  widths = c(2,1),
  # Size the plot heights as 2-to-1
  heights = c(2,1))

# Save it to file.
# play around with the height and width in ggsave()
# until you get it right.
ggsave(gg5, filename = "workshops/18C_visual_gg5.png", dpi = 300, width = 10, height = 6)

# View it
browseURL("workshops/18C_visual_gg5.png")


# Note: ggarrange outputs can't become plotly objects
# Try it - it won't work.
ggplotly(gg5)



# 14. Annotation #####################

# How can we annotate our maps?
# Well, fortunately, all ggplot objects are a bunch of x,y coordinates.
# So as long as we make a data.frame of annotations with x,y coordinates,
# we can pipe those into geom_text() or geom_label()

# Look at the bounding box
st_bbox(counties)

# Let's make a test note
note0 = tibble(
  x = -79,
  y = 44,
  label = "Stuff"
)

# Here the text!
gg4 +
  geom_text(data = note0, mapping = aes(x = x, y = y, label = label))

# Or if we want a border, here's another way to do it.
gg4 +
  geom_label(data = note0, mapping = aes(x = x, y = y, label = label))


# Let's try a more meaningful annotation.
poly_max %>%
  select(name, cars, rate_cars, pop)

# Hamilton County has the highest rate of emissions from cars
# 0.1 tons, 22.9, with ~4000 residents

note1 = poly_max %>%
  select(name, cars, rate_cars, pop) %>%
  as_tibble() %>%
  summarize(
    label = paste0(
      "Highest Emissions Rate ",
      "\n     ", 
      name,
      "\n     ",
      round(rate_cars, 1), " tons of SO2 per 1M residents"
    ))


gg6 = gg4 +
  # label the highlight polygon
  geom_sf(data = poly_max, fill = NA, color = "black", linewidth = 1) +
  # Add some text
  geom_text(data = note1,
            # Customize the x and y directly
            mapping = aes(x = -79.5, y = 44.7, label = label),
            # Format the text
            # left-justify the text, bold it, make it black
            hjust = 0, fontface = "bold", color = "darkgrey") + 
  # Recrop, since we edited feature locations
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45)) 

gg6


# 15. Lines #######################################

# How do we know that Hamilton County refers to the black outlined county?
# We can draw an arrow from the annotation to the feature

# Might help to look at the coordinates of that polygon's bounding box
poly_max %>%
  st_bbox()

gg7a = gg6 +
  geom_segment(
    mapping = aes(
      # Tweak the original text point location
      x = -78, y = 44.3,
      # Guestimate the edge of the polygon
      xend = -74.86, yend = 43.5),
    color = "darkgrey", linewidth = 0.75
  ) +
  # Recrop, since we edited feature locations
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45), expand = FALSE) 


gg7a

# Need a curve instead? Try geom_curve()
gg7b = gg6 +
  geom_curve(
    mapping = aes(
      x = -78, y = 44.3,
      xend = -74.86, yend = 43.5),
    color = "darkgrey", linewidth = 0.75, 
    # Adjust curvature
    # Negative values make left-handed curve;
    # Positive values make right-handed curve;
    # Zero makes a straight line
    curvature = 0.25
  ) +
  # Recrop, since we edited feature locations
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45), expand = FALSE) 

# View it
gg7b



# 16. Arrow + Scale #######################################

# Always clarify north and scale in your map,
# using ggspatial's functions

gg7b +
  # Add north arrow to top right
  ggspatial::annotation_north_arrow(location = "tr") +
  # Add scale to bottom left
  ggspatial::annotation_scale(location = "bl")


# 17. Interactivity ######################################

# Plotly can handle geom_segment(), but not geom_curve()
# It will also transplant your geom_text()
ggplotly(gg7a)

# Might need to adjust label position.
# Honestly, if you're making a plotly,
# it makes more sense to just put your annotations into hoverlabels.


# 18. Final Visual #####################################

# Here's a final visual, all in one swoop.


poly_ny = states %>% filter(state == "NY")

poly_max = counties %>%
  filter(rate_cars == max(rate_cars))

note1 = poly_max %>%
  select(name, cars, rate_cars, pop) %>%
  as_tibble() %>%
  summarize(
    label = paste0(
      "Highest Emissions Rate ",
      "\n     ", 
      name,
      "\n     ",
      round(rate_cars, 1), " tons of SO2 per 1M residents"
    ))


gg = ggplot() +
  # Blank background
  theme_void(base_size = 14) + 
  # Very bland, light background of states
  geom_sf(data = states, fill = "lightgrey", 
          color = "white", linewidth = 1.25) +
  # Highlight new york state over it, but not too much
  # Add a thick linewidth - that'll help later.
  geom_sf(data = poly_ny, fill = NA, color = "grey", linewidth = 3) +
  # Map the main trend - chloropleth heatmap
  geom_sf(data = counties, mapping = aes(fill = rate_cars),
          # Do ourselves a favor and make the outlines white and thin
          color = "white", linewidth = 0.5) +
  # Color Palette
  scale_fill_gradient2(
    low = "#648FFF", high = "#DC267F", 
    mid = "white", midpoint = log(6.975),
    trans = "log", na.value = "grey",
    labels = scales::label_number(accuracy = 1),
    # Add breaks that capture variation on a natural log scale okay
    breaks = c(1, 2, 4, 7, 14, 21),
    # Specify a discrete colorbar (steps)
    guide = guide_colorsteps(barheight = 10, show.limits = TRUE),
    # And add a name
    name = "Passenger Car\nEmissions\nper million\nresidents"
  ) +
  # Labelling
  labs(title = "Expected Sulfur Dioxide Emissions per Capita",
       subtitle = "in New York Counties (n = 62) in 2025",
       caption = paste0(
         "Your Name Here, Affiliation",
         "\n", # linebreak
         "Predicted using EPA MOVES 3.1 software.")) +
  # Center the plot title
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
  # Center the plot subtitle
  theme(plot.subtitle = element_text(hjust = 0.5, face = "italic")) +
  # Move the caption
  theme(plot.caption = element_text(hjust = 0))  +
  
  geom_curve(
    mapping = aes(
      x = -78, y = 44.3,
      xend = -74.86, yend = 43.5),
    color = "darkgrey", linewidth = 0.75, 
    # Adjust curvature
    # Negative values make left-handed curve;
    # Positive values make right-handed curve;
    # Zero makes a straight line
    curvature = 0.25
  ) +
  # label the highlight polygon
  geom_sf(data = poly_max, fill = NA, color = "black", linewidth = 1) +
  # Add some text
  geom_text(data = note1,
            # Customize the x and y directly
            mapping = aes(x = -79.5, y = 44.7, label = label),
            # Format the text
            # left-justify the text, bold it, make it black
            hjust = 0, fontface = "bold", color = "darkgrey") +
  # Add north arrow to top right
  ggspatial::annotation_north_arrow(location = "tr") +
  # Add scale to bottom left
  ggspatial::annotation_scale(location = "bl") +
  # Crop the plot
  coord_sf(xlim = c(-79.5, -72), ylim = c(40.5, 45), expand = FALSE) 


ggsave(gg, filename = "workshops/18c_visual_gg.png",
       dpi = 300, width = 8, height = 6)

browseURL("workshops/18C_visual_gg.png")



# Phew! That was a lot of layers!

# Good work!
# Let's clean up.
rm(list = ls())