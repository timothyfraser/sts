#' @name 19P_analytics.R
#' @title Practice 19P: Analytics - Spatial Variables & Autocorrelation 
#' @author Tim Fraser
#' @description 
#' Practice Exercises for 19C_analytics

# Set working directory back to normal
# setwd("/cloud/project/")

# Load packages
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(sf)
library(broom)

# Let's get a snapshot of Japanese municipalities in 2018, 
# recording the number of ground-mounted, utility scale solar power plants
# in each municipality.
data = read_csv("data/jp_solar_farms_2018.csv")  %>%
  # Let's narrow into just the sites in Tohoku, the northeast region
  filter(region == "Tohoku")


# Let's do some data description.

# How many municipalities?
data %>% summarize(count = n())

# How many municipalities in what prefectures?
data %>% group_by(pref) %>% summarize(count = n())


# We're going to look at the sp variable - describing total solar adoption,
# and the pv_output_2018 variable, 
# describing projected solar photovoltaic output 
# from the World Bank's Global Solar Atlas in 2018
# It basically describes sunniness.

subset = data %>% 
  select(muni_code, muni, pref, sp, pv_output_2018) 
subset

# Is there a relationship between sunniness and solar?
# (let's log transform, because solar is a count. )
# (We'll add 1 since there are some cities with 0 solar plants
# want to be able to log transform them.)
m = subset %>% 
  lm(formula = log(sp + 1) ~ pv_output_2018)

# Some relationship - some explanatory power
# About 75% of variation remains unexplained.
broom::glance(m)

# Positive slope, uber significant.
broom::tidy(m)

# Let's get the predictions and residuals from our model object
stat = broom::augment(m) %>% 
  # Rename the outcome so it's easier to query
  rename(y = `log(sp + 1)`) %>%
  # Add the municipality code in for joining
  mutate(muni_code = subset$muni_code,
         pref = subset$pref)


# Let's view that relationship

gg = ggplot() +
  # Plot the raw points
  geom_point(data = stat, mapping = aes(x = pv_output_2018, y = y)) +
  # Plot the line of best fit
  geom_line(data = stat, mapping = aes(x = pv_output_2018, y = .fitted),
            color = "blue", linewidth = 1.25)

gg # View it


# Here's another way, using the raw data
# gg = ggplot() +
#   # Plot the points
#   geom_point(data = subset, mapping = aes(x = pv_output_2018, y = log(sp + 1) )) +
#   # Let's plot a line of best fit there
#   geom_smooth(data = subset, mapping = aes(x = pv_output_2018, y = log(sp + 1)), 
#               method = "lm", se = FALSE)


# Let's view the distance - the error - between each prediction and the observed outcome

gg = gg +
  # Plot lineranges, for each pv output, giving the observed versus the prediction
  geom_linerange(data = stat, mapping = aes(x = pv_output_2018, ymin = y, ymax = .fitted), 
                 # Let's make them blue and transparent,
                 # to signify a conceptual relationship with the line of best fit.
                 color = "blue", alpha = 0.25)

# This is our model error! Cool, right?
gg



# Let's try and map our predictions, 
# to see if our predicted values cluster spatially.
# Let's get some municipality polygons too.
shapes = read_sf("data/japan/municipalities.geojson") %>%
  filter(region == "Tohoku") %>%
  # Join in the predictions
  left_join(by = c("muni_code"), y = stat)

# Map the original outcome
# A lot of clustering here...
gg1 = ggplot() +
  geom_sf(data = shapes, mapping = aes(fill = y), color = "white") +
  scale_fill_viridis(option = "plasma") +
  theme_void()
gg1

# Map the predictions
# Whoa! Talk about a spatial pattern!
gg2 = ggplot() +
  geom_sf(data = shapes, mapping = aes(fill = .fitted), color = "white") +
  scale_fill_viridis(option = "plasma") +
  theme_void()
gg2

# Let's try the model residuals.
# Yeah, there are definitely ares that tend to
# share the same amount of error with their neighbors.
gg3 = ggplot() +
  geom_sf(data = shapes, mapping = aes(fill = .resid), color = "white") +
  scale_fill_viridis(option = "plasma") +
  theme_void()
gg3


# I'm going to save these here for class
ggsave(gg1, filename = "workshops/19P_analytics_y.png", dpi = 300, width = 4, height = 7)
ggsave(gg2, filename = "workshops/19P_analytics_yhat.png", dpi = 300, width = 4, height = 7)
browseURL("workshops/19P_analytics_yhat.png")

# What spatial variables could we calculate here?
# - average neighbor's solar?

# For each polygon, get me the values of the polygon that neighbors it
neighbor_values = shapes %>%
  select(muni_code, geometry) %>%
  st_join(y = shapes %>% select(adj_y = y), join = st_touches, left = FALSE) %>%
  # Convert to tibble for data wrangling
  as_tibble() %>%
  # For each municipality,
  group_by(muni_code) %>%
  # Take the mean of the adjacent municipalities' logged solar count
  summarize(adj_y = mean(adj_y))

# We can join these values into the data.frame
stat2 = stat %>%
  left_join(by = c("muni_code"), y = neighbor_values)

stat2 # View it

stat2 %>%
  select(muni_code, pref, y, adj_y, pv_output_2018)

# What's the relationship between log-solar in a city
# and the average log-solar of its neighboring cities? 
stat2 %>%
  lm(formula = y ~ adj_y) 

# What about after controlling for solar PV potential?
stat2 %>%
  lm(formula = y ~ adj_y + pv_output_2018)

# What about after controlling for the 
# spatial tendency of certain prefectures to have more than others?
stat2 %>%
  lm(formula = y ~ adj_y + pv_output_2018 + pref) 

# Can we calculate some quantities of interest?

m = stat2 %>%
  lm(formula = y ~ adj_y + pv_output_2018 + pref) 
exp(0)

scenario = stat2 %>%
  # Suppose in Fukushima prefecture
  filter(pref == "Fukushima") %>%
  group_by(pref) %>%
  reframe(
    # Suppose the neighboring municipalities had this average amount,
    # in logged solar
    adj_y = seq(from = 1, to = 250, by = 10) %>% log(),
    # And this median amount of solar PV 
    pv_output_2018 = median(pv_output_2018)
  ) %>%
  mutate(id = 1:n()) %>%
  # Get predictions and sigma for each scenario
  mutate(yhat = predict(m, newdata = .),
         sigma = glance(m)$sigma)
  
# Simulate and back-transform
sims = scenario %>%
  group_by(adj_y, id) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp() - 1)

# Get me some quantities of interest
qis = sims %>%
  group_by(adj_y) %>%
  summarize(estimate = median(ysim),
            lower = quantile(ysim, probs = 0.025),
            upper = quantile(ysim, probs = 0.975)) %>%
  # Don't forget to back-transform the adjancent log-solar avergae
  mutate(adj_y = exp(adj_y) - 1)

# Let's view then, what happens as neighbors' solar increases, on average?
ggplot() +
  geom_ribbon(data = qis, mapping = aes(x = adj_y, ymin = lower, ymax = upper), 
              alpha = 0.25, fill = "purple") +
  geom_line(data = qis, mapping = aes(x = adj_y, y = estimate)) +
  # Transform the y-axis for clearer viewing
  scale_y_continuous(trans = "log", labels = scales::label_number(accuracy = 0.1)) 


# Nice! Congrats! You can calculate a spatial variable AND simulate with it!

# Cleanup
rm(list = ls())
