#' @name 19C_analytics.R
#' @title Lesson 19: Analytics 3 - Spatial Autocorrelation
#' @author Tim Fraser
#' @description
#' This lesson introduces the concept of spatial autocorrelation,
#' how to measure it, and how to adjust for it when modeling predictions.

# 0. Setup #############################

## 0.1 Packages ###########################
library(dplyr)
library(readr)
library(ggplot2)
library(viridis)
library(sf)
library(broom)

## 0.2 Data ################################

# We're going to reconstruct a dataset of Boston grid cells,
# including the rate of social infrastructure sites and related covariates.

grid = read_sf("data/boston_social_infra/boston_grid.geojson")
bg = read_sf("data/boston_social_infra/boston_block_groups.geojson")
sites = read_sf("data/boston_social_infra/boston_social_infra.geojson")
census = read_csv("data/boston_social_infra/boston_census_data.csv")

# Take all cells...
data = grid %>%
  # left join in all sites, spatially
  st_join(y = sites) %>%
  # For each cell, get total social infrastructure sites
  as_tibble() %>%
  group_by(cell) %>%
  summarize(sites = n()) %>%
  # Join back in the geometry
  left_join(by = "cell", y = grid) %>%
  # Join in covariates
  left_join(by = "cell", y = census) %>%
  # Calculate rate of sites by population density
  mutate(rate = sites / pop_density * 1000) %>%
  # Fix NAs.
  mutate(rate = if_else(is.infinite(rate), true = NA, false = rate))  %>%
  # While we're at it, let's also grab the x and y coordinates of each centroid
  mutate(geometry %>% st_centroid() %>% 
           st_coordinates() %>%
           as_tibble() %>% select(x = 1, y = 2)) %>%
  # Finally, let's drop NAs in our key variables
  filter(rate != "NA", median_income != "NA")

data #view it



# We're going to keep it as a tibble;
# if we ever need to make it back into a spatial data.frame, 
# we can use sf::st_as_sf()

# View variable
data$rate %>% summary()

# Show the geographic extent too!
data$x %>% summary()
data$y %>% summary()


# Clear excess data
remove(bg, census, grid, sites)







# 1. Evaluating Autocorrelation ############################################

## 1.1 Modeling an Effect #######################

# We might hypothesize that the rate of social infrastructure in Boston 
# is impacted by the wealth of the neighborhood.
# It shouldn't be, but it probably is.
# So, we might want to make clear, 
# for every extra dollar the average household earns in an area,
# how much more social infrastructure do they host?

# We know now that we can do that pretty quickly.

# We can check our outcome variable rates for right skew
data$rate %>% hist() # view the histogram. Is it normal? (No)
data$rate %>% log() %>% hist() # does logging it make it more normal? (yes!)

# We can make a linear model 'm1'
m1 = data %>% lm(formula = log(rate) ~ median_income) 

m1

# View its model fit - pretty small; understandable.
broom::glance(m1)

# View it
m1


# How well does this model conceptually fit the data?

# Let's use augment() from broom to get back a data.frame
# describing each row of valid data supplied to the model.
# We'll call it 'stat' for predictions, because it includes many.
stat = m1 %>% broom::augment() 
# We can use this to find out a lot of important information.





# Let's look at the residuals - the error in your predictions
# Residuals (.resid) = Observed Value (y) - Predicted Value (.fitted)
stat %>%
  select(y = `log(rate)`, .fitted, .resid, .std.resid)

# This includes:
# - y = observed value (log-rate)
# - .fitted = predicted value (predicted log-rate)
# - .resid = residual/error (error in predicted log-rate)
# - .std.resid = standardized residual/error (standardized error in predicted log-rate, mean-centered at 0)


# Linear models (and most models) assume that 
# the error in model predictions 
# is randomly, normally distributed.
# That allows us to say
# Y = Intercept + Beta*X + Error

# When we calculate standard errors for beta coefficients,
# we rely on that assumption of **normally distributed error**

# But what if that error is NOT normally distributed?

# We can test it two ways.
# - histogram of standardized residuals
# - scatterplot of residuals and predicted values



## 1.2 Histogram of Residuals #########################

# That's a tad skewed
stat$.std.resid %>% hist()


# Let's build a normal curve real quick
example = tibble(
  x = seq(from = -3, to = 3, length.out = 50),
  density = dnorm(x, mean = 0, sd = 1),
  count = nrow(stat) * density # estimate the count for your sample size
) 


# How do our observed residuals compare to a normal distribution?
ggplot() +
  geom_density(
    data = stat,
    mapping = aes(x = .std.resid, y = after_stat(count),
                  fill = "observed"), 
    alpha = 0.5) +
  geom_area(
    data = example, 
    mapping = aes(x = x, y = count,
                  fill = "random"), 
    alpha = 0.5) 

# Pretty funky. Not perfect.

remove(example)



## 1.3 Scatterplot of Residuals and Fitted Values ##################

# Additionally/alternatively, we can detect issues with residuals
# by comparing the predicted values against the residuals (error)
# If there is no relationship, it should look like noise.
# A Loess-smoothed curve would be flat, about against the 0 yintercept.

# If there is a relationship, then a loess-smoothed curve would NOT be flat,
# and it would differ greatly from the yintercept.

# Let's plot it.

# Base plot
ggplot() +
  # Scatterplot...
  geom_point(data = stat, mapping = aes(x = .fitted, y = .resid)) +
  # Add a yintercept at 0...
  geom_hline(yintercept = 0, linetype = "dashed") +
  # Add a loess smoothed curve, with no confidence interval (se = FALSE)
  geom_smooth(data = stat, mapping = aes(x = .fitted, y = .resid), 
              method = "loess", se = FALSE)



# Okay, this suggests to me that something is definitely up.
# Certain levels of predictions do tend to have similar levels of error.
# That is autocorrelation.

# How are we going to handle this?




# 2. Spatial Autocorrelation ######################################

# First, let's check and see if our error is spatially autocorrelated,
# as opposed to autocorrelated some other way.

# Join the stats into your data.frame and make a spatial data.frame
s = data %>%
  # Add stat in, since it's the same number of rows
  mutate(stat) %>%
  # Make it into a spatial data.frame, with wgs projection (4326)
  st_as_sf(crs = 4326)




# Let's map our predictions and residuals
# First, do our predictions tend to cluster spatially?
# Eg. do nearby cells have similar predictions??
ggplot() +
  geom_sf(data = s, mapping = aes(fill = .fitted)) +
  scale_fill_viridis(option = "plasma")




# Second, do our residuals tend to cluster spatially?
# Eg. do nearby cells have similar levels of error??
ggplot() +
  geom_sf(data = s, mapping = aes(fill = .std.resid)) +
  scale_fill_viridis(option = "plasma")




# Quite!!

# Okay, so we're probably looking at some spatial autocorrelation here.

# Can we measure the degree of spatial autocorrelation?




# 3. Accounting for Spatial Autocorrelation #################################

## 3.1 Neighborhood Effects ###########################################

# A basic way to handle spatial autocorrelation

# You might just be able to say, hey, these grid cells are in the same neighborhood;
# the neighborhood accounts for the difference.
# Control for the neighborhood in your model.

data %>% glimpse()
m2 = data %>% lm(formula = log(rate) ~ median_income + factor(neighborhood))

stat2 = m2 %>% augment()

# A little better?
stat2$.std.resid %>% hist()

# Somewhat better spread
ggplot() +
  geom_point(data = stat2, mapping = aes(x = .fitted, y = .std.resid)) +
  geom_hline(yintercept = 0, linetype = "dashed")







## 3.2 Measure Autocorrelation as a Variable #########################################

s = data %>%
  st_as_sf(crs = 4326)


# We could estimate, hey, what is the average rate of adjacent neighbors?
adjacency = s %>%
  # Join in the adjacent grid cells, and filter to just them
  st_join(y = s %>% select(adjrate = rate, geometry), 
          join = st_touches, left = FALSE) %>%
  # For each cell, look at the rates of its adjacent neighbors in 'adjrate'
  as_tibble() %>%
  group_by(cell) %>%
  summarize(
    # Get mean of adjacent values
    adj_mean = mean(log(adjrate), na.rm = TRUE)
    )


## 3.3 Control for Autocorrelation in Models ##############################

### Add Predictor ########################

# We could hypothetically try to control for our adjacenct values,
# by controlling for the mean of adjacent logged rates
m3 = data %>% 
  left_join(by = "cell", y = adjacency) %>%
  lm(formula = log(rate) ~ median_income + adj_mean + neighborhood)

# Our predictive power sure does increase!
m3 %>% broom::glance()

# How are our residuals?
m3$residuals %>% scale() %>% hist()
# Better.




### Differencing #############################

# We could try to control for adjacent values
# by SUBTRACTING the mean of adjacent logged values from our logged rates

m4 = data %>%
  left_join(by = "cell", y = adjacency) %>%
  lm(formula = log(rate) - adj_mean ~ median_income + neighborhood)


# Our predictive power should fall significantly,
# because we're now predicting the DIFFERENCE in log-rates, a different concept
m4 %>% broom::glance()

# How are our residuals?
m4$residuals %>% scale() %>% hist()
# A little funky, but in a new way!





### Correlation Structure ###########################

# We could use advanced regression techniques.
# We can add a spatial correlation 

library(sdep)
# The MASS::glmmPQL() function lets us do penalized 
# https://stats.oarc.ucla.edu/r/faq/how-do-i-model-a-spatially-autocorrelated-outcome/

# nlme helps with random effects models,
# and it can also support correlation structures.
# Linear Mixed Effects model
library(nlme)

# Add a dummy variable, that's just a constant
data$constant = 1


# You could do a standard model like so
data %>%
  lm(formula = log(rate) ~ median_income)



# You could do it as a mixed effect model with no mixed effects like so
nlme::lme(
  fixed = log(rate) ~ median_income, 
  data = data, 
  random = ~ 1 | constant
)



# Notice how the median_income beta coefficient remains the same?

# It's about to change, once we add a spatial correlation structure.

# We can specify a spatial correlation structure like this:

# If you expect a 'standard' spatial correlation, try corGaus() 

mm1 = nlme::lme(
  fixed = log(rate) ~ median_income, 
  data = data, 
  random = ~ 1 | constant, 
  correlation = corSpatial(form = ~ x + y, type = "gaussian")
)
mm1

# If you have reason to believe there's an exponential change in your value over distance
mm2 = nlme::lme(
  fixed = log(rate) ~ median_income, 
  data = data, 
  random = ~ 1 | constant, 
  correlation = corSpatial(form = ~ x + y, type = "exponential")
)

mm2

mm3 = nlme::lme(
  fixed = log(rate) ~ median_income, 
  data = data, 
  random = ~ 1 | constant, 
  correlation = corSpatial(form = ~ x + y, type = "linear")
)

mm3


# However, you can see that this method is kind of like taking a shot in the dark.
# You probably should do a likelihood ratio test to compare,
# which model specification improves your model fit,
# and simultaneously,
# you should have a good justification for that model specification.

# Not great for data communication.
# Hard to implement for iterative big data subsets.

### Likelihood ratio test ###############################

# Model Likelihood

# Does model 2 significantly improve on the model fit (loglikelihood)
# compared to model one?
# How different is the model loglikelihood from m2 compared to model 1?
# Chisq = evaluates how different is (0 to infinity)
# Pr(>Chisq) = p-value - statistical significance
# How likely is that difference due to chance?
lmtest::lrtest(mm1,mm2)


# Let's compare a bunch in a row
lmtest::lrtest(mm1,mm2,mm3)



# Let's compare a bunch in a row
lmtest::lrtest(mm1,mm3,mm2)


# Can we output this as a tibble?
lmtest::lrtest(mm1,mm3,mm2) %>%
  broom::tidy()


# Can we output this as a tibble?
lmtest::lrtest(mm1,mm3,mm2) %>%
  broom::tidy() %>%
  mutate(model = c("Gaussian", "Linear", "Exponential")) %>%
  select(model, loglik = LogLik, statistic, p.value)



rm(list = ls())

