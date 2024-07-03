#' 9P_dataviz.R
#' Practice: Lesson 9: Data Visualization 
#' Topic: Simulating Quantities of Interest for Visualization
#' 
#' aka
#' 
#' 1 Million Ways to Visualize Regression Results!

# Note: this is an OPTIONAL practice.
# You'll get a BUNCH of ideas on how to data wrangle and visualize your regression results.
# but it is **optional.**

# Visualization is central to communicating the results of social systems,
# but deciding how to best visualize the results of a statistical model 
# can be challenging. Below, I introduce over a dozen ways to visualize your regression results with simulation. 
# These visualizations all use statistical simulation, 
# a technique developed by Gary King and colleagues in 2000. 
# This technique is available in SPSS, Stata, and R, 
# and while functions may differ slightly,
# all visuals made below in R could also be generated using other programs. 
# Let’s briefly dig into what that is.

# Statistical simulation injects random variation into 
# the beta coefficients of a statistical model,
# producing 1000 slightly different sets of beta coefficients. 
# By supplying a set of hypothetical data, usually the traits of 
# an average observation in your dataset, 
# we can then vary just one of those traits, 
# feed them into our 1000 models with 1000 slightly different beta coefficients,
# and get 1000 slightly different projected outcomes. 
# This approximates estimation uncertainty - the uncertainty inherent in your model.

# We can even average those outcomes over random error again, 
# so that we account for fundamental uncertainty. 
# The 1000 projected outcomes we produce are called expected values, 
# and they represent 1000 very precise estimates 
# of the range of outcomes you would get given the conditions we supplied to the model.

# While a statistical model outputs just a beta coefficient and a standard error, 
# we have 1000 expected values to deal with!
# This creates lots of opportunities for interesting and useful visualizations 
# that will capture your reader’s eye and allow you to
# precisely estimate quantities of interest about your data.



# 1. Introduction ################################

## 1.1 Packages #############################
library(dplyr)
library(readr)
library(broom)
library(ggplot2)
library(viridis)

## 1.2 Data #################################

# For this practice workshop, we're going to use the jp_emissions.csv dataset
# which records emissions data in tons of CO2 per million residents
read_csv("data/jp_emissions.csv") %>% glimpse()

# Read more about the data here!
# https://doi.org/10.1016/j.eist.2020.09.006

# Let's gather some data on consumer emissions in the municipalities in Tokyo, Japan

# Finally, we’re going to format that data, 
# zooming into just cities and wards in the Tokyo area.
# We will create our outcome of interest, the rate of carbon emissions per 1,000 residents. 
# We’ll also create a few extra covariates (many are saved in this dataset), 
# including population density.

dat = read_csv("data/jp_emissions.csv") %>%
  # Finally, let's keep just a few of the many variables:
  select(
    pref, # name of prefecture (region)
    muni, # name of municipality
    muni_code, # each city's five-digit unique identifier code
    year, # year
    emissions_consumer_subtotal, # Consumer emissions total
    fin_str_index, # government capacity
    # and a bunch of covariates that might also shape emissions, including:
    pop, # population
    area_inhabitable, # inhabitable land area km-sq 
    income_per_capita, # income per capita 
    pop_age_65_plus, # % Age 65 or older
    pop_college, # % graduated college
    value_agr_mill, # agricultural output in millions of yen per capita 
    value_manuf_mill, # manufacturing output in millions of yen per capita 
    value_commerce_mill # commerical output in millions of yen per capita 
  ) %>%
# Next, we're going to overwrite that dataset,
# keeping just a subset of cities located in Tokyo.
  # Grab just the cities and wards in Tokyo
  filter(pref == "Tokyo") %>%
  # Calculate rate of consumer emissions per 1000 residents
  mutate(consumer_emissions = emissions_consumer_subtotal / pop * 1000) %>%
  # Calculate population density
  mutate(pop_density = pop / area_inhabitable) 

# Take a look at the data!
dat %>% glimpse()


## 1.3 Basic Model ######################################

# Let's get started.

# Finally, we are going to make a basic model. 
# Since emissions is a right-skewed rate, 
# we will use a log-linear model / log-OLS / aka gamma model


# I have this hypothesis that cities with greater governance capacity,
# as measured by this Financial Strength Index (fin_str_index) might see lower consumer emissions,
# because these city governments mobilize their citizens!
# Let's make a model to test it, controlling for alternative explanations of consumer emissions.

# A financial strength index reflects the financial strength of a local government. 
# It is calculated by dividing basic financial revenue 
# by basic financial demand for a city government.
# A financial strength index over means 1 shows that financial resources
# of the city are much greater than the taxes allocated ordinarily.

# Let’s estimate the expected emissions for cities with low, high, 
# and higher government capacity, 
# as measured by financial strength indices between 0 and 1.5.


m = dat %>%
  lm(formula = log(consumer_emissions) ~ fin_str_index +
       pop_density + income_per_capita +
       pop_age_65_plus + pop_college +
       value_agr_mill + value_manuf_mill + value_commerce_mill + factor(year) )

# What's our explanatory power? 83%. Nice!
broom::glance(m)



## 1.4 Theme #########################

# Next, set a theme that all our ggplots will use
mytheme <- theme_set(
  theme_classic(base_size = 14) +
    theme(panel.border = element_rect(fill = NA, color = "black"),
          plot.caption = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
)



# 2. Scenario A ############################


# Next, we are going to create several visualizations using this model! 
# These will generally highlight the powerful, 
# negative effect on consumer emissions per 1000 residents 
# made by strong governance capacity,
# represented by the financial strength index of a city.


## 2.0 Simulating Scenario A ############################

# Okay, let's make some scenarios...
# Suppose it's the latest year available (2017)
# Make me scenarios where:
# - it's the latest year available (2017)
# - all other variables are held at their medians (numeric) or modes (categories)
# - the financial strength index varies from 0 to 1.5, over 50 values.
scenarios = dat %>%
  filter(year == 2017) %>%
  reframe(
    # vary the financial strength index
    fin_str_index = seq(from = 0, to = 1.5, length.out = 50),
    # Keep the year - there's just one unique type now
    year = unique(year),
    # Get the median of your covariates
    income_per_capita = median(income_per_capita), 
    pop_density = median(pop_density), pop_age_65_plus = median(pop_age_65_plus), pop_college = median(pop_college),
    value_agr_mill = median(value_agr_mill), value_manuf_mill = median(value_manuf_mill), value_commerce_mill = median(value_commerce_mill)
  ) 

# Technically, classic Gary King-style statistical simulation 
# involves multiple kinds of simulation.
# We're going to skip over those for now and just do plain monte-carlo simulations.

# Let's get our simulations
sims = scenarios %>%
  # Get starter logged predictions and sigma
  mutate(yhat = predict(m, newdata = .),
         sigma = glance(m)$sigma)  %>%
  # For each value of fin_str_index, get simulations
  group_by(fin_str_index) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp())
# View a few
sims %>% head()



## 2.1 Ribbon Plot of Predicted Values #################################

# The most common move is to plot a ribbon of predicted values, 
# ranging between the 2.5th and 97.5th percentiles, 
# with the median indicated by a line (in this case, a dashed line).

# Let's make some statistics
stat = sims %>%
  group_by(fin_str_index) %>%
  summarize(median = median(ysim),
            lower = quantile(ysim, 0.025),
            upper = quantile(ysim, 0.975))
stat # view

ggplot() +
  geom_ribbon(
    data = stat,
    mapping = aes(x = fin_str_index, y = median,
                  ymin = lower, ymax = upper),
    alpha = 0.5, color = "#785EF0", fill = "#785EF0") +
  geom_line(linetype = "dashed") +
  labs(x = "Financial Strength Index\n(Government Capacity)",
       y = "Expected Consumer Emissions Rates\n(within 95% confidence interval)")


remove(stat)


## 2.2 Scatterplot of Predicted Values #################################

# Alternatively, we could plot every expected value within
# the 95% confidence interval
# (meaning, those greater than 2.5% and less than 97.5% of all values.) 
# It’s a little bumpy, but pretty.

stat = sims %>%
  group_by(fin_str_index) %>%
  filter(ysim > quantile(ysim, 0.025),
         ysim < quantile(ysim, 0.975)) %>%
  ungroup()

ggplot() +
  geom_jitter(
    data = stat,
    mapping = aes(x = fin_str_index, y = ysim, color = ysim),
    alpha = 0.25) + 
  viridis::scale_color_viridis(option = "plasma") +
  guides(color = guide_colorbar(barheight = 10, barwidth = 0.5)) +
  labs(x = "Financial Strength Index\n(Government Capacity)",
       y = "Expected Consumer Emissions Rates\n(within 95% confidence interval)",
       color = "Expected\nRates")

remove(stat)

## 2.3 Line Plot of Expected Values, by Percentile #####################

# Alternatively, we could plot each expected value as a line, colored by percentile!
# Then, we could show only those greater than 2.5% or less than 97.5% of all values.


stat = sims %>%
  group_by(fin_str_index) %>%
  # Sort and get percentile
  arrange(ysim) %>%
  mutate(percentile = 1:n() / 10) %>%
  # Filter within these quantiles
  filter(ysim > quantile(ysim, 0.025),
         ysim < quantile(ysim, 0.975)) %>%
  ungroup() 

ggplot() +
  geom_line(
    data = stat,
    mapping = aes(x = fin_str_index, y = ysim, group = percentile, color = percentile),
    alpha = 0.5) +
  scale_color_gradient2(low = "#DC267F", high = "#648FFF", mid = "#FFB000", midpoint = 50) +
  #viridis::scale_color_viridis(option = "plasma") +
  guides(color = guide_colorbar(barheight = 10, barwidth = 0.5)) +
  labs(x = "Financial Strength Index\n(Government Capacity)",
       y = "Expected Consumer Emissions Rates\n(within 95% confidence interval)",
       color = "Percentile")

remove(stat)

## 2.4 Ribbon Plot by Level of Confidence ######################################

# But maybe we like ribbons, 
# but want to show different levels of confidence very clearly. 
# Let’s make a series of bands, one for each interval.

# Get quantities of interest from the simulations
qi <- bind_rows(
  # 99.9 interval
  sims %>%
    group_by(fin_str_index) %>%
    summarize(median = median(ysim),
              lower = quantile(ysim, 0.0005),
              upper = quantile(ysim, 0.9995),
              ci = "99_9"),
  # 99 interval
  sims %>%
    group_by(fin_str_index) %>%
    summarize(median = median(ysim),
              lower = quantile(ysim, 0.005),
              upper = quantile(ysim, 0.995),
              ci = "99"),
  # 95th percentile
  sims %>%
    group_by(fin_str_index) %>%
    summarize(median = median(ysim),
              lower = quantile(ysim, 0.025),
              upper = quantile(ysim, 0.975),
              ci = "95"),
  # 90th percentile
  sims %>%
    group_by(fin_str_index) %>%
    summarize(median = median(ysim),
              lower = quantile(ysim, 0.05),
              upper = quantile(ysim, 0.95), 
              ci = "90"))
# Get statistics, formatted as needed
stat = qi %>%
  group_by(fin_str_index, median, ci) %>%
  # Pivot these longer into a tidy format
  reframe(type = c( rep("lower", n()), rep("upper", n()) ),
          pv = c( lower, upper ) ) %>%
  # Arrange in order
  arrange(desc(fin_str_index),pv) %>%
  # Now for each round of simulations,
  group_by(fin_str_index) %>%
  # Grab the median, and make upper and lower bands
  reframe(
    median = median[1],
    lower = c(pv[1], pv[2], pv[3], pv[4], pv[5], pv[6], pv[7]),
    upper = c(pv[2], pv[3], pv[4], pv[5], pv[6], pv[7], pv[8]),
    group = 1:7) %>%
  # And label them
  mutate(ci = group %>% recode_factor(
    "1" = "99.9%",
    "2" = "99%",
    "3" = "95%",
    "4" = "90%",
    "5" = "95%",
    "6" = "99%",
    "7" = "99.9%") %>% 
      # And order the intervals
      factor(levels = c("90%", "95%", "99%", "99.9%"))) 

# Finally, visualize the predicted values with confidence bands.
ggplot() +
  geom_ribbon(
    data = stat,
    mapping = aes(x = fin_str_index, ymin = lower, ymax = upper, 
                  group = group, fill = ci),
    color = "white") +
  geom_line(
    data = stat,
    mapping = aes(x = fin_str_index, y = median, group = ci),
    color = "white", linetype = "dashed") +
  viridis::scale_fill_viridis(discrete = TRUE, option = "plasma",
                              direction = 1, begin = 0.2, end = 0.8) +
  labs(x = "Financial Strength Index\n(Government Capacity)",
       y = "Expected Consumer Emissions Rates\n(within 95% confidence interval)",
       fill = "Confidence\nInterval")
# That looks snazzy!

remove(qi)


## 2.5 2-D Density Plane of Expected Values ############################


# Using raster
ggplot() +
  # Map the density plane here
  stat_density_2d(
    data = sims, mapping = aes(
      x = fin_str_index, y = ysim, 
      # after_stat() is a special ggplot command that 
      # lets you access common statistics calculated internally by ggplot 
      # read more here:
      # https://ggplot2.tidyverse.org/reference/geom_density.html#computed-variables
      fill = after_stat(density)), 
    geom = "raster", contour = FALSE)  +
  # And for kicks, overlay the confidence intervals we generated before.
  geom_ribbon(
    data = stat,
    mapping = aes(x = fin_str_index, y = median, 
                  ymin = lower, ymax = upper, group = group),
    color = "white", fill = NA, linewidth = 1.25) +
  viridis::scale_fill_viridis(option = "plasma") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  guides(fill = guide_colorbar(barwidth = 1, barheight = 10)) +
  labs(x = "Financial Strength Index\n(Government Capacity)",
       y = "Expected Consumer Emissions Rates",
       fill = "Density", caption = "Lines indicate 90, 95, 99, and 99.9% intervals")

# Let's clean up
remove(stat)



# 3. Scenario B #####################################

## 3.0 Simulating Scenario B ############################

# Let's make some new scenarios!

# Make me scenarios where:
# - it's the latest year available (2017)
# - all other variables are held at their medians (numeric) or modes (categories)
# - the financial strength index varies across the 20th to 80th percentiles in 2017
#        meaning, a range we know cities often span
# - the income per capita ALSO varies across the 20th to 80th percentiles in 2017

# This will be a little more complex.
# We will need the expand_grid() function from the tidyr package.
# As named, it expands grids!
tidyr::expand_grid(a = c(1,2), b = c(3,4))

# Make our new scenarios
scenarios = dat %>%
  filter(year == 2017) %>%
  reframe(
    # Make a grid of 25 x 25 values totalling 625 rows
    tidyr::expand_grid(
      # Get 25 values spanning fin_str_index's general range
      fin_str_index = seq(
        from = quantile(fin_str_index, probs = 0.20),
        to = quantile(fin_str_index, probs = 0.80), 
        length.out = 25),
      # Get 25 values spanning income's general range
      income_per_capita = seq(
        from = quantile(income_per_capita, probs = 0.20),
        to = quantile(income_per_capita, probs = 0.80), 
        length.out = 25)
    ),
    # After that, add these single constant variables    
    year = unique(year),
    pop_density = median(pop_density),
    pop_age_65_plus = median(pop_age_65_plus),
    pop_college = median(pop_college),
    value_agr_mill = median(value_agr_mill),
    value_manuf_mill = median(value_manuf_mill),
    value_commerce_mill = median(value_commerce_mill)
  )

# Let's get our simulations
sims = scenarios %>%
  # Get starter logged predictions and sigma
  mutate(yhat = predict(m, newdata = .),
         sigma = glance(m)$sigma)  %>%
  # For each value of fin_str_index, get simulations
  group_by(fin_str_index, income_per_capita) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp())

# View a few
sims %>% head()

## 3.1 HeatMap ################################

# Still, maybe we want to compare two variables. 
# We could check, for instance, the expected values as governance capacity 
# increases from its 20th percentile to its 80th percentile,
# compared against expected value as income per capita increases,
# eg. from its 20th percentile to its 80th percentile.

stat = sims %>%
  group_by(fin_str_index, income_per_capita) %>%
  summarize(median = quantile(ysim, probs=  0.5),
            lower = quantile(ysim, probs = 0.025),
            upper = quantile(ysim, probs = 0.975)) %>%
  ungroup() %>%
  mutate(group = 1:n())

# Visualize as a heatmap
ggplot() +
  # Add heatmap as tiles
  geom_tile(
    data = stat,
    mapping = aes(x = fin_str_index, y = income_per_capita, fill = median)
  ) + 
  # Visualize the contours overtop, ifhelpful
  geom_contour(
    data = stat, 
    mapping = aes(x = fin_str_index, y = income_per_capita, z = median),
    color = "white", linewidth = 1.25) +
  scale_fill_viridis(option = "plasma") +
  labs(fill = "Median\nExpected\nConsumer\nEmissions\nRates",
       x = "Financial Strength Index (20~80%)",
       y = "Income per Capita [thousands of yen] (20~80%)")

remove(stat)


# 4. Scenario C ##################################

## 4.0 Simulating Scenario C ############################

# Get 25 values spanning fin_str_index's general range
c1 = dat %>%
  filter(year == 2017) %>%
  reframe(
    group = 1,
    # We want 25 scenarios describing the lower to upper end of a variable of interest
    value = seq(from = 20, to = 80, length.out = 25),
    # Vary Financial Strength Index by its realistic range...
    fin_str_index = seq(
      from = quantile(fin_str_index, probs = 0.20),
      to = quantile(fin_str_index, probs = 0.80), 
      length.out = 25),
    # Hold everything else constant
    income_per_capita = median(income_per_capita),
    year = unique(year),
    pop_density = median(pop_density), pop_age_65_plus = median(pop_age_65_plus), pop_college = median(pop_college),
    value_agr_mill = median(value_agr_mill), value_manuf_mill = median(value_manuf_mill), value_commerce_mill = median(value_commerce_mill)
  )
c2 =  dat %>%
  filter(year == 2017) %>%
  reframe(
    group = 2,
    # We want 25 scenarios describing the lower to upper end of a variable of interest
    value = seq(from = 20, to = 80, length.out = 25),
    # Get 25 values spanning income's general range
    income_per_capita = seq(
      from = quantile(income_per_capita, probs = 0.20),
      to = quantile(income_per_capita, probs = 0.80), 
      length.out = 25),
    # Hold everything else constant
    fin_str_index = median(fin_str_index),
    year = unique(year),
    pop_density = median(pop_density), pop_age_65_plus = median(pop_age_65_plus), pop_college = median(pop_college),
    value_agr_mill = median(value_agr_mill), value_manuf_mill = median(value_manuf_mill), value_commerce_mill = median(value_commerce_mill)
  )
# Stack the scenarios...
scenarios = bind_rows(c1,c2) 


# Let's get our simulations
sims = scenarios %>%
  # Get starter logged predictions and sigma
  mutate(yhat = predict(m, newdata = .),
         sigma = glance(m)$sigma)  %>%
  # For each value of fin_str_index, get simulations
  group_by(group, value, fin_str_index, income_per_capita) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp())



## 4.1 Two Ribbons or More! #########################################


stat = sims %>%
  group_by(group, value, fin_str_index, income_per_capita) %>%
  summarize(median = quantile(ysim, probs=  0.5),
            lower = quantile(ysim, probs = 0.025),
            upper = quantile(ysim, probs = 0.975)) %>%
  ungroup()  %>%
  # adjust naming
  mutate(type = case_when(group == 1 ~ "Governance Capacity\n(Financial Strength Index)",
                          group == 2 ~ "Wealth\n(Income per capita)"))


ggplot() +
  geom_ribbon(
    data = stat,
    mapping = aes(x = value, ymin = lower, ymax = upper, group = type, fill = type),
    alpha = 0.25) +
  geom_line(
    data = stat,
    mapping = aes(x = value, y = median, ymin = lower, ymax = upper, 
                  group = type, fill = type, color = type),
    linewidth = 1.5) +
  scale_fill_manual(values = c("#DC267F", "#648FFF"))  +
  theme(legend.position = "bottom") +
  guides(color = "none") +
  labs(x = "Rank among Tokyo Municipalities (20 - 80%)",
       y = "Expected Consumer Emissions Rates\n(within 95% confidence intervals)",
       fill = "Predictor")


# 5. Scenario D ####################################

## 5.1 Simulating Scenario C ################################

# Setup new scenario, where financial strength varies in 4 breaks
scenarios = dat %>%
  filter(year == 2017) %>%
  reframe(
    # vary the financial strength index
    fin_str_index = c(0, 0.5, 1, 1.5),
    # Constants
    year = unique(year), income_per_capita = median(income_per_capita), 
    pop_density = median(pop_density), pop_age_65_plus = median(pop_age_65_plus), pop_college = median(pop_college),
    value_agr_mill = median(value_agr_mill), value_manuf_mill = median(value_manuf_mill), value_commerce_mill = median(value_commerce_mill)
  ) 

# Let's get our simulations
sims = scenarios %>%
  mutate(yhat = predict(m, newdata = .),
         sigma = glance(m)$sigma)  %>%
  group_by(fin_str_index) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp())

## 5.2 Dot and Whisker Plots #####################

# Alternatively, we might want to show expected emissions 
# given very specific quantities, not the whole range.
# In that case, we can use a dot and whisker plot. 
# This is commonly used to visualize beta coefficients,
# but using simulation, we can estimate the expected emissions
# given specific levels of financial strength
stat = sims %>%
  group_by(fin_str_index) %>%
  summarize(median = quantile(ysim, probs=  0.5),
            lower = quantile(ysim, probs = 0.025),
            upper = quantile(ysim, probs = 0.975))

ggplot() +
  geom_linerange(
    data = stat, 
    mapping = aes(x = factor(fin_str_index), 
                  ymin = lower, ymax = upper, 
                  color = factor(fin_str_index) ),
    linewidth = 2) +
  geom_point(
    data = stat,
    mapping = aes(x = factor(fin_str_index), y = median, color = factor(fin_str_index) ),
    size = 5) +
  geom_point(
    data = stat,
    mapping = aes(x = factor(fin_str_index), y = median),
    size = 3, color = "white"
  ) +
  coord_flip() +
  viridis::scale_color_viridis(discrete = TRUE, direction = -1, 
                               begin = 0.2, end = 0.8, option = "plasma") +
  labs(x = "Financial Strength Index\n(Government Capacity)",
       y = "Expected Consumer Emissions Rates") +
  guides(color = "none")

remove(stat)


## 5.2 Density Plots (Predictions) ###############################

ggplot() +
  geom_density(
    data = sims,
    mapping = aes(x = ysim, 
                  group = fin_str_index, fill = fin_str_index),
    alpha = 0.5, linewidth = 1.25
  ) +
  scale_fill_viridis(option = "plasma") +
  labs(x = "Financial Strength Index (0 to 1.5)",
       y = "Frequency of Predicted Value (Probability Density)",
       fill = "Predicted\nConsumer\nEmissions\nRates")

## 5.3 Density Plots (Effects) ###############################

# Get simulated first differences / effects
stat = sims %>%
  reframe(x0 = ysim[fin_str_index == 0],
          x5 = ysim[fin_str_index == 0.5],
          x10 = ysim[fin_str_index == 1.0],
          x15 = ysim[fin_str_index == 1.5])  %>%
  reframe(
    type = c( rep("0 to 0.5", n()), 
              rep("0 to 1.0", n()),
              rep("0 to 1.5", n())),
    diff = c( x5 - x0,
              x10 - x0,
              x15 - x0)
  ) %>%
  # Filter to 95% range for cleaner visual
  group_by(type) %>% 
  filter(diff < quantile(diff, probs = 0.975),
         diff > quantile(diff, probs = 0.025))

ggplot() +
  geom_density(
    data = stat, 
    mapping = aes(x = diff, 
                  group = type, fill = type),
    alpha = 0.75, linewidth = 1.25
  )  +
  scale_fill_viridis(discrete = TRUE, option = "plasma", begin = 0, end = 0.75)  +
  labs(x = "Change in Expected\nConsumer Emissions Rates (within 95% confidence interval)",
       y = "% of Simulations (n = 3000)",
       fill = "Change in\nFinancial\nStrength Index")


## 5.4 Dot and Whisker Plot (Effects) ####################
# Fun fact - density plots are just dot-and-whisker plots 
# with a distribution. So, instead, we could just as easily
# depict these as dot-and-whisker plots.


# Get simulated first differences / effects
stat = sims %>%
  reframe(x0 = ysim[fin_str_index == 0],
          x5 = ysim[fin_str_index == 0.5],
          x10 = ysim[fin_str_index == 1.0],
          x15 = ysim[fin_str_index == 1.5])  %>%
  reframe(
    type = c( rep("0 to 0.5", n()), rep("0 to 1.0", n()), rep("0 to 1.5", n())),
    diff = c( x5 - x0, x10 - x0, x15 - x0)
  ) %>%
  group_by(type) %>%
  summarize(median = median(diff),
            lower = quantile(diff, 0.025),
            upper = quantile(diff, 0.975))
  
ggplot(data = stat, mapping = aes(x = type, y = median, ymin = lower, ymax = upper, 
                       color = type)) +
  geom_linerange(size = 2) +
  geom_point(size = 5) +
  geom_point(size = 3, color = "white") +
  viridis::scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8, option = "plasma") +
  coord_flip() +
  guides(color = "none") +
  labs(x = "Change in Financial Strength Index",
       y = "Change in Expected\nConsumer Emissions Rates (within 95% confidence interval)")


## 5.5 Violin Plots ###########################################

# Alternatively, we could take the same results and 
# depict them as violins, which are double-sided density plots
# that show the distribution, while not overlapping with each other. 
# We could also display the actual expected values,
# jittered slightly for effect. Like always, it is probably 
# important to first zoom into just a specific 
# confidence interval worth of expected values
# before plotting distributions; this visual uses
# a 95% confidence interval. 
# One helpful thing about violin plots is that 
# you can usually draw a line at the median - 
# the 50th percentile - helping crystallize exactly what value it is.

stat = sims %>%
  reframe(x0 = ysim[fin_str_index == 0],
          x5 = ysim[fin_str_index == 0.5],
          x10 = ysim[fin_str_index == 1.0],
          x15 = ysim[fin_str_index == 1.5])  %>%
  reframe(
    type = c( rep("0 to 0.5", n()), rep("0 to 1.0", n()), rep("0 to 1.5", n())),
    diff = c( x5 - x0, x10 - x0, x15 - x0)
  ) %>%
  # Filter to 95% range for cleaner visual
  group_by(type) %>% 
  filter(diff < quantile(diff, probs = 0.975),
         diff > quantile(diff, probs = 0.025))

ggplot() +
  geom_jitter(data = stat, 
              mapping = aes(x = type, y = diff, color = type),
              alpha = 0.75) +
  geom_violin(data = stat, 
              mapping = aes(x = type, y = diff, color = type, group = type), 
              draw_quantiles = 0.5)  +
  labs(x = "Change in Financial Strength Index\n(Government Capacity)",
       y = "Change in Expected\nConsumer Emissions Rates") +
  guides(color = "none") +
  viridis::scale_color_viridis(discrete = TRUE, begin = 0.2, end = 0.8, option = "plasma")

## 5.6 Lollipop Charts ###############################################

# Alternatively, you might want something more recognizable.
# This is a lollipop chart, which shows how far away from 
# zero the median first difference is, 
# using a line connected to a lollipop,
# on which is written the median first different. 
# However, we still might want some estimate 
# of the confidence interval, 
# so we show the 95% confidence range by jittering 
# the first differences themselves around the lollipops.


stat = sims %>%
  reframe(x0 = ysim[fin_str_index == 0],
          x5 = ysim[fin_str_index == 0.5],
          x10 = ysim[fin_str_index == 1.0],
          x15 = ysim[fin_str_index == 1.5])  %>%
  reframe(
    type = c( rep("0 to 0.5", n()), rep("0 to 1.0", n()), rep("0 to 1.5", n())),
    diff = c( x5 - x0, x10 - x0, x15 - x0)
  )
  
# Get jittered points for background
jitters = stat %>%
  group_by(type) %>% 
  filter(diff < quantile(diff, probs = 0.975),
         diff > quantile(diff, probs = 0.025))
# Get main median effects for foreground
points = stat %>% 
  group_by(type) %>%
  summarize(median = median(diff))

ggplot() +
  geom_hline(yintercept = 0, linetype = "solid", alpha = 0.5,
             color = "#373737", linewidth = 1.5) +
  geom_jitter(
    data = jitters, mapping = aes(x = type, y = diff, color = type),
    alpha = 0.2) + 
  geom_linerange(
    data = points,
    mapping = aes(x = type, y = median, ymax = median, ymin = 0), 
    color = "darkgrey", linewidth = 3) +
  geom_point(data = points,
             mapping = aes(x = type, y = median,color = type),
             size = 35, alpha = 0.75) +
  geom_point(data = points,
             mapping = aes(x = type, y = median,color = type),
             size = 30, color = "white", alpha = 0.75) +
  geom_point(data = points,
             mapping = aes(x = type, y = median,color = type),
             size = 25) +
  geom_text(data = points,
            mapping = aes(x = type, y = median, 
                          label = round(median, 2), color = type),
            size = 5, color = "white") +
  guides(color = "none") +
  viridis::scale_color_viridis(discrete = TRUE, begin = 0.8, end = 0.2, option = "plasma") +
  labs(x = "Change in Financial Strength Index\n(Government Capacity)",
       y = "Change in Expected\nConsumer Emissions Rates") 



# Clean up
rm(list = ls())
