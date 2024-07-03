# 9C: Data Visualization III
# Topic: Extended Techniques for Temporal Data Visualization and Analysis
# Rates, Logging, Predictions, and More (Regression)
# Prof. Fraser

# So, we know how to query and visualize tables from our databases.
# But with big data, that's SO MUCH DATA!
# How do we summarize that information into single actionable numbers?


# For a good intro to regression, see this tutorial:
# https://moderndive.com/5-regression.html


# 0. Import Data ########################################

# Let's use some data on Japanese solar power to investigate.

# In this workshop, we’ll use the data.frame jp, 
# which is a matched sample of 43 monthly observations 
# on 147 Japanese municipalities,
# where some experienced disaster deaths or damage
# from the 2011 tsunami and earthquake (disaster == 1), 
# while others did not (disaster == 0). 
# This creates a total dataset of 6,321 city-month observations.

# Load packages
library(dplyr) # for data-wrangling
library(readr) # for reading in data
library(ggplot2) # for visualization
library(tidyr) # for drop-na
library(broom) # for regression model helpers

# Let's get a dataset that can help us evaluate path dependence.

# Save this solar dataset as an object in my environment
jp = read_csv("data/jp_solar.csv") %>%
  select(muni_code, date, year, solar_rate, disaster, pop) %>%
  # Remove outlier
  filter(date != "2017-09-25") %>%
  # Must order by city and time
  arrange(muni_code, date)


# In this dataset, each row is a city-month!

# View first 3 rows of dataset
jp %>% head(3)

# In this dataset, our variables mean:
#  
# - solar_rate: rate of new rooftop solar units 
#               adopted in that city that month, 
#               normalized per 1000 residents.
# - disaster: dichotomous variable indicating 
#             whether city experienced disaster deaths or damage 
#             from the 2011 tsunami and earthquake (disaster == 1),
#             or did not (disaster == 0).
# - pop: population from city’s most recent 5-year census tally.



# 1. Quantities of Interest ######################################

# In previous exercises, we learned to get beta coefficients (slopes)
# from linear models with lm()

# But Gary King et al. (2000) tells us that beta coefficients 
# often are not a great indicator.
# If our predictors are correlated, their standard errors vary widely.
# If our model error is correlated, their standard errors vary widely.
# Not good! 
# Good news: model predictions don't have these same problems.
# We're going to learn how to calculate ANY QUANTITY OF INTEREST WE WANT
# using model predictions instead.

# Let's do a primarily temporal analysis.
# We'll evaluate, how much does the rate of solar per 1000 residents vary,
# given whether your city was hit by the 2011 disaster or not (disaster),
# and given any date-specific fixed effects (factor(date))
m = jp %>%
  lm(formula = solar_rate ~ disaster + factor(date) )
# View it
m
# Lots of beta coefficients! Can we get a prediction instead?


# 2. Prediction ###########################

## 2.1 Predictions with Confidence Intervals ###############################

# Such models are powerful, but hard to interpret.
# Best strategy is NOT to interpret the beta coefficients, but the predictions.


# We need to make a 'frame' of newdata, then feed to our model for prediction.

# Simple example - What's the predicted solar rate in a city in the earliest date available (2014-05-25)?
# looking at those hit and not hit by disaster?

# We can do this two ways.

# We can manually make a tibble of the predictor data - keeping the same names and formats...
stat = tibble(
  date = "2014-05-25",
  disaster = c(0,1)
) %>%
  # And make a prediction, piping . (the prior table) into newdata
  mutate(yhat = predict(m, newdata = .))
# View it
stat

# Or we can make it automatically with data wrangling...
stat = jp %>%
  # Make a data.frame with as many rows as there are min(date) and unique(disaster)
  reframe(date = min(date), disaster = unique(disaster)) %>%
  # And make a prediction, piping . (the prior table) into newdata
  mutate(yhat = predict(m, newdata = .))
# View it 
stat



# Can we get error?
stat = stat %>%
  # Add sigma, the residual standard error, in units of the outcome - solar units per 1000 residents
  mutate(sigma = broom::glance(m)$sigma)


# Next, we're going to take Monte Carlo simulations 
# where for each prediction...
# we make a normal distribution with rnorm()
# where its mean is centered on the prediction 
# with a standard deviation of the model's standard error.
# we can take N random samples from that distribution
# approximating the range of how much that prediction would vary 
# if our sample was just a teeny bit different due to random chance.

# Let's start by taking just 3 random samples each. See what happened?
# Try running this code a bunch of times.
stat %>%
  group_by(date, disaster) %>%
  reframe(ysim = rnorm( n = 3, mean = yhat, sd = sigma))

# Let's try this with 1000 samples each instead.
sims = stat %>%
  group_by(date, disaster) %>%
  reframe(ysim = rnorm( n = 1000, mean = yhat, sd = sigma))
# View it
sims 

# In the old days, it was really quite hard to take random samples.
# It took time. 
# So, we developed lots of formulas to help us approximate the standard error,
# eg. standard errors of beta coefficients. 
# Now, many stats users only know how to use those formula.
# But since random samples are not computationally quite easy
# We can use them to calculate WAY MORE quantities of interest
# than there are existing formula for.

# For example: for each of our predictor conditions, we could calculate....
sims %>%
  group_by(date, disaster) %>%
  summarize(
    # the mean simulated solar rate 
    estimate = mean(ysim, probs = 0.50),
    # the median simulated solar rate
    median = quantile(ysim, probs = 0.50),
    # the lower 2.5th percentile - what we'd expect just 2.5 percent of the time
    lower = quantile(ysim, probs = 0.025),
    # the upper 97.5th percentile
    upper = quantile(ysim, probs = 0.975),
    # we could even estimate funky properties, like:
    # what's the variance of the sampling distribution?
    variance = var(ysim),
    # What's the standard deviation of the sampling distribution?
    sd = sd(ysim), 
    # sd should be about the standard error sigma from our model
    # heads up: in this special case ONLY - the sd of simulations IS the standard error
    # because it's the standard deviation of the approximated sampling distribution,
    # which is the literal definition of a standard error.
    
    # How many times greater than the mean is the standard deviation?
    cv = sd(ysim) / mean(ysim),
    
    # Most importantly, we could estimate MEANINGFUL measures
    # QUANTITIES OF INTEREST
    # that tell us something more than our prediction alone can.
    
    # What percent of the time should we expect a solar rate greater than 
    # an industry benchmark of 0.25 units per 1000 residents?
    percent_b1 = sum(ysim > 0.25) / n(),
    # What percentage of the time should we expect 
    # a solar rate of zero or less?
    percent_zero = sum(ysim <= 0) / n(),
    
    # Or, suppose we had a city with a population of 60000 residents.
    # We can directly calculate predictions and uncertainty 
    # while unit changing without worry
    mean_units = mean(ysim * 60000),
    se_units = sd(ysim * 60000) # see note about standard error above
  )


## 2.2 Effects ###############################


# Similarly, we can use simulation to calculate all sorts of interesting effects
# as quantities of interest too.

# 1. Marginal Effect
#    Difference in Prediction
#    Easy to do as a straight calculation, but hard to handle uncertainty.
#    Simulation will help.

# Here's a trick - you can reframe data to 
# put the ysim values when disaster == 0 in an x column (baseline)
# put the ysim values when disaster == 1 in an x1 column (treatment)
# then subtract them
# this will give you the full sampling distribution of marginal effects
# Let's make our object effects
effects = sims %>%
  group_by(date) %>%
  reframe(x = ysim[ disaster == 0],
          x1 = ysim[ disaster == 1],
          # You could get the difference (that's the big one)
          diff = x1 - x,
          # Percent change
          percent_change = (x1 - x) / x,
          # Standardized difference
          z = ((x1 - x) - mean(x1 - x)) / sd(x1 - x),
          # Simulated effect in number of solar panels for a city of 60,000
          panels = x1*60000 - x*60000
          # etc.
         )

# View it
effects

# For that date,
effects %>%
  group_by(date) %>%
  # Let's get the estimated marginal effect (mean simulated difference)
  summarize(
    estimate = mean(diff),
    # Plus the median for comparison
    median = quantile(diff, prob = 0.5),
    # The standard error of the marginal effect
    se = sd(diff),
    # Lower 95% confidence interval for marginal effect
    lower = quantile(diff, prob = 0.025),
    # Upper 95% confidence interval for marginal efffect
    upper = quantile(diff, prob = 0.975))

# THIS IS SUPER POWERFUL.


# 3. Constructing Scenarios ###########################

## 3.1 Simulating Many Scenarios ###########################

# Remember, we can use group_by() a lot.
# WHAT IF....
# WE DESIGNED A MORE COMPLEX SCENARIO FOR PREDICTION????
# AND THEN VISUALIZE IT!!!!!

# For every date available under study...
sims2 = jp %>%
  group_by(date) %>%
  reframe(disaster = c(0,1)) %>%
  # Let's make a prediction
  # And make a prediction, piping . (the prior table) into newdata
  mutate(yhat = predict(m, newdata = .)) %>%
  # Let's add the standard error of the model as a constant
  mutate(sigma = broom::glance(m)$sigma) %>%
  # For each group, let's simulate
  group_by(date, disaster) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma)) %>%
  # Calculate first difference at each time period
  group_by(date) %>%
  reframe(x = ysim[disaster == 0],
          x1 = ysim[disaster == 1],
          diff = x1 - x)

# approximate the sampling distribution with a median, lower 95% CI and upper 95% CI.
stats2 = sims2 %>%
  group_by(date) %>%
  summarize(
    estimate = median(diff, probs = 0.50),
    lower = quantile(diff, probs = 0.025),
    upper = quantile(diff, probs = 0.975))


# Visualize it out the wazoo!

# Blank plot
gg = ggplot()

# You can visualize the raw effects sampling distribution over time...
gg = gg +   
  geom_jitter(data = sims2, mapping = aes(x = date, y = diff, group = date), 
              alpha = 0.1, color = "purple") 
gg # view
  
# You can visualize the CIs over time as ribbons
gg = gg +
  geom_ribbon(data = stats2, mapping = aes(x = date, y = estimate, ymin = lower, ymax = upper),
              alpha = 0.5, fill = "purple")

gg # view

# You can visualize the median (often called expected) effect as a line
gg = gg +
  geom_line(data = stats2, mapping = aes(x = date, y = estimate), color = "black", linewidth = 1.5) 

gg # view


## 3.2 Captioning ##############################

# How the heck are we going to convey this to our reader reliably?

# Clarify labels to the reader
# Important when simulating to remind reader about what the change means 
# eg. --> [A - B] or [B - A]
gg = gg + labs(
  # This is a pretty bold title - we don't really know for sure it's from disaster,
  # but I know from field experience that there are potential impacts 
  title = "Difference in Solar Adoption from Disaster",
    x = "Month",
    y = "Median Expected Change in Solar Rate (95% Confidence Interval)\n[Disaster Hit Cities - Unaffected Cities]")

gg # view

# always a good idea to give yourself a caption here,
# to describe what actually is going on here.
# Here are 2 version of the caption, depending on your audience.

# Simple but clear
caption1 = paste0(
  "Points show predicted change in solar rates per month \n",  # remember to use linebreaks
  "for a disaster hit city compared to an unaffected city, \n",
  "using 1000 'simulations' of predicted change with model uncertainty.\n",
  "Lines show median and 95% confidence interval of monthly effects."
)

# Precise but boring 
caption2 = paste0(
  "Points show predicted change in solar rates per month \n", # remember to use linebreaks
  "for a disaster hit city compared to an unaffected city, using 1000 simulations.\n",
  "Each simulation approximates model error by adding randomly adding noise to predictions,\n",
  "using Monte Carlo samples from a normal distribution,\n",
  "scaled to the model residual standard error.\n",
  "Lines show median and 95% confidence interval of monthly effects."
)

# Add your caption here. 
gg + labs(caption = caption1) + 
  # Let's left justify (hjust = 0) the caption
  theme(plot.caption = element_text(hjust = 0))


# Unfortunately, our model's predictive power remains relatively low
# Check R2 ==> 100% is perfect, 0% is worst
broom::glance(m)$r.squared

# Can we do better? See below

# Let's clean up
rm(list = ls())

# 4. Making BETTER Models ################################


## 4.1 Outcome Variable Transformations #########################

# Some issues have to do not with our predictors,
# but with our outcome variable itself.
# We need to model this outcome variable in a way that appropriately handles
# the nature of this variable.
# This might improve our predictive power -
# and it definitely will improve the conceptual validity of our model.
# 
# So what it is about our variable that should concern us?

# read in our data
jp = read_csv("data/jp_solar.csv") %>%
  select(muni_code, date, year, solar_rate, disaster, pop) %>%
  # Remove outlier
  filter(date != "2017-09-25") %>%
  # Must order by city and time
  arrange(muni_code, date)

## 4.2. Rates as Outcome Variables ###################################

# We’re going to investigate the rate of rooftop solar adoption 
# per 1000 residents in each town over time (solar_rate).

jp$solar_rate %>% summary()
jp$solar_rate %>% hist()

# Whoa! That is a LOT of zeros. This should concern you.

# How many are we talking here?
jp %>%
  summarize(zeros = sum(solar_rate == 0),
            percent = zeros / n())

# Going to cause negative preditions
jp %>%
  lm(formula = solar_rate ~ disaster + factor(date)) %>%
  predict()  %>%
  summary()


# When analyzing a social system, you often work with rate data.
# This is because rates are counts, normalized
# rates are helpful because they've already accounted for a key confounder, like population
# eg. Total vaccinations per capita
# eg. Total solar farms per capita
# eg. Total kt of emissions per mile

# But if we want to model them, 
# we have to grapple with the fact that most rates are right skewed,
# going from zero to infinity. 
# They have more values on the left than the right.

# This is inconvenient for a linear model predicting your rate,
# which assumes that its error will be normally distributed.
# Yours won't; you'll predict most zeros as negative, which isn't possible.
# You'll get a bunch of highly correlated residuals,
# because all our cities with zero or few observed solar farms 
# will receive impossible negative predicted values, which makes no sense.

# So, we need to **transform our rates**,
# log-transforming it to make the data more normally distributed
jp$solar_rate %>% log() %>% hist()


# A lot of our municipalities recorded a rate of zero solar farms. 
# But it is not possible to log-transform a rate of zero solar farms. 
# Unfortunately, this will cut out your zeros.
# Some people exclude these zero-observations, 
# but this is a very dicey decision.
# Those zeros are deeply theoretically important. 
# If we think of our measurements as being subject 
# to even the slightest amount of error, which all measurements are, 
# then these are municipalities that probably 
# had very, very, very few solar farms per 1,000 residents, if not zero.
# If we remove all of these cases, 
# our model results will skew in favor of our cases which adopted more solar farms.
# We want to keep the zeros.


# You have a few options

# 1. Square Root Transformation of your Outcome. It can handle zeros.
#    not ideal, since most rates are better adjusted by logs, but it works.
#
# 2. Add a constant to your entire outcome. But this is kind of annoying, 
#    as you have to remember to subtract the constant in every simulation.
#
# 3. Replace the zeros in your outcome with a realistic-but-tiny value
#    A good rule of thumb is to take half as small as your min-non-zero value
#    Then, you can log transform as much as you like.
#    Plus, you haven't changed the rest of your values, so no back-adjustment later.

jp %>%
  filter(solar_rate > 0) %>%
  summarize(min = min(solar_rate))

# Okay. So, it looks like 0.005 or 0.001 would be in the right ballpark.

# A little easier to work with now.
log(jp$solar_rate + 0.005) %>% summary()
# Let's try a few adjustments till we get the right one
# that produces non-negative back-transformed predictions.

## 4.3 Modeling with Logs and Adjustments #####################

# Let's do method 3 from above, where we make a permanent adjustment to the zeros,
# then log transform.

# So, we can apply an adjustment, using dplyr's case_when() variable.
jp2 = jp %>%
  # update solar_rate so that...
  mutate(solar_rate = case_when(
    # whenever solar_rate is 0, make it 0.001
    solar_rate == 0 ~ 0.001,
    # whenever anything else is the case, just leave the solar_rate as is
    TRUE ~ solar_rate))

# Let's create m2, our model that incorporates a logged solar rate
m2 = jp2 %>% lm(formula = log(solar_rate) ~ disaster + factor(date))
# Let's look at the prediction range
m2 %>% predict()  %>% range()

# remember, those numbers are LOGGED
# so let's exponentiate them back
m2 %>% predict() %>% exp() %>% summary()
# All non-negative!!!!!!

## 4.4 Back-Transforming Simulations #####################

# One of the big value addeds of simulation
# is that is makes back-transforming easier.

# back transforming means:
# I have a logged_prediction in log units. I have a log_stderror in log units.
# I want a prediction in normal units. I can get that with exp(logged_prediction).
# I want a stderror in normal units. I CANNOT GET THAT with exp(log_stderror)
# DOES NOT WORK, because of non-linearity.

# BUT! You can use simulation to get those error terms in their original units.
# Let's try it out using our model of log(solar_rate)

# Let's make a single prediction, using this scenario of inputs...
stat3 = tibble(date = "2014-05-25", disaster = c(0,1)) %>%
  mutate(yhat = predict(m2, newdata = .), # get prediction 
         sigma = broom::glance(m2)$sigma) # get standard error
stat3 # view it

# Okay, so yhat is in units of "log-rooftop-solar-units-per-1000-residents", and sigma is too.

# So how about this?
# Let's simulate the predictions in log form...
# then EXPONENTIATE ALL THE SIMULATIONS with exp()
sims = stat3 %>%
  group_by(date,disaster) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp())

# Now, we've got an approximated sampling distribution 
# in original units of 'rooftop solar units per 1000 residents'.
sims

# We can now calculate quantities of interest like we normally would
sims %>%
  group_by(date,disaster) %>%
  summarize(
    estimate = mean(ysim, na.rm = TRUE),
    # INCLUDING uncertainty
    # so we can get the standard error in original units 
    # which approximates the standard deviation of the sampling distribution
    se = sd(ysim, na.rm = TRUE),
    # and any other quantities of interest we care about
    median = quantile(ysim, probs=  0.50),
    lower = quantile(ysim, probs = 0.025),
    upper = quantile(ysim, probs = 0.975))

# Rad!

rm(list = ls())

# 5. Simulating MANY Models ########################

## 5.1 Predictions at Scale ###############################

# When working with databases,
# it can be very computationally inefficient to produce one model.
# Your overall model may have very crappy explanatory power,
# even if it has tons of observations, simply because there is so much variation.
# It might be better to use a bunch of smaller models instead.

# For example,
# we could make 1 model of disaster effects per month, using all of the cities as observations.

# From each model, we could extract beta coefficients,
# giving us the estimated effect of the disaster for that month.

# That's neat - but it only works when you DON'T transform your outcome.
# If your outcome is transformed in the model,
# the beta coefficients end up in the units of the transformation,
# Making them pretty much unintelligible.

# Instead, the useful quantity of interest here would be predictions.

# So how do we make simulations for MANY MODELS at scale???

# ITERATE ITERATE ITERATE

# This is the power of data science a la the dplyr package.


library(dplyr)
library(readr)
library(broom)
library(ggplot2)

jp = read_csv("data/jp_solar.csv") %>%
  select(muni_code, date, year, solar_rate, disaster, pop) %>%
  filter(date != "2017-09-25") %>%   # Remove outlier
  arrange(muni_code, date) %>% # Arrange for visual clarity
  # Fix solar rate
  mutate(solar_rate = case_when(solar_rate == 0 ~ 0.001, TRUE ~ solar_rate)) 


# Remember that we can use the broom package's
# tidy() function to get a model's coefficient data.frame
jp %>% lm(formula = log(solar_rate) ~ disaster) %>% broom::tidy()
# and
# glance() function to get a model's summary statistics, like r.squared and sigma
jp %>% lm(formula = log(solar_rate) ~ disaster) %>% broom::glance()

# We have 42 distinct dates
jp %>% select(date) %>% distinct() %>% summarize(count = n())

# Each date has exactly 147 city observations
jp %>%
  group_by(date) %>%
  summarize(count = n()) %>%
  summarize(min = min(count),
            max = max(count))

# So we know we can use group_by() and reframe() 
# to return model data.frames for each date.

# ex1. Get every coefficient table 
jp %>% group_by(date) %>%
  reframe( lm(formula = log(solar_rate) ~ disaster) %>% broom::tidy())

# ex2. Get every summary statistic table
jp %>%
  group_by(date) %>%
  reframe(lm(formula = log(solar_rate) ~ disaster) %>% broom::glance())

# How about predictions?
# We can use reframe({...}) with curly braces
# to do a multi-step process, per group, ending with the resulting data.frame
jp %>%
  group_by(date) %>%
  reframe({ 
    # Make a temporary model object
    model = lm(formula = log(solar_rate) ~ disaster) 
    # Make a temporary 1 row data.frame of summary statistics
    stats = broom::glance(model)
    # Make a tibble of conditions
    scenario = tibble(disaster = c(0,1)) %>%
      # Add the prediction for each
      mutate(yhat = predict(model, newdata = .)) %>%
      # Add the standard error for each
      mutate(sigma = stats$sigma)
    # Return the result
    scenario
    })

# Technically, you could do that in separate steps without using reframe({...})
# but it means repeatedly running your models, and that actually can take time
# with big data, so let's try to conserve the number of times you do that.
# These { } symbols are essentially cooking up an 'expression' - 
# a short-term function that gets acted on for each group.

# The strength is how easy it was. No for-loops. All dplyr compatible.
# The weakness is that you can only know 
# as much about each model as you output at the end.

## 5.2 Simulations at Scale ###########################

# Let's try a similar philosophy, but now we'll do simulations.

# We start by getting predictions and standard errors for each scenario, like above
predictions = jp %>%
  group_by(date) %>%
  reframe({ 
    # Make a temporary model object and model statistics
    model = lm(formula = log(solar_rate) ~ disaster) 
    stats = broom::glance(model)
    # Get predictions and standard error for each scenario
    scenario = tibble(disaster = c(0,1)) %>%
      mutate(yhat = predict(model, newdata = .)) %>%
      mutate(sigma = stats$sigma)
    # Return the result
    scenario
  })

# Then, we can continue the process!

# Get simulations
sims = predictions %>%
  # For each date-disaster scenario...
  group_by(date, disaster) %>%
  # get 1000 simulations and back-transform them (exponentiate)
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp()) 

# Notice how big this dataframe is. HUGE.
# It will not really be tenable to maintain nearly 100,000 observations in memory.
# Like, it'll still work, but it might slow your dashboard down.
# So, we probably want to be calculating quantities of interest 
# (eg. confidence intervals) rather than visualizing the entirety.
sims %>% summarize(count = n())

# Now let's calculate some quantities of interest.

# We might want the simulated 'predicted values',
# also know as 'pv's.
pv = sims %>%
  group_by(date, disaster) %>%
  summarize(estimate = mean(ysim),
            se = sd(ysim),
            lower = quantile(ysim, probs = 0.025),
            upper = quantile(ysim, probs = 0.975))
pv # view it
# This will let us make ribbon plots, crossbars, whatever.

# We might want effects too, sometimes called 'marginal effects' or 'first differences'
# We can get effects!
effects = sims %>%
  # For each group, get the differences
  group_by(date) %>%
  reframe(x = ysim[disaster == 0],
          x1 = ysim[disaster == 1],
          diff = x1 - x) %>%
  # For each group, get quantities of interest ABOUT the differences
  group_by(date) %>%
  summarize(estimate = mean(diff),
            se = sd(diff),
            lower = quantile(diff, probs = 0.025),
            upper = quantile(diff, probs = 0.975))
  
effects # view it


## 5.3 Simplifying Simulations at Scale #######################################

# Finally, if we were clear on from the beginning
# what kind of quantities of interest we wanted to produce, 
# we could generate those from the get go, all in one go.

# You could do it several ways:
# - reframe({ scenario }) then get qis per group --> takes about 0.5 seconds
# - reframe({ qi }) - do all the simulation inside reframe. --> takes about 1.5 seconds

# I recommend doing it like this.
qi = jp %>%
  group_by(date) %>%
  reframe({ 
    # Make a temporary model object and model statistics
    model = lm(formula = log(solar_rate) ~ disaster) 
    stats = broom::glance(model)
    # Get predictions and standard error for each scenario
    scenario = tibble(disaster = c(0,1)) %>%
      mutate(yhat = predict(model, newdata = .)) %>%
      mutate(sigma = stats$sigma)
  }) %>%
  # Let's get quantities of interest about
  # **percent change in rooftop solar adoption rates**
  #   for a disaster hit town vs. an unaffected town
  # Simulate and backtransform
  group_by(date, disaster) %>%
  reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp()) %>%
  # For each specific group, get the percent change
  group_by(date) %>%
  reframe(x = ysim[disaster == 0], x1 = ysim[disaster == 1],
          percent_change = (x1 - x)/x) %>%
  # For each specific group, get quantities of interest ABOUT the per
  group_by(date) %>%
  summarize(estimate = median(percent_change),
            se = sd(percent_change),
            lower = quantile(percent_change, probs = 0.025),
            upper = quantile(percent_change, probs = 0.975))


# This is what it would look like to do it ALL inside reframe({...})
# Why would I ever do this?
# If I wanted to quickly ALSO get back model statistics.
qi2 = jp %>%
  group_by(date) %>%
  reframe({
    model = lm(formula = log(solar_rate) ~ disaster)
    stats = broom::glance(model)
    scenario = tibble(disaster = c(0,1)) %>%
      mutate(yhat = predict(model, newdata = .)) %>%
      mutate(sigma = stats$sigma)
    qis = scenario %>%
      group_by(disaster) %>%
      reframe(ysim = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp()) %>%
      reframe(x = ysim[disaster == 0], x1 = ysim[disaster == 1],
              percent_change = (x1 - x)/x) %>%
      summarize(estimate = median(percent_change),
                se = sd(percent_change),
                lower = quantile(percent_change, probs = 0.025),
                upper = quantile(percent_change, probs = 0.975))
    # Add in the model statistics for that scenario
    result = qis %>%
      mutate(rsq = stats$r.squared,
             nobs = stats$nobs)

  })

# For instance, it looks like my rsq ranges from...
qi2$rsq %>% summary()


## 5.4 Benchmarking ################################

# If you ever need to test how long a process takes to compute,
# you can use system.time({...})
# For example:
system.time({ rnorm(n = 1000000)   })
# Look at the elapsed column in the result. For me, about 0.03 seconds.

## 5.5 Visualizing MANY Models' Simulations #########################

# Finally, here would be a visual of our many distinct monthly models'
# and the quantities of interest from their simulated percent change

# Let the ggplot fun begin!
gg = ggplot() +
  geom_ribbon(
    data = qi,
    mapping = aes(x = date, ymin = lower, ymax = upper), alpha = 0.5) +
  geom_line(
    data = qi,
    mapping = aes(x = date, y = estimate)
  )
# This looks pretty terrible. 
# That's because we're working with 
# an outcome that tends to be right skewed (solar_rates) 
# and because this y-axis is a percent change - could be positive or negative.
# We need some way to visualize this change,
# allowing us to see the negatives on a logarithmic scale
# same as the positives on a logarithmic scale...

# We can use the scales package to do this,
# using the pseudo_log_trans() function.
library(scales)

# Add bi-directional log-scale
gg = gg +  scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 1 )) 
  # You can adjust sigma for visual quality. Doesn't change data.
gg # view

# Or do it with better breaks
gg = gg +
  scale_y_continuous(
    # Set extra breaks if you want them.
    # When working with natural log, I like the 1-3 rule.
    # Eg. 1, 3, 10, 30, 100, 300, etc.
    breaks = c(-100, -30, -10, -3, -1, 0, 1, 3, 10, 30, 100, 300, 1000, 3000, 10000, 30000),
    # Adjust sigma for visual quality.
    trans = scales::pseudo_log_trans(sigma = 1 ))
gg # view

# Add labels
gg = gg  +
  labs(y = "Median Expected % Change in Solar Adoption Rate\n[Disaster Hit vs. Unaffected]",
       x = "Month",
       title = "% Change in Rooftop Solar over Time")
gg # view

## 5.6 Describing Multi-Model Simulation ###########################################

# Here's my best shot at describing multi-model simulation.
# Use the questions below as guidelines for describing your model.

bigcaption = paste0(
    # What does the chart show, in 1 sentence? Lines? Bands? Points? Metrics?
    "Chart shows median (lines) and 95% confidence intervals (bands) of \n",
    "expected % change in solar adoption rate per month, based on 1000 simulations per monthly scenario.\n",
    # What does the metric mean?
    "% change calculated as added solar rate in disaster hit cities, compared to baseline of unaffected cities. \n",
    # How many models? How many observations?
    "Each month modeled separately, totalling 42 monthly models of 147 city observations each. \n",
    # What was the model?
    "Each monthly model evaluated the bivariate effect of the disaster (1/0) on the logged solar rate.\n",
    # How good were the models?
    "Each monthly model had an average explanatory power (R2) of 19%, ranging from 7% to 50%.\n",
    # What does a simulation mean?
    "Each simulation approximates model error by adding random noise \n",
    "to predictions for each monthly scenario.\n",
    # What does random noise mean?
    "Random noise drawn from normal distributions, scaled to the model residual standard error."
  )






# Cleanup!

rm(list = ls())



