# class_5_analytics_1.R
#
# Series: Analytics 1
# Topic: Techniques for Temporal Data
# Lagged Variables, Means, and Lines-of-Best-Fit (Regression)
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
library(tidyr) # for drop-na
library(readr) # for reading in data
library(ggplot2) # for visualization
library(broom) # for regression model helpers

# Save this solar dataset as an object in my environment
jp = read_csv("data/jp_solar.csv") %>%
  select(muni_code, date, year, solar_rate, disaster, pop) %>%
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


# 1. Rates as Outcome Variables ###################################

# We’re going to investigate the rate of rooftop solar adoption 
# per 1000 residents in each town over time (solar_rate).

jp$solar_rate %>% summary()
jp$solar_rate %>% hist()

# Whoa! That is a LOT of zeros. This should concern you.

# How many are we talking here?
jp %>%
  summarize(zeros = sum(solar_rate == 0),
            percent = zeros / n())


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

# A good rule of thumb is to add a constant, 
# about half as small as your min non-zero value,
# and then log-transform

jp %>%
  filter(solar_rate > 0) %>%
  summarize(min = min(solar_rate))

# Okay. So, it looks like 0.005 or 0.001 would be in the right ballpark.

# A little easier to work with now.
log(jp$solar_rate + 0.005) %>% summary()




# 2. Lagging a Variable #######################################

# Below, we’re going to learn to use the lag() function.

# Let’s make some lagged variables to account for 
# the effect of past solar deployment on future solar deployment. 
# Cities that have been ‘on a roll’ in recent months and years 
# might see a boost in future deployment, 
# because their local businesses and partners are seeing investment, 
# gaining name recognition, and
# getting practice at pulling off these projects. 
# Let’s test that hypothesis!

# First, we need to create a lagged variable.
# We will order the dataset with arrange(), then lag the variable, 
# one city at a time using group_by(), 
# using lag() to push back the variable for that city, leaving an NA behind.
# Important to check that it has worked as expected.


# Let's make another 'jp' data.frame, with some adjustments
jp2 = jp %>%
  # Must order by city and time
  arrange(muni_code, date) %>%
  # Must group by municipality - IMPORTANT
  group_by(muni_code) %>%
  # Then lag the solar rate, using n to specify how many steps.
  mutate(
    # Get the logged solar rate
    solar_log = log(solar_rate + 0.005),
    # First let's lag by 1 row,
    solar_lag1 = lag(solar_log, n = 1),
    # Then let's lag by 2 rows
    solar_lag2 = lag(solar_log, n = 2),
    # Then let's lag by 3 rows
    solar_lag3 = lag(solar_log, n = 3)) %>%
  ungroup() 

# Let's check the result!
jp2 %>%
  select(muni_code, date, solar_rate, solar_log, contains("lag") ) %>%
  head(5)

# 3. lm() #############################################

## 3.1 Modeling ###################

# Finally, let’s model with them!
  
# This dataset was original designed as a matching experiment 
# to test the effect of disasters on rooftop solar adoption. 
# So, we’ll run our first model to 
# test the direct effect of disasters on solar adoption. 

##  Making a simple line-of-best fit model
# The lm() function fits the line of best fit, in the order (y ~ x)
m1 = jp2 %>% lm(formula = solar_log ~ disaster)
m1
# Let's interpret this.
# predicted solar_log + 0.005 =  -3.666 + 1.562 * disaster

# so, when disaster = 0, the predicted log-solar rate + 0.005 is -3.666
# when disaster = 1, the predicted log-solar rate + 0.005 is -3.666 + 1.562

# Let's try a few more.

## Controlling for time

# This data occurred over time, so you should really be adjusting for 
# variation over time.
m2 = jp2 %>% lm(formula = solar_log ~ disaster + factor(date)  ) 

## Effect of Prior Solar, 1 month prior
m3 = jp2 %>% lm(formula = solar_log ~ disaster + factor(date) + solar_lag1)

m3

## Effect of Prior Solar, 2 months prior
m4 = jp2 %>% lm(formula = solar_log ~ disaster + factor(date) + 
                  solar_lag1 + solar_lag2)
m4

## Effect of Prior Solar, 3 months prior
m5 = jp2 %>% lm(formula = solar_log ~ disaster + factor(date) + 
                  solar_lag1 + solar_lag2 + solar_lag3)
m5

## 3.2 Tables #####################################

# We could report models quickly with texreg, omitting the coefficients for date, since there are so many.
# But all these beta coefficients are in wonky log units.
texreg::screenreg(list(m1,m3,m4,m5), omit.coef = "date")



# 4. Prediction ###########################

## 4.1 Predictions with Confidence Intervals ###############################

# Such models are powerful, but hard to interpret.
# Best strategy is NOT to interpret the beta coefficients, but the predictions.


# We need to make a 'frame' of newdata, then feed to our model for prediction.

# Simple example - What's the predicted log-solar rate in a city in 2014-05-25,
# looking at those hit and not hit by disaster?
stat = jp2 %>%
  summarize(date = "2014-05-25", disaster = c(0, 1) ) %>%
  mutate(yhat = predict(m2, newdata = .))

# Can we get error?
stat = stat %>%
  # Add sigma, the residual standard error, in log-solar units
  mutate(sigma = broom::glance(m2)$sigma)

# Remember to backtransform your rate prediction into normal units, 
# adjusting for your constant you added
stat = stat %>%
  mutate(estimate = yhat %>% exp() - 0.005)
  
# To get a backtransformed-standard error, you can simulate 1000 random draws from that prediction with an error range of sigma,
# back-transform the simulations,
# and then take the standard deviation.
stat = stat %>%
  group_by(date, disaster) %>%
  mutate(se = (rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp() - 0.005) %>% sd()) %>%
  ungroup()

# How about confidence intervals?
stat = stat %>%
  group_by(date, disaster) %>%
  # Get the lower 2.5th percentile...
  mutate(lower = (rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp() - 0.005) %>% quantile(probs = 0.025)) %>%
  # Get the upper 97.5th percentile...
  mutate(upper = (rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp() - 0.005) %>% quantile(probs = 0.975)) %>%
  ungroup()

# Normally, you'd want to do just 1 round of 1000 simulations, and take summary statistics from it,
# but this way is a little cleaner for teaching. Works as well.


## 4.2 Predictions from Many Models ###########################################

# But what if your database is way too big to run one model on?
# Alternatively, you could make lots of little models.

# Then let's estimate the effect of the disaster on solar OVER TIME!

stat2 = jp2 %>%
  # for each date
  group_by(date) %>%
  # Get one model, and use broom's tidy function to extract the coefficient table as a data.frame
  reframe(
    # Use curly braces to do a multi-step process ending in a data.frame
    {
      # Get model
      model = lm(formula = solar_log ~ disaster, data = .)
      # Get sigma
      sigma = broom::glance(model)$sigma
      # Get predictions
      result = tibble(
        disaster = c(0,1),
        yhat = predict(model, newdata = tibble(disaster)),
        sigma = sigma
      )
      # Return the result
      result
    }
  )

## 4.3 Predictions with Confidence Intervals over Time  #####################

# Get estimates
stat3 = stat2 %>%
  # For each date...
  group_by(date, disaster) %>%
  reframe(
    {
      # Get simulations
     sims = rnorm(n = 1000, mean = yhat, sd = sigma) %>% exp() - 0.005
     # Make a data.frame summarizing those simulations
     tibble(
       estimate = yhat %>% exp() - 0.005,
       se = sd(sims),
       lower = quantile(sims, probs = 0.025),
       upper = quantile(sims, probs = 0.975)
     )  
    }
  )

# Let the ggplot fun begin!
ggplot() +
  geom_ribbon(
    data = stat3,
    mapping = aes(x = date, ymin = lower, ymax = upper, group = disaster, fill = disaster), alpha = 0.5) +
  geom_line(
    data = stat3,
    mapping = aes(x = date, y = estimate, group = disaster, color = disaster)
  )
  
  
  
  
  
  # Cleanup!

rm(list = ls())
  
  

