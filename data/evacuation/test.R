

library(dplyr)

# Make some data
data = tibble(
  zone = c("A", "A", "B", "C", "B", "B", "B"),
  price = c(100, 300, 200, 30, 200, 200, 20)
)

# How many parcels
data %>%
  summarize(count = n())

data %>%
  summarize(min_price = min(price))

# Min and Max parcels
data %>%
  reframe(value = c( min(price), max(price) ) )

# For each zone, what's the min price
data %>%
  group_by(zone) %>%
  summarize(min_price = min(price))


# Try this out but swap some values
# eg. what if I use the max instead
# what if I use the mean, sd, 
# etc.

# Is there a relationship between zoning and price
data %>%
  lm(formula = price ~ zone)


# Or, is the mean of zone A higher than the others?
data %>%
  group_by(zone) %>%
  summarize(mean_price = mean(price))




