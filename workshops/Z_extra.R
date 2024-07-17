#' @name Z_extra.R
#' @title Extra Trainings
#' @description
#' This script contains extra, **optional** trainings for those interested.


# Z. EXTRA: Simulation #######################

# The quick way to handle autocorrelation is to use a method
# to generate quantities of interest that is generally robust to it,
# eg. it does not rely on beta coefficients.

# In this case, we'd want to use full statistical simulation a la Gary King,
# where we account for both 
# - estimation uncertainty (uncertainty in beta coefficients)
# AND
# - fundamental uncertainty (uncertainty in predictions)

# Get a data.frame of predictor values, as scenarios for simulation
scenarios = data %>%
  reframe(
    `(Intercept)` = c(1,1),
    median_income = c(
      quantile(median_income, na.rm = TRUE, probs = 0.25),
      quantile(median_income, na.rm = TRUE, probs = 0.75)
    )
  ) %>%
  mutate(scenario = 1:n()) %>%
  select(scenario, `(Intercept)`, median_income)

scenarios

# Get 1000 estimates of slightly different alpha and beta coefficients
uncertainty_est = MASS::mvrnorm(n = 1000, mu = m1$coefficients, Sigma = vcov(m1)) %>%
  as_tibble() %>%
  mutate(id = 1:n()) 


# Pivot column names into rows, 
# using pivot_longer() from the tidyr package
scenarios_long = scenarios %>% 
  tidyr::pivot_longer(cols = -c(scenario), names_to = "var", values_to = "x")

# Pivot uncertainty estimates into long format, 
# using pivot_longer() from the tidyr package
uncertainty_est_long = uncertainty_est %>%
  tidyr::pivot_longer(cols = -c(id), names_to = "var", values_to = "beta") 

# For each scenario value, join in the uncertainty estimates corresponding to it
frame = left_join(
  by = "var", 
  x = scenarios_long,
  y = uncertainty_est_long,
  multiple = "all")

frame %>% head()
frame %>% tail()

# For each scenario and simulation id
predictions = frame %>%
  group_by(scenario, id) %>%
  summarize(yhat = sum(x * beta)) %>%
  # and add in the residual standard error
  mutate(sigma = glance(m1)$sigma)

# Now for each prediction,
# simulate uncertainty in the predictions
sims = predictions %>%
  group_by(scenario, id) %>%
  reframe(
    # Take 1 random draw...
    ysim_log = rnorm(n = 1, mean = yhat, sd = sigma),
    # And back transform the simulation
    ysim = ysim_log %>% exp())

# Your simulations have now accounted for uncertainty in both 
sims %>%
  group_by(scenario) %>%
  summarize(estimate = quantile(ysim, probs = 0.5),
            lower = quantile(ysim, probs = 0.025),
            upper = quantile(ysim, probs = 0.975))


sims %>%
  group_by(id) %>%
  reframe(
    x0 = ysim[scenario == 1],
    x1 = ysim[scenario == 2],
    diff = x1 - x0) %>%
  summarize(estimate = quantile(diff, probs = 0.5),
            lower = quantile(diff, probs = 0.025),
            upper = quantile(diff, probs = 0.975))

# This reveals that, yeah, the simulated difference 
# looks a LOT different that the beta coefficient
# Probably not significant as is.
m1 %>% broom::tidy()