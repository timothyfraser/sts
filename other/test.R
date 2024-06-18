#' @name test
#' @title test.R
#' @author Tim Fraser
#' @description Testing for Social Technical Systems Class


library(dplyr)
library(readr)
library(ggplot2)

d = tribble(
  ~x, ~y, ~c,
  2,  3,  17,
  -1, 1,  0
)

m = as.matrix(d)

s = solve(a = m[,-3], b = m[,3])

d %>%
  mutate(test = x * s[1] + y * s[2])

# 17 = 2*x + 3*y
# If we solve for y...
# y = (17 - 2*x)/3

# -1*x + 1*y = 0
# y = x
y1 = function(x){  (17 - 2*x)/3  }
y2 = function(x){ return(x) }

test = tibble(
  x = seq(from = 0, to =10, by = 0.1),
  ya = y1(x),
  yb = y2(x)
)

ggplot() +
  geom_line(data = test, mapping = aes(x = x, y = ya)) +
  geom_line(data = test, mapping = aes(x = x, y = yb)) +
  geom_point(data = tibble(x = s[1], y = s[2]), mapping = aes(x = x, y = y), size = 3)


# Multivariate Normal Simulations (Best) ###########################################


# Original formula
# y = 2 * x + 3
# y = -5.3 * x + 4

# Suppose to start, you have a system of 2 equations, where...
# --- variable x has a standard error. 
# --- constant c **could** a standard error too if you choose.
# --- variable y could have a standard error - sigma! representing the model standard error - prediction error.
# --- slope of y MUST be 1, since you've set everything equal to y.
d = tribble(
  ~beta_x, ~beta_c, ~beta_y, ~se_x, ~se_c, ~se_y,
  2,       3,       1,       0.5,   0.1,    0.5,   
  -5.3,     4,      1,        0.4,   0.2,    0.4
)  %>%
  # Give a unique ID to each equation in your system of equations
  mutate(id = 1:n())

# Knowing nothing else about these variables...
dsim = d %>%
  group_by(id) %>%
  mutate(
    # Total variables being simulated
    n_p = length(c(beta_y, beta_x, beta_c)),
    # Get covariance matrix, where variance of the constant 'alpha' is 0
    # Assume interrelationships/inter-correlations are 0
    sigma = matrix(data = 0, nrow = n_p, ncol = n_p ) %>% `diag<-`(c(se_y, se_x, se_c)) %>% .^2 %>% list(),
    mu = c(beta_y, beta_x, beta_c) %>% list()
  ) %>%
  # For each equation, 
  group_by(id) %>%
  # Randomly simulate the slope coefficients for as many variables as are NOT the outcome y or constants alpha,
  # holding constant the beta_y and alpha
  reframe(
    # For as many reps as we make, give a unique id from 1 to n    
    # Generate 1000 simulations for each equation...
    mvtnorm::rmvnorm(n = 1000, mean = mu[[1]], sigma = sigma[[1]]) %>%
      as_tibble() %>%
      select(beta_y = 1, beta_x  = 2, beta_c = 3)
  ) %>%
  # For each equation,
  group_by(id) %>%
  # Add an ID for each replicate
  mutate(rep = 1:n()) %>%
  ungroup() %>%
  # Add in a zero row for the original
  bind_rows(d %>% mutate(rep = 0) %>% select(id, rep, beta_y, beta_x, beta_c))  %>%
  arrange(rep, id)


dsim
# Now, for each rep, let's solve the system of equations, 
# adding the following values x and y to the data.frame


solve_it = function(a, b, ...){
  # Get back number of columns
  purrr::possibly(.f = ~solve(a,b,...) %>% t(), 
                  otherwise = matrix(NA, nrow = 1, ncol = ncol(a)),
                  quiet  = FALSE)()
}


m = dsim %>%
  filter(rep == 0) %>%
  select(beta_y, beta_x, beta_c) %>%
  mutate(beta_x = -1*beta_x) %>%
  as.matrix()
solve(a = m[,-3], b = m[,3])
# m %>% solve(a = .[,-3], b = .[,3])

solve_it(a = m[,-3], b = m[,3])


coords = dsim %>%
  # Adjust the slopes so that we're showing
  # beta_x *x + beta_y*y = beta_c * c
  # Instead of 
  # beta_y*y = beta_x*x + beta_c*c
  # In other words, subtract beta_y*y from one side and beta_c*c from the other
  #mutate(beta_y = -1*beta_y,
  #       beta_c = -1*beta_c)  %>%
  # OR, more simply....
  # subtract beta_x*x (as many x's as there are)
  mutate(beta_x = -1*beta_x) %>%
  group_by(rep) %>%
  summarize(
    solve(a = tibble(beta_y, beta_x) %>% as.matrix(),
          b = tibble(beta_c) %>% as.matrix()) %>%
      t() %>%
      as_tibble() %>%
      select(y = 1, x = 2)
  )

coords

# 95% confidence solution space
ci = coords %>%
  filter(rep != 0) %>%
  filter(y >= quantile(y, probs = 0.025),
         y <= quantile(y, probs = 0.975),
         x >= quantile(x, probs = 0.025),
         x <= quantile(x, probs = 0.975))
ci

# Visualize the lines
lines = ci %>% 
  summarize(xmin = min(x),
            xmax = max(x)) %>%
  reframe(x = seq(from = xmin, to = xmax, by = 0.1)) %>%
  group_by(x) %>%
  reframe(dsim) %>%
  # Then calculate a predicted yhat value
  mutate(yhat = (beta_x * x + beta_c * 1) / beta_y ) %>%
  mutate(line = paste0(id,"-",rep)) 

# Visualize the solution space
ggplot() +
  geom_point(data = ci, mapping = aes(x = x, y = y), color = "white") +
  geom_density_2d_filled(data = ci, mapping = aes(x = x, y = y), alpha = 0.75) +
  geom_point(data = coords %>% filter(rep == 0),
             mapping = aes(x =x, y= y), shape = 21, color = "black", fill = "white", size = 5) +
  geom_line(data = lines %>% filter(rep == 0), 
            mapping = aes(x = x, y= yhat, group = line),
            alpha = 0.1) +
  coord_fixed(xlim = range(ci$x), ylim = range(ci$y))

# Visualize all lines, focusing on the solution space
ggplot() +
  geom_line(data = lines, mapping = aes(x = x, y= yhat, group = line, color = factor(id)),
            alpha = 0.1) +
  geom_point(data = ci, mapping = aes(x = x, y = y), color = "white") +
  geom_point(data = coords %>% filter(rep == 0),
             mapping = aes(x =x, y= y), shape = 21, color = "black", fill = "white", size = 5) +
  coord_fixed(xlim = range(ci$x), ylim = range(ci$y), ratio = 1/4)

# Solution Space
coords %>% filter(rep == 1)





# Pooled Standard Error #####################################################

# Original formula
# y = 2 * x + 3
# y = -5.3 * x + 4

# Suppose to start, you have a system of 2 equations, where...
# --- variable x has a standard error. 
# --- constant c **could** a standard error too if you choose.
# --- variable y could have a standard error - sigma! representing the model standard error - prediction error.
# --- slope of y MUST be 1, since you've set everything equal to y.
d = tribble(
  ~beta_x, ~beta_c, ~beta_y, ~se_x, ~se_c, ~se_y,
  2,       3,       1,       0.5,   0.1,    0.3,   
  -5.3,     4,      1,        0.4,   0.2,    0.2
)  %>%
  # Give a unique ID to each equation in your system of equations
  mutate(id = 1:n())

dsim = d %>%
  summarize(
    # Estimate a pooled standard error, not assuming equal variances, like so...    
    se_x = sqrt(sum(se_x^2)),
    se_y = sqrt(sum(se_y^2)),
    se_c = sqrt(sum(se_c^2)),
    # Total variables being simulated
    n_p = length(c("beta_y", "beta_x", "beta_c")),
    # Get covariance matrix, where variance of the constant 'alpha' is 0
    # Assume interrelationships/inter-correlations are 0
    sigma = matrix(data = 0, nrow = n_p, ncol = n_p ) %>% `diag<-`(c(se_y, se_x, se_c)) %>% .^2 %>% list(),
    # Where mu is held at 0
    mu = c(0,0,0) %>% list()
  ) %>%
  reframe(
    # For as many reps as we make, give a unique id from 1 to n    
    # Generate 1000 simulations for each equation...
    mvtnorm::rmvnorm(n = 1000, mean = mu[[1]], sigma = sigma[[1]]) %>%
      as_tibble() %>%
      select(error_y = 1, error_x  = 2, error_c = 3)
  ) %>%
  # Add a replicate ID
  mutate(rep = 1:n()) %>%
  # Add in a zero row for the original
  bind_rows(
    tibble(rep = 0, error_y = 0, error_x = 0, error_c = 0)
  ) %>%
  # Now, **whatever** the betas are, we're going to add these to BOTH
  # thus keeping the same basic relationships (?)
  tidyr::expand_grid(
    ., id = 1:nrow(d)
  ) %>%
  # Bring in the original system of equations
  left_join(by = "id", y = d %>% select(id, beta_x, beta_c, beta_y)) %>%
  # Update the betas with the simulations (original stays the same)
  mutate(
    beta_y = beta_y + error_y,
    beta_x = beta_x + error_x,
    beta_c = beta_c + error_c
  ) %>%
  arrange(rep, id) %>%
  # Return the final estimates
  select(rep, id, beta_x, beta_y, beta_c)

solve_it = function(a, b, ...){
  # Get back number of columns
  purrr::possibly(.f = ~solve(a,b,...) %>% t(), 
                  otherwise = matrix(NA, nrow = 1, ncol = ncol(a)),
                  quiet  = FALSE)()
}



coords = dsim %>% 
  # Adjust the slopes so that x and y are on same side and constant c is on other
  # OR, simply....
  # subtract beta_x*x (as many x's as there are)
  mutate(beta_x = -1*beta_x) %>%
  group_by(rep) %>%
  summarize(
    solve(a = tibble(beta_y, beta_x) %>% as.matrix(),
          b = tibble(beta_c) %>% as.matrix()) %>%
      t() %>%
      as_tibble() %>%
      select(y = 1, x = 2)
  )


# 95% confidence solution space
ci = coords %>%
  filter(rep != 0) %>%
  filter(y >= quantile(y, probs = 0.025),
         y <= quantile(y, probs = 0.975),
         x >= quantile(x, probs = 0.025),
         x <= quantile(x, probs = 0.975))
ci

# Visualize the lines
lines = ci %>% 
  summarize(xmin = min(x),
            xmax = max(x)) %>%
  reframe(x = seq(from = xmin, to = xmax, by = 0.1)) %>%
  group_by(x) %>%
  reframe(dsim) %>%
  # Then calculate a predicted yhat value
  mutate(yhat = (beta_x * x + beta_c * 1) / beta_y ) %>%
  mutate(line = paste0(id,"-",rep)) 
ci

# Visualize the solution space (y varies, but x doesn't)
ggplot() +
  geom_vline(data = ci, mapping = aes(xintercept = y), color = "grey") +
  geom_density(data = ci, mapping = aes(x = y), alpha = 0.5, fill = "steelblue")  +
  geom_vline(data = coords %>% filter(rep == 0),
             mapping = aes(xintercept = y), linetype = "dashed", color = "black", linewidth = 1.25)

# Solution Space
coords %>% filter(rep == 1)



