# download_data.R
# A script for downloading all necessary data for this database
# Note: for instructor use only.

library(dplyr)
library(readr)
library(sf)
setwd(rstudioapi::getActiveProject())

# Stop message
stop("Don't run this script.\nThis is just for the instructor to download the data. \nSee data/redlining/README.md for how to use the data.")


# Download manually from this webpage:
# https://archive.icpsr.umich.edu/nanda/view/studies/141121/data-documentation

format = function(path){
  path %>%
  readxl::read_excel() %>%
    mutate(across(.cols = c(contains("GEOID"), contains("CBSA"), contains("METRO")),
                .fns = ~as.character(.x))) %>%
    mutate(across(.cols = c(contains("HRI"), contains("INT")),
                  .fns = ~as.numeric(.x))) %>%
    select(geoid = 1, cbsa = 2, metro_name = 3, hri =  4, interval = 5)
}


data = bind_rows(
  format("data/redlining/Historic Redlining Indicator 2000.xlsx") %>% mutate(year = 2000),
  format("data/redlining/Historic Redlining Indicator 2010.xlsx") %>% mutate(year = 2010),
  format("data/redlining/Historic Redlining Indicator 2020.xlsx") %>% mutate(year = 2020)
)

# Save to file metro names
data %>%
  select(cbsa, metro_name) %>%
  distinct() %>%
  arrange(desc(cbsa)) %>%
  write_csv("data/redlining/cbsa.csv")

# Now save to file without metro name - cleaner
data %>%
  select(-metro_name) %>%
  mutate(county = stringr::str_sub(geoid, 1, 5)) %>%
  select(year, cbsa, county, geoid, hri, interval) %>%
  write_csv("data/redlining/redlining.csv")


paths = c("data/redlining/Historic Redlining Indicator 2000.xlsx",
          "data/redlining/Historic Redlining Indicator 2010.xlsx",
          "data/redlining/Historic Redlining Indicator 2020.xlsx")

unlink(paths)
rm(list = ls())


