#' @name generate_polygons.R
#' @description
#' Short script to get polygon for several datasets.


setwd(paste0(rstudioapi::getActiveProject()))
library(dplyr)
library(tigris)
# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# EMISSIONS ########################################
unlink("data/transportation/states.geojson")

## states ############################
tigris::states(year = 2020, cb = TRUE, resolution = "20m") %>%
  st_as_sf() %>%
  setNames(names(.) %>% tolower()) %>%
  select(geoid, state = stusps, name, area_land = aland, area_water = awater, geometry) %>%
  st_make_valid() %>%
  st_transform(crs = wgs)  %>%
  write_sf("data/transportation/states.geojson")

remove(shapes)

## counties ##################################

shapes = tigris::counties(year = 2020, cb = TRUE, resolution = "20m") %>%
  st_as_sf() %>%
  setNames(names(.) %>% tolower()) %>%
  st_make_valid() %>%
  st_transform(crs = wgs)  %>%
  select(geoid, state = stusps, name = namelsad, area_land = aland, area_water = awater, geometry) %>%
  write_sf("data/transportation/counties.geojson")


# 
# read_sf("data/transportation/counties.geojson") %>%
#   filter(state == "NY")

## roads ###########################################
roads = tigris::primary_secondary_roads(state = "NY", year = 2020) %>%
  st_as_sf() %>%
  setNames(names(.) %>% tolower()) %>%
  st_make_valid() %>%
  st_transform(crs = wgs)  %>%
  filter(rttyp %in% c("C", "S", "I", "U")) %>%
  write_sf("data/transportation/roads.geojson")

roads = read_sf("data/transportation/roads.geojson")
counties = read_sf("data/transportation/counties.geojson") %>% 
  filter(state == "NY") %>%
  select(geoid, geometry)

# Join the county ID into each 
roads %>%
  st_join(y = counties) %>%
  select(linearid, fullname, rttyp, mtfcc, geoid, geometry) %>%
  saveRDS("data/transportation/roads.rds")

unlink("data/transportation/roads.geojson")

# roads = read_sf("data/transportation/roads.geojson")

# roads %>%
#   as_tibble() %>%
#   group_by(rttyp) %>%
#   summarize(count = n(),
#             distance = st_length(geometry) %>% sum() %>% {./1000})
# 
# # Filter out the common name and other roads
# roads %>%
#   filter(rttyp %in% c("C", "S", "I", "U")) %>%
#   write_sf("data/transportation/roads.geojson")

# JAPAN ########################
# Get Japanese polygons

# https://github.com/timothyfraser/urban_regimes/blob/main/raw_data/map_regions.rds


library(dplyr)
library(readr)
library(sf)
library(ggplot2)
# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

unlink("data/japan/country.geojson")
country = read_rds("other/shapes_country.rds") %>%
  st_transform(crs = wgs) %>%
  st_make_valid() %>%
  write_sf("data/japan/country.geojson")

ggplot() +
  geom_sf(data = country)

#regions = read_rds("other/shapes_regions.rds")



pref = tribble(
  ~pref_code, ~pref_name, ~region,
  "01", "Hokkaido", "Hokkaido",
  "02", "Aomori", "Tohoku",
  "03", "Iwate", "Tohoku",
  "04", "Miyagi", "Tohoku",
  "05", "Akita", "Tohoku",
  "06", "Yamagata", "Tohoku",
  "07", "Fukushima", "Tohoku",
  "08", "Ibaraki", "Kanto",
  "09", "Tochigi", "Kanto",
  "10", "Gunma", "Kanto",
  "11", "Saitama", "Kanto",
  "12", "Chiba", "Kanto",
  "13", "Tokyo", "Kanto",
  "14", "Kanagawa", "Kanto",
  "15", "Niigata", "Chubu",
  "16", "Toyama", "Chubu",
  "17", "Ishikawa", "Chubu",
  "18", "Fukui", "Chubu",
  "19", "Yamanashi", "Chubu",
  "20", "Nagano", "Chubu",
  "21", "Gifu", "Chubu",
  "22", "Shizuoka", "Chubu",
  "23", "Aichi", "Chubu",
  "24", "Mie", "Kansai",
  "25", "Shiga", "Kansai",
  "26", "Kyoto", "Kansai",
  "27", "Osaka", "Kansai",
  "28", "Hyogo", "Kansai",
  "29", "Nara", "Kansai",
  "30", "Wakayama", "Kansai",
  "31", "Tottori", "Chugoku",
  "32", "Shimane", "Chugoku",
  "33", "Okayama", "Chugoku",
  "34", "Hiroshima", "Chugoku",
  "35", "Yamaguchi", "Chugoku",
  "36", "Tokushima", "Shikoku",
  "37", "Kagawa", "Shikoku",
  "38", "Ehime", "Shikoku",
  "39", "Kochi", "Shikoku",
  "40", "Fukuoka", "Kyushu",
  "41", "Saga", "Kyushu",
  "42", "Nagasaki", "Kyushu",
  "43", "Kumamoto", "Kyushu",
  "44", "Oita", "Kyushu",
  "45", "Miyazaki", "Kyushu",
  "46", "Kagoshima", "Kyushu",
  "47", "Okinawa", "Kyushu"
)

unlink("data/japan/prefectures.geojson")
read_rds("other/shapes_prefectures.rds") %>%
  st_transform(crs = wgs) %>%
  st_make_valid() %>%
  left_join(by = "pref_code", y = pref) %>%
  select(pref_code, pref_name, region) %>%
  write_sf("data/japan/prefectures.geojson")

unlink("data/japan/regions.geojson")
regions = read_sf("data/japan/prefectures.geojson") %>%
  st_make_valid() %>%
  group_by(region) %>%
  summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  filter(!is.na(region)) %>%
  write_sf("data/japan/regions.geojson")



#dataset = read_csv("https://github.com/timothyfraser/better_together/blob/main/raw_data/dataset.csv")

dataset = read_csv("other/dataset.csv") %>%
  filter(year == max(year)) %>%
  select(muni_code, muni, muni_en, pref, pref_code) %>%
  mutate(muni_en = muni_en %>% stringr::str_remove(pattern = paste0(pref, "[ ]")) ) %>%
  mutate(muni_type = stringr::str_extract(muni_en, "[-].*") %>% stringr::str_remove("[-]")) %>%
  mutate(muni_type = case_when(
    muni_en == "Takizawashi" ~ "shi",
    TRUE ~ muni_type
  )) %>%
  mutate(muni_en = stringr::str_remove(muni_en, pattern = paste0("[-]", muni_type) ))  %>%
  select(muni_code, muni_jp = muni, muni = muni_en, 
         muni_type_jp = muni_type,
         pref, pref_code) %>%
  mutate(muni_type = muni_type_jp %>% dplyr::recode(
    "shi" = "city",
    "ku" = "ward",
    "machi" = "town",
    "cho" = "town",
    "mura" = "village",
    "son" = "village"
  )) %>%
  select(muni_code, muni, muni_type, muni_type_jp, muni_jp, pref, pref_code) %>%
  left_join(by = "pref_code", y = pref %>% select(pref_code, region)) %>%
  write_csv("data/japan/municipality_names.csv")


muni = read_rds("other/shapes_municipalities.rds") 

muni %>%
  left_join(by = "muni_code", y = read_csv("data/japan/municipality_names.csv")) %>%
  select(muni_code, muni:region, geometry) %>%
  st_transform(crs = wgs) %>%
  st_make_valid() %>%
  write_sf("data/japan/municipalities.geojson")

read_sf("data/japan/municipalities.geojson")

rm(list = ls())

# FLORIDA #############################
library(dplyr)
library(readr)
library(sf)
# Generate county subdivisions in Florida
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


read_rds("data/evacuation/nodes.rds") %>%
  select(geoid) %>%
  mutate(code = stringr::str_sub(geoid, 1,2)) %>%
  select(code) %>%
  distinct() 

## states #####################
tigris::states(year = 2020, cb = TRUE, resolution = "20m") %>%
  st_as_sf() %>%
  setNames(names(.) %>% tolower()) %>%
  select(geoid, state = stusps, name, area_land = aland, area_water = awater, geometry) %>%
  filter(geoid %in% c("01", "12", "13")) %>%
  st_make_valid() %>%
  st_transform(crs = wgs)  %>%
  write_sf("data/evacuation/states.geojson")

## counties #########################

unlink("data/evacuation/counties.geojson", force = TRUE)

# Get all counties in those states
shapes = tigris::counties(state = c("01", "13", "12"), year = 2020, cb = TRUE, resolution = "20m") %>%
  st_as_sf() %>%
  setNames(names(.) %>% tolower()) %>%
  select(geoid, state = stusps, name = namelsad, area_land = aland, area_water = awater, geometry) %>%
  st_make_valid() %>%
  st_transform(crs = wgs) %>%
  write_sf("data/evacuation/counties.geojson")

## county subdivisions ########################

# Get valid counties in the study region  
valid = read_rds("data/evacuation/nodes.rds") %>%
  select(geoid) %>%
  mutate(code = stringr::str_sub(geoid, 1,5)) %>%
  select(code)  %>%
  distinct() %>%
  mutate(state = stringr::str_sub(code, 1,2),
         county = stringr::str_sub(code, 3,5))

csub = valid %>%
  split(.$code) %>%
  purrr::map_dfr(~tigris::county_subdivisions(
    state = .$state, county = .$county, 
    year = 2020, cb = TRUE) %>%
      st_as_sf())
# Get all county subdivisions in the study area
csub %>%
  setNames(names(.) %>% tolower())  %>%
  mutate(geoid_county = stringr::str_sub(geoid, 1,5)) %>%
  select(geoid, name, state = stusps, county = namelsadco, geoid_county, area_land = aland, area_water = awater, geometry) %>%
  st_make_valid() %>%
  st_transform(crs = wgs) %>%
  write_sf("data/evacuation/county_subdivisions.geojson")

read_sf("data/evacuation/county_subdivisions.geojson")


rm(list = ls())

## roads #############################
# Get EPSG:4326 (WGS 84) projection
wgs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


roads = tibble(state = c("01", "12", "13")) %>%
  split(.$state) %>%
  purrr::map_dfr(
    ~tigris::primary_secondary_roads(
      state = .$state,
      year = 2020) %>%
      st_as_sf() %>%
      setNames(names(.) %>% tolower()) %>%
      st_make_valid() %>%
      st_transform(crs = wgs)
  )

roads %>% 
  filter(rttyp %in% c("C", "S", "I", "U")) %>%
  saveRDS("data/evacuation/roads.rds")

# Now spatially join in every county subdivision, county, and state ID to overlapping roads
csub = read_sf("data/evacuation/county_subdivisions.geojson")  %>%
  select(geoid, geoid_county, state, geometry)

# Overwrite the original with this slightly smaller set
read_rds("data/evacuation/roads.rds") %>%
  st_join(y = csub, left = FALSE) %>%
  saveRDS("data/evacuation/roads.rds")

# write_sf("data/evacuation/roads.geojson")

# roads = read_sf("data/transportation/roads.geojson")
# 
# roads %>%
#   as_tibble() %>%
#   group_by(rttyp) %>%
#   summarize(count = n(),
#             distance = st_length(geometry) %>% sum() %>% {./1000})
# 
# # Filter out the common name and other roads
# roads %>%
#   filter(rttyp %in% c("C", "S", "I", "U")) %>%
#   write_sf("data/transportation/roads_select.geojson")
