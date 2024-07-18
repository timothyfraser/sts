# Borrowed from:
# https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/GBRVWY

library(dplyr)
library(readr)
library(stringr)
library(readxl)


pref = tribble(
  ~pref_code, ~pref, ~region,
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

readxl::read_excel("other/Database_prefectural_analysis_R_relevant_variables.xlsx") %>%
  setNames(names(.) %>% tolower()) %>%
  select(code, prefecture_english, municipality_prefecture_english, 
         pv_output_2018, windspeed_2018, solarkw, sp, sp_10_49_kw, sp_50_499_kw, sp_500_1999kw, `sp_2000_kw+`,
         land_price_commercial_2009, land_price_residential_2009,
         area_2010, population_2010, unemployment_2010, voter_turnout_2012, crime_rate_2008, income_taxable_per_capita_2010, financial_str_index_2010, ratio_revs_exp_2010,
         death11, damaged11, coast, fukushima_exclusion_zone) %>%
  rename(muni = municipality_prefecture_english,
         pref = prefecture_english,
         muni_code = code,
         sp_10_49kw = sp_10_49_kw,
         sp_50_499kw = sp_50_499_kw,
         #sp_500_1999kw,
         sp_2000kw_plus = `sp_2000_kw+`
         )  %>%
  # Get the name of the municipality
  mutate(muni = str_extract(muni, "[ ].*") %>% str_trim("left")) %>%
  mutate(muni_type_jp = str_extract(muni, "[-].*") %>% str_remove("-")) %>%
  mutate(muni = str_remove(muni, paste0("[-]", muni_type_jp))) %>%
  mutate(muni_type = muni_type_jp %>% dplyr::recode(
    "shi" = "city",
    "cho" = "town",
    "machi" = "town",
    "mura" = "village",
    "son" = "village",
    "ku" = "ward"
  )) %>%
  mutate(muni_type_jp = case_when(muni == "Takizawashi" ~ "shi", TRUE ~ muni_type_jp),
         muni_type = case_when(muni == "Takizawashi" ~ "city", TRUE ~ muni_type)) %>%
  # Join in the region names
  left_join(by = "pref", y = pref) %>%
  select(muni_code, muni, muni_type, pref, pref_code, region, fukushima_exclusion_zone, coast, pv_output_2018:damaged11) %>%
  rename(disaster_deaths_2011 = death11,
         disaster_damage_2011 = damaged11) %>%
  write_csv("data/jp_solar_farms_2018.csv")

rm(list = ls())

