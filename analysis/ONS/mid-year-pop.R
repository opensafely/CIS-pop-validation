# # # # # # # # # # # # # # # # # # # # #
# Purpose: Download and restructure mid pop estimates to be used to weight the
# TPP population
#
# Notes: the 2020 mid year population estimates are used for reweighing
# # # # # # # # # # # # # # # # # # # # #

# preliminaries ----
## import libraries ----
library('tidyverse')
library('onsr')
library('here')
## create output directories ----
fs::dir_create(here("ONS-data"))

# download mid year pop estimates ----
ons_pop_estimates_2020 <- 
  ons_get("mid-year-pop-est", edition = "mid-2020-april-2021-geography") %>%
  filter(`calendar-years`==2020)
ons_pop_estimates_2021 <- 
  ons_get("mid-year-pop-est", edition = "mid-2021-april-2022-geography") %>%
  filter(`calendar-years`==2021)
ons_pop_estimates <-
  bind_rows(ons_pop_estimates_2020,
            ons_pop_estimates_2021)

# restructure to same format as our estimates are in ----
ons_pop_estimates_region <-
  ons_pop_estimates %>%
  filter(
    `administrative-geography` %in% c(
      "E12000001",
      "E12000002",
      "E12000003",
      "E12000004",
      "E12000005",
      "E12000006",
      "E12000007",
      "E12000008",
      "E12000009" 
    ), # nine regions in England
    sex %in% c("male", "female"),
    `single-year-of-age` %in% c(0:89, "90+")
  ) %>%
  transmute(
    year=`calendar-years`,
    age=`single-year-of-age`,
    age_int = as.integer(str_extract(age, "\\d+")), # remove +
    ageband5year = case_when(
      age_int>=0 & age_int<=4 ~ "0-4",
      age_int>=5 & age_int<=9 ~ "5-9",
      age_int>=10 & age_int<=14 ~ "10-14",
      age_int>=15 & age_int<=19 ~ "15-19",
      age_int>=20 & age_int<=24 ~ "20-24",
      age_int>=25 & age_int<=29 ~ "25-29",
      age_int>=30 & age_int<=34 ~ "30-34",
      age_int>=35 & age_int<=39 ~ "35-39",
      age_int>=40 & age_int<=44 ~ "40-44",
      age_int>=45 & age_int<=49 ~ "45-49",
      age_int>=50 & age_int<=54 ~ "50-54",
      age_int>=55 & age_int<=59 ~ "55-59",
      age_int>=60 & age_int<=64 ~ "60-64",
      age_int>=65 & age_int<=69 ~ "65-69",
      age_int>=70 & age_int<=74 ~ "70-74",
      age_int>=75 & age_int<=79 ~ "75-79",
      age_int>=80 & age_int<=84 ~ "80-84",
      age_int>=85 & age_int<=89 ~ "85-89",
      age_int>=90 ~ "90+",
    ),
    agebandCIS = case_when(
      age_int>=2 & age_int<=10 ~ "2-Y6",
      age_int>=11 & age_int<=15 ~ "Y7-Y11",
      age_int>=16 & age_int<=24 ~ "Y12-24",
      age_int>=25 & age_int<=34 ~ "25-34",
      age_int>=35 & age_int<=49 ~ "35-49",
      age_int>=50 & age_int<=69 ~ "50-69",
      age_int>=70 ~ "70+",
    ),
    ageband5year = factor(ageband5year,
                          levels = c("0-4",
                                     "5-9",
                                     "10-14",
                                     "15-19",
                                     "20-24",
                                     "25-29",
                                     "30-34",
                                     "35-39",
                                     "40-44",
                                     "45-49",
                                     "50-54",
                                     "55-59",
                                     "60-64",
                                     "65-69",
                                     "70-74",
                                     "75-79",
                                     "80-84",
                                     "85-89",
                                     "90+")),
    agebandCIS = factor(agebandCIS,
                        levels = c("2-Y6",
                                   "Y7-Y11",
                                   "Y12-24",
                                   "25-34",
                                   "35-49",
                                   "50-69",
                                   "70+")),
    sex=`Sex`,
    region=str_replace(str_to_title(`Geography`), "And The", "and The"), # TPP
    mid_year_pop=`v4_0`) %>%
  mutate(
    region_epiforecast = case_when(
      region == "East Midlands" | region == "West Midlands" ~ "Midlands",
      region == "North East" | region == "Yorkshire and The Humber" ~ "North East and Yorkshire",
      TRUE ~ region),
    .before = mid_year_pop) %>%
  arrange(
    year,
    age_int,
    sex,
    region
  )

# save restructured estimates ----
write_rds(ons_pop_estimates_region, here("ONS-data", "mid-year-pop.rds"))
