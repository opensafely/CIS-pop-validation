######################################
# Import measures data
# Import mid-year population estimates from ONS
# Calculate weights to match English population
# Aggregate measures at sex, ageband, and region level, weighted and unweighted
######################################

# preliminaries ----
## import libraries ----
library('tidyverse')
library('lubridate')
library('here')
## import design elements ----
source(here("analysis", "lib", "design.R"))
source(here("analysis", "lib", "functions.R"))
## define input/output directories ----
measures_dir <- here("output", "measures")
fs::dir_create(here("output", "analysis"))
analysis_dir <- here("output", "analysis")

# import measures ----
# all measure csv files
measures_path_all <- fs::dir_ls(path=measures_dir, glob="*.csv", type="file")
# only summary files (exclude files with date suffix)
measures_path_summary <- measures_path_all[!str_detect(measures_path_all, "\\_\\d+\\-\\d+\\-\\d+\\.csv$")]
# name of measure
measures_path_summary %>%
  fs::path_file() %>%
  fs::path_ext_remove() %>%
  str_remove("measure_")

# import measures data from csv and standardise / tidy dataset
data_measures <-
  map_dfr(
    measures_path_summary,
    function(path){

      measure_period <-
        path %>%
        fs::path_file() %>%
        fs::path_ext_remove() %>%
        str_remove("measure_")

      dat <- read_csv(path)
      names(dat)[names(dat)==measure_period] <- "events"
      dat$measure_period <- measure_period

      dat
    }
  ) %>%
  mutate(
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
    sex = factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
    events = as.integer(events),
    population = as.integer(population),
    date = as.Date(date),
    year = as.integer(lubridate::year(date)),
    period = str_extract(measure_period, "[:alnum:]+$"),
    measure = str_remove(measure_period, paste0("_",period)),
    measure = factor(
      measure,
      levels = c(
        "postest",
        "primary_care_covid_case",
        "covidemergency",
        "covidadmitted",
        "any_infection_or_disease"
      )
    ),
    period = factor(
      period,
      levels = c(
        "01",
        "14",
        "ever"
      )
    )
  ) %>%
  select(
    ageband5year,
    agebandCIS,
    sex,
    region,
    region_epiforecast,
    year,
    measure,
    period,
    date,
    events,
    population,
    rate=value,
  ) %>%
  arrange(
    ageband5year,
    sex,
    region,
    year,
    measure,
    period,
    date,
  )

# change dummy data to make code testable 
if(Sys.getenv("OPENSAFELY_BACKEND") %in% c("", "expectations")){
  data_measures <- 
    data_measures %>% 
    group_by(ageband5year, sex, region, year, measure, period, date) %>%
    summarise(events = sum(events),
              population = sum(population),
              rate = events / population,
              .groups = "keep") %>%
    mutate(
      agebandCIS = case_when(
        ageband5year == "0-4" | ageband5year == "5-9" ~ "2-Y6",
        ageband5year == "10-14" ~ "Y7-Y11",
        ageband5year == "15-19" | ageband5year == "20-24" ~ "Y12-24",
        ageband5year == "25-29" | ageband5year == "30-34" ~ "25-34",
        ageband5year %in% c("35-39", "40-44", "45-49")~ "35-49",
        ageband5year %in% c("50-54", "55-59", "60-64", "65-69") ~ "50-69",
        TRUE ~ "70+",
      ),
      region_epiforecast = case_when(
        region == "East Midlands" | region == "West Midlands" ~ "Midlands",
        region == "North East" | region == "Yorkshire and The Humber" ~ "North East and Yorkshire",
        TRUE ~ region)
    )
}

# import ONS mid population estimates ----
ons_pop <- read_rds(here("ONS-data", "mid-year-pop.rds"))

# in the above, age_int is eg 0, 1, 2, 4 and ageband5year is 0-4
# we want to know the total number in ons pop in the ageband5year,
# so we sum over the different age_int in the category ageband5year
ons_pop <- 
  ons_pop %>%
  group_by(year, ageband5year, agebandCIS, sex, region, region_epiforecast) %>%
  summarise(ons_pop = sum(mid_year_pop),
            .groups = "keep") %>%
  ungroup() # forget grouping

# 'ons_pop' has 684 rows: 19 ageband x 2 sex x 9 region x 2 year (2020 and 2021)
# we're using 2021 for the years 2022 and 2023
# we multiply this table by 4, (342 x 4) to account for every year (2020-2023)
# [note that we're using the same tables for every year for now]
# the column total_ons_pop is needed to standardise the rates using the ons pop
ons_pop <-
  ons_pop %>%
  {
    ## use 2020 mid-year estimates until updated mid-year estimates come along...
    bind_rows(
      filter(., year == 2020),
      filter(., year == 2021),
      filter(., year == 2021) %>% mutate(., year = 2022),
      filter(., year == 2021) %>% mutate(., year = 2023),
    )
  }

# join measures and ons_pop table
data_measures_weights <-
  left_join(
    data_measures,
    ons_pop %>% select(ageband5year, agebandCIS, sex, region, region_epiforecast, year, ons_pop),
    by = c("ageband5year", "agebandCIS", "sex", "region", "region_epiforecast", "year")
  ) %>%
  mutate(
    all = "all",
    measure_descr = fct_recoderelevel(measure, recoder$measure),
    period_descr = fct_recoderelevel(period, recoder$period),
  )

# calculate aggregated outputs to match ONS-CIS data publications ----
roundmid_any <- function(x, to=1){
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}
# notes: round_any rounds x/to to the nearest integer --> roundmid_any takes the
# ceiling. 
# 0 is mapped to 0,
# 1, 2, 3, 4, 5, 6 is mapped to 3
# 7, 8, 9, 10, 11, 12 is mapped to 9
# 13, 14, 15, 16, 17, 18 is mapped to 15 
# etc.
# it maps to left mid of interval
rounded_rates <- function(data, ...){
  data %>%
    group_by(...) %>%
    summarise(
      events_total = sum(events),
      population_total = sum(population),
      total_ons_pop = sum(ons_pop),
      rate_weighted = sum((ons_pop / total_ons_pop) * rate), 
      var_rate_weighted = sum((1 / total_ons_pop^2) * (ons_pop^2 / population) * rate * (1 - rate)),
      .groups = "keep",
    ) %>%
    ungroup() %>%
    # rounding
    mutate(
      events_roundmid6 = roundmid_any(events_total,6),
      population_roundmid6 = roundmid_any(population_total,6),
      rate_unweighted = events_roundmid6 / population_roundmid6,
      var_rate_unweighted = (rate_unweighted * (1 - rate_unweighted)) / population_roundmid6,
    ) %>%
    select(-c(events_total, population_total))
}

data_sex <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, sex, date)
data_ageband5year <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, ageband5year, date)
data_agebandCIS <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, agebandCIS, date)
data_region <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, region, date)
data_region_epiforecast <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, region_epiforecast, date)
data_all <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, all, date)

# write to file ----

## these should be imported back in using `readtype_csv`.

writetype_csv(data_sex, path = fs::path(analysis_dir, "rates_sex.csv"))
writetype_csv(data_ageband5year, path = fs::path(analysis_dir, "rates_ageband5year.csv"))
writetype_csv(data_ageband5year, path = fs::path(analysis_dir, "rates_agebandCIS.csv"))
writetype_csv(data_region, path = fs::path(analysis_dir, "rates_region.csv"))
writetype_csv(data_region, path = fs::path(analysis_dir, "rates_region_epiforecast.csv"))
writetype_csv(data_all, path = fs::path(analysis_dir, "rates_all.csv"))
