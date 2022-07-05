######################################

# Import measures data
# Import mid-year population estimates from ONS
# Calculate weights to match English population
# Aggregate measures at sex, ageband, and region level, weighted and unweighted
######################################


# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('lubridate')
library('here')

## Import design elements ----
source(here("analysis", "lib", "design.R"))
source(here("analysis", "lib", "functions.R"))

## define input/output directories ----

measures_dir <- here("output", "measures")

fs::dir_create(here("output", "analysis"))
analysis_dir <- here("output", "analysis")


# Import measures ----

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
    sex = factor(sex, levels=c("F", "M"), labels= c("Female", "Male")),
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
    sex,
    region,
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


# Derive tpp population estimates by date / region / sex / age

tpp_pop <-
  data_measures %>%
  filter(
    # should be exactly the same for other periods and measures
    period=="01",
    measure == "postest"
  ) %>%
  group_by(
    ageband5year,sex,region,year,date
  ) %>%
  summarise(
    tpp_pop=sum(population),
  ) %>%
  group_by(date) %>%
  mutate(
    tpp_prop = tpp_pop/sum(tpp_pop),
  ) %>% ungroup()


# import ONS mid population estimates ----

ons_pop <- read_rds(here("output", "ONS-data", "mid-year-pop.rds"))

ons_pop <-
  ons_pop %>%
  group_by(
    ageband5year,sex,region,year
  ) %>%
  summarise(
    ons_pop=sum(mid_year_pop)
  ) %>%
  group_by(year) %>%
  mutate(
    total_pop = sum(ons_pop),
    ons_prop = ons_pop/sum(ons_pop),
  ) %>%
  ungroup() %>%
  {
    ## use 2020 mid-year estimates until updated mid-year estimates come along...
    bind_rows(
      mutate(., year =2020),
      mutate(., year =2021),
      mutate(., year =2022),
      mutate(., year =2023)
    )
  }

# reweight rates ----

# calculate weights to reweight TPP rates
tpp_weights <-
  left_join(
    tpp_pop,
    ons_pop,
    by = c(
      "ageband5year",
      "sex",
      "region",
      "year"
    )
  ) %>%
  mutate(
    weight = ons_prop/tpp_prop
  )

# calculate reweighted rates
data_measures_weights <-
  left_join(
    data_measures,
    tpp_weights %>% select(ageband5year, sex, region, year, weight),
    by= c("ageband5year", "sex", "region", "year")
  ) %>%
  mutate(
    measure_descr = fct_recoderelevel(measure, recoder$measure),
    period_descr = fct_recoderelevel(period, recoder$period),
  )




# calculate aggregated outputs to match ONS-CIS data publications ----


roundmid_any <- function(x, to=1){
  # like round_any, but centers on (integer) midpoint of the rounding points
  ceiling(x/to)*to - (floor(to/2)*(x!=0))
}


rounded_rates <- function(data, ...){
  data %>%
    group_by(...) %>%
    summarise(
      events = sum(events),
      population = sum(population),
      #rate_unweighted = events/population,
      rate_weighted = sum(events*weight)/(sum(population*weight)), # = weighted.mean(events/population, population*weight),
    ) %>%
    ungroup() %>%
    # rounding
    mutate(
      events = roundmid_any(events,6),
      population = roundmid_any(population,6),
      rate_unweighted = events/population,
    )
}



data_sex <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, sex, date)
data_ageband5year <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, ageband5year, date)
data_region <- rounded_rates(data_measures_weights, measure, measure_descr, period, period_descr, region, date)


# write to file ----

## these should be imported back in using `readtype_csv`.

writetype_csv(data_sex, path = fs::path(analysis_dir, "rates_sex.csv"))
writetype_csv(data_ageband5year, path = fs::path(analysis_dir, "rates_ageband5year.csv"))
writetype_csv(data_region, path = fs::path(analysis_dir, "rates_region.csv"))

