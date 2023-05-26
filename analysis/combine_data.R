######################################

# Import aggregated, rounded measures data
# Import epiforecast data
# Combine and save to ./output/analysis/combined
######################################


# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('lubridate')
library('here')

## Import design elements ----
source(here("analysis", "lib", "functions.R"))

## define input/output directories ----
output_dir <- here("output", "analysis", "combined")
fs::dir_create(output_dir)
analysis_dir <- here("output", "analysis")
epiforecast_dir <- here("epiforecast-data")

# Import aggregated measures ----

import_data <- function(file_name, var){
  data <- readtype_csv(file = file_name) %>% 
    filter(period != "01" & !(measure %in% c("covidemergency", "covidadmitted"))) %>%
    rename(cat = .data[[var]]) %>%
    add_column(group = var, .before = "cat") %>%
    mutate(name = if_else(period == "ever", "cumulative_exposure", "infections"), .before = 1) %>%
    mutate(across(starts_with("rate"), ~ if_else(period == "14", .x / 14, .x))) %>%
    mutate(across(starts_with("var"), ~ if_else(period == "14", .x / (14 ^ 2), .x))) %>%
    mutate(rate_lower_weighted = rate_weighted - qnorm(0.975) * var_rate_weighted,
           rate_upper_weighted = rate_weighted + qnorm(0.975) * var_rate_weighted,
           rate_lower_unweighted = rate_unweighted - qnorm(0.975) * var_rate_unweighted,
           rate_upper_unweighted = rate_unweighted + qnorm(0.975) * var_rate_unweighted) %>%
    relocate(c(group, cat), .before = measure) %>%
    rename_with(.fn = ~ paste0("os_", .x), .cols = ends_with("weighted")) %>%
    select(-c(total_ons_pop, period, period_descr, starts_with("os_var")))
}
os_national <- 
  import_data(
    file_name = fs::path(analysis_dir, "rates_all.csv"),
    var = "all"
  ) %>% mutate(cat = "England", group = "national")
os_region <- 
  import_data(
    file_name = fs::path(analysis_dir, "rates_region.csv"),
    var = "region"
  ) %>%
  select(-c(events_roundmid6, population_roundmid6))
os_ageband5year <-
  import_data(
    file_name = fs::path(analysis_dir, "rates_ageband5year.csv"),
    var ="ageband5year"
  )

# Import epiforecast data ----

import_epiforecast <- function(file_name_infections, file_name_ever, var){
  epiforecast_infections <-
    read_csv(file = file_name_infections, show_col_types = FALSE) %>%
    pivot_longer(matches("^q[0-9]+$"), names_to = "quantile") %>%
    mutate(value = if_else(name == "infections", value / population, value)) %>%
    pivot_wider(names_from = "quantile") %>%
    filter(name == "infections") %>%
    select(name, variable, date, q5, q50, q95)
  epiforecast_ever <-
    read_csv(file = file_name_ever, show_col_types = FALSE) %>%
    filter(name == "cumulative_exposure") %>%
    select(name, variable, date, q5, q50, q95) 
  epiforecast <-
    bind_rows(epiforecast_infections,
              epiforecast_ever) %>%
    rename(cat = variable) %>%
    add_column(group = var, .after = "name") %>%
    rename_with(.f = ~ paste0("epiforecast_", .x), .cols = starts_with("q"))
}
epiforecast_national <-
  import_epiforecast(
    file_name_infections = fs::path(epiforecast_dir, "estimates_national.csv"),
    file_name_ever = fs::path(epiforecast_dir, "cumulative_national.csv"),
    var = "national"
  ) %>% filter(cat == "England")
epiforecast_region <-
  import_epiforecast(
    file_name_infections = fs::path(epiforecast_dir, "estimates_regional.csv"),
    file_name_ever = fs::path(epiforecast_dir, "cumulative_regional.csv"),
    var = "region"
  )
epiforecast_age <-
  import_epiforecast(
    file_name_infections = fs::path(epiforecast_dir, "estimates_age.csv"),
    file_name_ever = fs::path(epiforecast_dir, "cumulative_age.csv"),
    var = "age"
  ) %>% mutate(cat = if_else(cat == "2-10", "02-10", cat))

# Combine OS measures and epiforecast data ----

combine_os_epiforecast <- function(os, epiforecast){
  os %>% left_join(epiforecast, by = c("name", "group", "cat", "date")) %>%
    select(-ends_with("unweighted")) %>%
    pivot_longer(cols = c("os_rate_weighted", "epiforecast_q50"),
                 names_to = "source",
                 values_to = "rate") %>%
    mutate(source = ifelse(source == "os_rate_weighted", "OpenSAFELY", "epiforecast"),
           rate_lower = if_else(source == "OpenSAFELY", os_rate_lower_weighted, epiforecast_q5),
           rate_upper = if_else(source == "OpenSAFELY", os_rate_upper_weighted, epiforecast_q95)) %>% 
    select(-(starts_with("os_") | starts_with("epiforecast")))
}
data_national <- 
  combine_os_epiforecast(os_national, epiforecast_national)

# Save combined data
write_rds(data_national,
          fs::path(output_dir, "estimates_national.rds"))
# FIXME: currently different categories are used for region and age, need to 
# change the os estimates to match the epiforecast estimates