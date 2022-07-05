# # # # # # # # # # # # # # # # # # # # #
# Purpose: creates metadata objects for aspects of the study design
# This script should be sourced (ie `source(".../design.R")`) in the analysis scripts
# # # # # # # # # # # # # # # # # # # # #

# Preliminaries ----

## Import libraries ----
library('tidyverse')
library('here')
## create output directories ----
fs::dir_create(here("analysis", "lib"))



# import globally defined repo variables
study_dates <-
  jsonlite::read_json(path=here("analysis", "lib", "study-dates.json")) %>%
  map(as.Date)

# define outcomes ----

measures_lookup <- tribble(
  ~measure, ~measure_descr,

  "postest", "Positive SARS-CoV-2 test",
  "primary_care_covid_case", "Infection or disease identified in primary care",
  "covidemergency", "COVID-19 A&E attendance",
  "covidadmitted", "COVID-19 hospitalisation",
  "any_infection_or_disease", "Any infection or disease",
)


## lookups to convert coded variables to full, descriptive variables ----

recoder <-
  lst(
    measure = c(
      "Positive SARS-CoV-2 test" = "postest",
      "Infection or disease identified in primary care" = "primary_care_covid_case",
      "COVID-19 A&E attendance" = "covidemergency",
      "COVID-19 hospitalisation" = "covidadmitted",
      "Any infection or disease" = "any_infection_or_disease"
    ),
    period = c(
      "Single-day" = "01",
      "Within previous 14-days" = "14",
      "Ever" = "ever"
    )
  )


# use this to factorise and relevel variables
# eg measure_descr = fct_recoderelevel(measure, recoder$measure)
fct_recoderelevel <- function(x, lookup){
  stopifnot(!is.na(names(lookup)))
  factor(x, levels=lookup, labels=names(lookup))
}
