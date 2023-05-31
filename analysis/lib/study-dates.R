# # # # # # # # # # # # # # # # # # # # #
# Purpose: Define the study dates that are used throughout the rest of the 
# project
# 
# Notes:
# This script is separate from the design.R script as the dates are used by the 
# study definition as well as analysis R scripts.
# # # # # # # # # # # # # # # # # # # # #

# create output directories ----
fs::dir_create(here::here("analysis", "lib"))

# define key dates ----
study_dates <- tibble::lst(
  start_date = "2020-04-26", # Sunday 26 April 2020, one day before the first 
                             # date of ONS-CIS
  #end_date = "2020-05-17", # Sunday 17 May temp used for testing locally
  end_date = "2023-02-26", # Sunday 26 February 2023, should be updated when rerun
)

## NOTE: we use one day before the start of ONS-CIS so that
## the fortnightly periods used by ONS-CIS are aligned, such that
## the study definition index date is the last day of those period.
## This makes it easier to run "look-back" periods, over 1, 14 and infinity 
## days.
## Single-day estimates therefore fall on a sunday, which isn't the best day to 
## use.
## So focus on fortnightly averages.

jsonlite::write_json(study_dates,
                     path = here::here("analysis", "lib", "study-dates.json"),
                     auto_unbox=TRUE, pretty =TRUE)
