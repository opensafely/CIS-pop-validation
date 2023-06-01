######################################
# purpose: makes combinations of valid categories
# and saves this table to ./epiforecast-data/lookupcat.rds
######################################

# preliminaries ----
## import libraries ----
library(tidyverse)

# combinations of age ----
lookup_agecat <- 
  tibble(ageband5year = c("0-4",
                          "5-9",
                          "10-14",
                          "10-14",
                          "15-19",
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
                          "90+"),
         agebandCIS = c(rep("2-Y6", 3),
                        rep("Y7-Y11", 2),
                        rep("Y12-24", 2),
                        rep("25-34", 2),
                        rep("35-49", 3),
                        rep("50-69", 4),
                        rep("70+", 5)))

# combinations of region ----
lookup_regioncat <- 
  tibble(region = c("North East", 
                    "North West", 
                    "Yorkshire and The Humber",
                    "East Midlands",
                    "West Midlands",
                    "East",
                    "London",
                    "South East",
                    "South West"),
         region_epiforecast = c("North East and Yorkshire",
                                "North West",
                                "North East and Yorkshire",
                                "Midlands",
                                "Midlands",
                                "East",
                                "London",
                                "South East",
                                "South West"))
# comibine tables ----
lookup_cat <- tidyr::crossing(lookup_agecat, lookup_regioncat)
  
# save output ----
write_rds(lookup_cat,
          here("epiforecast-data", "lookupcat.rds"))
