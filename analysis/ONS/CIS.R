################################################################################
#
# 
#
################################################################################

library('tidyverse')
library("rvest")
library("readxl")
library("lubridate")
library("here")


fs::dir_create(here("ONS-data"))

ons_england_url <- 
  "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/conditionsanddiseases/datasets/coronaviruscovid19infectionsurveydata/2022/20220701covid19infectionsurveydatasetsengland.xlsx"

download.file(url = ons_england_url,
              destfile = fs::path(here("ONS-data"),"CIS-2022.xlsx"),
              mode = "wb")

#### TESTING

england_years <- c(2020 + seq(0, 2))
england_urls <- paste0(
  "https://www.ons.gov.uk/peoplepopulationandcommunity/",
  "healthandsocialcare/conditionsanddiseases/datasets/",
  "coronaviruscovid19infectionsurveydata/", england_years
) # nolint

# most current version + previous versions
file_urls <- lapply(england_urls, function(url) {
  session <- session(url)
  file_url <- session %>%
    html_element(xpath = paste0(
      "//a[contains(concat(' ', ",
      "normalize-space(@class),' '),' btn--primary ')]"
    )) %>%
    html_attr("href")
  return(file_url)
}) %>%
  unlist() %>%
  grep("\\.xlsx?$", value = TRUE, .)


#############

## positivity, fortnightly, overall ----

cis_positivity_2022_raw <-
  read_xlsx(
    path=fs::path(here("ONS-data"), "CIS-2022.xlsx"),
    sheet="UK summary - positivity",
    range="A7:E119",
    col_names = c("timeperiod", "positivity", "positivity.ll", "positivity.ul", "method")
  )

# cis_positivity_2022_raw <-
#   read_xlsx(
#     path=fs::path(here("ONS-data"), "CIS-2022.xlsx"),
#     sheet="1a",
#     range="A7:D120",
#     col_names = c("timeperiod", "positivity", "positivity.ll", "positivity.ul")
#   ) %>%
#   filter(
#     !is.na(positivity)
#   )

cis_positivity_2022 <-
  cis_positivity_2022_raw %>%
  mutate(
    date_from = dmy(str_split(timeperiod, fixed(" to "), simplify=TRUE)[,1]),
    date_to = dmy(str_split(timeperiod, fixed(" to "), simplify=TRUE)[,2])
  )


## incidence, fortnightly, overall ----

cis_incidence_2022_raw <-
  read_xlsx(
    path=fs::path(here("ONS-data"), "CIS-2022.xlsx"),
    sheet="UK summary - incidence",
    range="A8:E102",
    col_names = c("timeperiod", "incidence", "incidence.ll", "incidence.ul", "method")
  )

cis_incidence_2022 <-
  cis_incidence_2022_raw %>%
  mutate(
    date_from = dmy(str_split(timeperiod, fixed(" to "), simplify=TRUE)[,1]),
    date_to = dmy(str_split(timeperiod, fixed(" to "), simplify=TRUE)[,2])
  )


## positivity, daily, by region  ----


cis_positivity_2022_region_raw <-
  read_xlsx(
    path=fs::path(here("ONS-data"), "CIS-2022.xlsx"),
    sheet="1e",
    range="A7:CD48",
    col_names = paste0(
      c("date",
        rep(c("positivity", "positivity.ll", "positivity.ul", "npositivity", "npositivity.ll", "npositivity.ul", "rpositivity", "rpositivity.ll", "rpositivity.ul"), times=9)
      ),
      c("", rep("_",9*9)),
      c("",
        rep(c("North East", "North West", "Yorkshire and The Humber", "East Midlands", "West Midlands", "East of England", "London", "South East", "South West"), each=9)
      )
    )
  )

cis_positivity_2022_region <-
  cis_positivity_2022_region_raw %>%
  select(
    date,
    starts_with(("positivity"))
  ) %>%
  mutate(
    date = as.Date(date),
  ) %>%
  pivot_longer(
    cols=starts_with("positivity"),
    names_to = c("temp", "region"),
    names_sep="_",
    values_to = c("values")
  ) %>%
  pivot_wider(
    id_cols=c("date", "region"),
    names_from = "temp",
    values_from= "values"
  )



## positivity, daily, by region  ----

