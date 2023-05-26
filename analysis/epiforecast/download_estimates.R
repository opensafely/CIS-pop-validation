################################################################################
# import estimates from epiforecast
# and saves these in ./epiforecast-data
################################################################################

################################################################################
# 00. Import libraries + functions
################################################################################
library(dplyr)
library(readr)
library(fs)
library(here)
library(httr)
library(tidyverse)

################################################################################
# 0.1 Create directories for output
################################################################################
output_dir <- "epiforecast-data"
dir_create(here(output_dir))

################################################################################
# 1.0 
################################################################################
req <- GET("https://api.github.com/repos/epiforecasts/inc2prev/git/trees/master?recursive=1")
file_path <- data.frame(unlist(lapply(content(req)$tree,
                                      function(x) x$path)))
colnames(file_path) <- c('Path')
# detect csv files in ./outputs of epiforecast repo
file_path <- file_path %>%
  separate(Path, c('base', 'level1', 'level2'), '/') %>%
  filter(base == 'outputs') %>%
  filter(str_detect(level1, '.csv')) %>%
  select(base, level1) %>%
  rename(filename = level1)
# only need estimates_ and cumulative_ files
file_names <- 
  file_path %>%
  filter(str_detect(filename, "estimates_[^_]+\\.csv") | 
           str_detect(filename, "cumulative_[^_]+\\.csv")) %>%
  pull(filename)
# construct urls needed to download files
urls <- 
  map_chr(.x = file_names,
          .f = ~ path("https://raw.githubusercontent.com/epiforecasts/inc2prev/master/outputs/",
                      .x))
url_read_script <- path("https://raw.githubusercontent.com/epiforecasts/inc2prev/master/scripts/",
                        "read.R")

################################################################################
# 2.0 Save estimates
################################################################################
walk2(.x = urls,
      .y = file_names,
      .f = ~ download.file(
        url = .x,
        destfile = path(here(output_dir), .y))
      )