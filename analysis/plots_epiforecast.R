################################################################################
#
# Plot estimates using the .csv files in ./estimates_*.csv
# This script is an adapted version of ./scripts/plot_estimates.R that relies on
# the availability of the estimates in .rds files (--> not pushed to the repo)
#
# Attempt by: Linda Nab // linda.nab@phc.ox.ac.uk
# On: 16/11/2022
################################################################################

################################################################################
# 00. Import libraries + functions
################################################################################
library(here)
library(purrr)
library(ggplot2)
library(socialmixr)
library(dplyr)
library(readr)
library(scales)
library(tidyr)
source(here::here("scripts", "read.R"))

## whether to make spaghetti plots
spaghetti <- FALSE
filetype <- "png"

## Get tools
prev <- read_cis()
local_region <- prev %>%
  filter(level == "local") %>%
  select(level, variable, region) %>%
  distinct()

dir.create(here::here("figures", "additional"),
           showWarnings = FALSE, recursive = TRUE)

var_names <- c(
  est_prev = "Prevalence estimate",
  infections = "Estimated incidence",
  dcases = "Daily estimated prevalence",
  r = "Growth rate",
  Rt = "Reproduction number",
  cumulative_infections = "Cumulative incidence",
  cumulative_exposure = "Proportion ever infected"
)

labels <- list(
  est_prev = scales::percent_format(1L),
  infections = scales::percent_format(0.1),
  dcases = scales::percent_format(1L),
  r = waiver(),
  Rt = waiver(),
  cumulative_infections = scales::percent_format(1L),
  cumulative_exposure = scales::percent_format(1L)
)

group <- c(
  age_school = "Age group",
  national = "Nation",
  regional = "Region",
  variant_regional = "Variant",
  variant_national = "Variant"
)

hline <- c(
  r = 0,
  Rt = 1
)

histories <- list(all = Inf, `1year` = months(12), `3months` = months(3))
breaks <- c(all = "4 months", `1year` = "3 month", `3months` = "1 month")

# use .csv files and not .rds files
file_pattern <-
  paste0("estimates", "_[^_]+\\.csv")

files <-
  list.files(here("epiforecast-data"),
             pattern = file_pattern,
             full.names = TRUE)
# 5 files: _age, _local, _national, _regional, _variants 

for (file in files) {
  # SHORT SUMMARY OF DATA AND SOME COLUMNS -------------------------------------
  # data has colnames: name, mean, sd, median, mad, n_index, t_index, p_index, 
  # variable, date, level, population, q5-95
  # --> variable 'name' has the following unique values:
  # alpha, dcases, est_prev, eta, infections, init_growth, init_inc, r, R, rho
  # --> variable 'level' has, dependent on which file is read, values:
  # age_school (_age); local (_local); national (_national); 
  # regional (_regional); variant_national + variant_regional (_variant)
  # --> variable 'variable' contains, dependent on 'level', the level of 'level'
  # e.g. if level == 'age_school', variable has values:
  # 2-10; 11-15; 16-24; 25-34; 35-49; 50-69; 70+
  # CHANGES --------------------------------------------------------------------
  # In the original plot_estimates.r script, the following data wrangling is 
  # done:
  # data <- readRDS(file) %>%
  #   separate(variable, c("variable", "geography"), sep = "\\|")
  # this however leads to warnings in all but the _variant file. This step is 
  # therefore moved inside the if statement 
  # (any(grepl("^variant", unique(data$level))))
  # The .rds files differ from the .csv files, it is therefore needed to back
  # transform the .csv files to the .rds files as closely as possible. In the 
  # estimates.r script, the .rds files are saved first. Then, the estimates are
  # 'formatted' in format_estimates and then saved as .csv files. The inverse of
  # the formatting done in the estimates.r script is done in the lines of code 
  # below. Values for est_prev are divided by 100 and values for infections are 
  # divided by the population size. 
  # NOTE: in the estimates.r script, the values have been rounded, exact back 
  # transform is therefore not possible. 
  # NOTE2: in the estimates.r script, the unformatted estimates (saved in the 
  # .rds file) are left joined with 'population' and subsequently all rows with 
  # is.na(population) have been filtered out. It's not clear to me if this leads
  # to loss of information in the .csv files.
  data <- read_csv(file) %>%
    pivot_longer(matches("^q[0-9]+$"), names_to = "quantile") %>%
    mutate(value = if_else(name == "est_prev", value / 100, value),
           value = if_else(name == "infections", value / population, value)) %>%
    pivot_wider(names_from = "quantile")
  # ----------------------------------------------------------------------------
  cum_file <-
    here::here("epiforecast-data", paste0("cumulative_", sub("^[^_]+_", "", file)))
  if (file.exists(cum_file)) {
    cum_data <- read_csv(cum_file) %>%
      filter(name %in% names(var_names))
    if (any(grepl("^variant", unique(data$level)))) {
      # the bit that was first done outside this if statement
      data <- data %>%
        separate(variable, c("variable", "geography"), sep = "\\|")
      cum_data <- cum_data %>%
        mutate(geography = as.character(variable),
               variable = level)
    }
    if (!spaghetti) {
      # CHANGES ----------------------------------------------------------------
      # In the plot_estimates.r script, the following data wrangling is done:
      # quantiles <- parse_number(grep("^q", colnames(data), value = TRUE)) / 100
      # cum_data <- cum_data %>%
      #  group_by_at(vars(intersect(colnames(data), colnames(.)))) %>%
      #  summarise(
      #   value = quantile(value, quantiles),
      #   q = paste0("q", quantiles * 100),
      #   .groups = "drop"
      #  ) %>%
      #  pivot_wider(names_from = "q")
      # In the analyse.R script, 'cumulative' is saved in the .rds files and
      # then these are formatted in 'csum' (the .csv file). I assume that the
      # data wrangling done there is equal to the data wrangling here, and can
      # therefore be omitted.
    }
    data <- data %>%
      bind_rows(cum_data)
    if (!"geography" %in% colnames(data)) { # in all but the _variant file
      data <- data %>%
        mutate(geography = NA_character_,
               variable = as.character(variable))
    }
    # --------------------------------------------------------------------------
  }
  
  for (level in unique(data$level)) {
    level_data <- data %>%
      filter(level == {{ level }}) %>%
      mutate(variable = if_else(variable == "2-10", "02-10", variable),
             name = if_else(name == "R", "Rt", name)) %>%
      filter(name %in% names(var_names)) # est_prev, infections, dcases, r, Rt,
    # cumulative_infections; cumulative_exposure
    
    if (level == "local") {
      level_data <- level_data %>%
        left_join(local_region, by = c("level", "variable")) # adds column
      # region with names for the J* codes in column 'variable'
    }
    
    colour_var <- ifelse(level == "local", "region", "variable")
    
    for (history in names(histories)) { # all, 1year, 3months
      for (name in unique(level_data$name)) {
        plot_df <- level_data %>%
          filter(name == {{ name }}, 
                 date > max(date) - histories[[history]])
        aes_str <- list(x = "date", colour = colour_var)
        if (spaghetti) {
          plot_df <- plot_df %>%
            mutate(var_sample = interaction(variable, sample))
          aes_str <- c(aes_str, list(y = "value", group = "var_sample"))
        } else {
          aes_str <- c(aes_str, list(y = "q50", fill = colour_var))
        }
        p <- ggplot(plot_df, do.call(aes_string, aes_str)) +
          ylab(name) + xlab("")
        if (spaghetti) {
          p <- p +
            geom_line(alpha = 0.25)
        } else {
          p <- p +
            geom_line() +
            geom_ribbon(mapping = aes(ymin = `q25`, ymax = `q75`), alpha = 0.35, colour = NA) +
            geom_ribbon(mapping = aes(ymin = `q5`, ymax = `q95`), alpha = 0.175, colour = NA)
        }
        p <- p +
          scale_x_date(breaks = breaks[[history]],
                       labels = date_format("%b %Y")) +
          scale_y_continuous(var_names[name], labels = labels[[name]]) +
          scale_colour_brewer(group[level],  palette = "Set1") +
          scale_fill_brewer(group[level],  palette = "Set1") +
          theme_light()
        if (name %in% names(hline)) {
          p <- p +
            geom_hline(yintercept = hline[[name]], linetype = "dashed")
        }
        if (grepl("^variant_", level)) {
          p <- p +
            facet_wrap(~geography)
        }
        if (grepl("cumulative_", name)) {
          p <- p + 
            expand_limits(y = 0)
        }
        ggsave(here::here("figures", "additional",
                          paste0(level, "_", name, "_",
                                 history, ".", filetype)), p,
               width = 8, height = 4)
      }
    }
  }
}
