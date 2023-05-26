library(stringr)
library(tidyverse)
library(here)
library(scales)

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

histories <- c(all = Inf)
breaks <- c(all = "4 months")

# use .csv files and not .rds files
file_pattern <-
  paste0("estimates", "_[^_]+\\.csv")

files <-
  list.files(here("epiforecast-data"),
             pattern = file_pattern,
             full.names = TRUE)
names(files) <- str_extract(files, "(?<=_).*(?=.csv)")

# 5 files: _age, _local, _national, _regional, _variants 
level <- "national"
file = files[[level]]

data <- read_csv(file) %>%
  pivot_longer(matches("^q[0-9]+$"), names_to = "quantile") %>%
  mutate(value = if_else(name == "est_prev", value / 100, value),
         value = if_else(name == "infections", value / population, value)) %>%
  pivot_wider(names_from = "quantile")

cum_file <-
  here::here("epiforecast-data", paste0("cumulative_", sub("^[^_]+_", "", file)))

cum_data <- read_csv(cum_file) %>%
  filter(name %in% names(var_names))

data <- data %>%
  bind_rows(cum_data)

if (!"geography" %in% colnames(data)) { # in all but the _variant file
  data <- data %>%
    mutate(geography = NA_character_,
           variable = as.character(variable))
}

level_data <- data %>%
  filter(level == {{ level }}) %>%
  mutate(variable = if_else(variable == "2-10", "02-10", variable),
         name = if_else(name == "R", "Rt", name)) %>%
  filter(name %in% names(var_names)) 

if (level == "local") {
  level_data <- level_data %>%
    left_join(local_region, by = c("level", "variable")) # adds column
  # region with names for the J* codes in column 'variable'
}

colour_var <- ifelse(level == "local", "region", "variable")
history <- "all"
name <- "infections"
spaghetti <- FALSE



plot_df <- level_data %>%
  filter(name == {{ name }}, 
         date > max(date) - histories[[history]],
         variable == "England")
aes_str <- list(x = "date", colour = colour_var)
aes_str <- c(aes_str, list(y = "q50", fill = colour_var))
p <- ggplot(plot_df, do.call(aes_string, aes_str)) +
  ylab(name) + xlab("")
p <- p +
  geom_line() +
  geom_ribbon(mapping = aes(ymin = `q25`, ymax = `q75`), alpha = 0.35, colour = NA) +
  geom_ribbon(mapping = aes(ymin = `q5`, ymax = `q95`), alpha = 0.175, colour = NA)
p <- p +
  scale_x_date(breaks = breaks[["all"]],
               labels = date_format("%b %Y")) +
  scale_y_continuous(var_names["infections"], labels = labels[["infections"]]) +
  scale_colour_brewer(group["national"],  palette = "Set1") +
  scale_fill_brewer(group["national"],  palette = "Set1") +
  theme_light()
if (grepl("^variant_", level)) {
  p <- p +
    facet_wrap(~geography)
}
if (grepl("cumulative_", name)) {
  p <- p + 
    expand_limits(y = 0)
}
fs::dir_create(here::here("output", "figures", "epiforecast"))
ggsave(here::here("output", "figures", "epiforecast",
                  paste0(level, "_", name, "_",
                         history, ".", "png")), p,
       width = 8, height = 4)
