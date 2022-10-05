######################################

# Import aggregated, rounded measures data
# Plot them nicely
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

fs::dir_create(here("output", "analysis"))
analysis_dir <- here("output", "analysis")
#analysis_dir <- here("released_output", "analysis")


# Import aggregated measures ----


data_sex <- readtype_csv(file = fs::path(analysis_dir, "rates_sex.csv"))
data_ageband5year <- readtype_csv(file = fs::path(analysis_dir, "rates_ageband5year.csv"))
data_region <- readtype_csv(file = fs::path(analysis_dir, "rates_region.csv"))
data_all <- readtype_csv(file = fs::path(analysis_dir, "rates_all.csv"))


# reshape to long format ----

long <- function(data){
  data %>%
    pivot_longer(
      cols=c("rate_unweighted","rate_weighted"),
      names_to="weighted",
      values_to="rate"
    ) %>%
    mutate(
      weighted = case_when(
        weighted=="rate_unweighted" ~ "Unweighted",
        weighted=="rate_weighted" ~ "Weighted",
        TRUE ~ NA_character_
      )
    )
}

data_long_sex <- long(data_sex)
data_long_ageband5year <- long(data_ageband5year)
data_long_region <- long(data_region)
data_long_all <- long(data_all)



# plot TPP estimates of infection, etc ----

plot_measures <- function(data_long, group, period){

  period0 <- period

  data_long %>%
    filter(period==period0) %>%
    mutate(
      measure_descr = fct_relabel(measure_descr, str_wrap, width=15)
    ) %>%
    ggplot() +
    geom_hline(aes(yintercept=0), colour="black")+
    geom_line(aes(x=date, y=rate, colour={{group}}, linetype=weighted))+
    facet_grid(rows=vars(measure_descr), scales = "free_y")+
    scale_x_date(
      ##    limits = c(lubridate::floor_date(xmin, "1 month"), NA),
      labels = scales::label_date("%b"),
      expand = expansion(add=1),
      sec.axis = sec_axis(
        trans = ~as.Date(.),
        breaks = as.Date(seq(floor_date(study_dates$start_date, "year"), ceiling_date(study_dates$end_date, "year"), by="year")),
        labels = scales::label_date("%Y")
      )
    )+
    scale_y_continuous(
      #limits=c(0,1),
      labels=scales::label_number(scale=1000)
    )+
    labs(
      x="Date",
      y="Rate per 1,000 people",
      colour=NULL,
      linetype=NULL
    )+
    theme_minimal()+
    theme(
      legend.position="bottom",
      axis.ticks.x = element_line(),
      axis.text.x = element_text(hjust=0),
      strip.text.y = element_text(angle=0)
    )

}


plot_measures(data_long_sex, sex, "01")
plot_measures(data_long_sex, sex, "14")
plot_measures(data_long_sex, sex, "ever")



