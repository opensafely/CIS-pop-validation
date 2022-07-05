library('tidyverse')
library('yaml')
library('here')
library('glue')

# create action functions ----

## create comment function ----
comment <- function(...){
  list_comments <- list(...)
  comments <- map(list_comments, ~paste0("## ", ., " ##"))
  comments
}


## create function to convert comment "actions" in a yaml string into proper comments
convert_comment_actions <-function(yaml.txt){
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1")  %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}


## generic action function ----
action <- function(
  name,
  run,
  arguments=NULL,
  needs=NULL,
  highly_sensitive=NULL,
  moderately_sensitive=NULL,
  ... # other arguments / options for special action types
){

  outputs <- list(
    highly_sensitive = highly_sensitive,
    moderately_sensitive = moderately_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  action <- list(
    run = paste(c(run, arguments), collapse=" "),
    needs = needs,
    outputs = outputs,
    ... = ...
  )
  action[sapply(action, is.null)] <- NULL

  action_list <- list(name = action)
  names(action_list) <- name

  action_list
}


# specify project ----

## defaults ----
defaults_list <- lst(
  version = "3.0",
  expectations= lst(population_size=50000L)
)


source(here("analysis", "lib", "design.R"))


extract_dates <- seq(as.Date(study_dates$start_date), as.Date(study_dates$end_date), by=56)
extract_action_names <- glue("extract_data_{format(extract_dates, '%Y%m%d')}")

extract_action <- function(dates){

  dates_ISO8601 <- format(dates, "%Y-%m-%d")
  dates_strip <- format(dates, "%Y%m%d")

  actions_list <- map2(
    dates_ISO8601,
    dates_strip,
    ~action(
      name = glue("extract_data_{.y}"),
      run = glue("cohortextractor:latest generate_cohort --study-definition study_definition --index-date-range '{.x} to {.x} by week' --output-dir 'output/measures' --output-format feather"),
      needs = list(),
      highly_sensitive = lst(
        cohort = glue("output/measures/input_{.x}.feather")
      )
    )
  )


  set_names( # this is silly but I don't know any better
    map(actions_list, ~.x[[1]]),
    map(actions_list, ~names(.x[1]))
  )

}



dates_ISO8601 <- format(extract_dates, "%Y-%m-%d")
dates_strip <- format(extract_dates, "%Y%m%d")

actions_list <- map2(
  dates_ISO8601,
  dates_strip,
  ~action(
    name = glue("extract_data_{.y}"),
    run = glue("cohortextractor:latest generate_cohort --study-definition study_definition --index-date-range '{.x} to {.x} by week' --output-dir 'output/measures' --output-format feather"),
    needs = list(),
    highly_sensitive = lst(
      cohort = glue("output/measures/input_{.y}.feather")
    )
  )
)
## actions ----
actions_list <- splice(

  comment("# # # # # # # # # # # # # # # # # # #",
          "DO NOT EDIT project.yaml DIRECTLY",
          "This file is created by create-project.R",
          "Edit and run create-project.R to update the project.yaml",
          "# # # # # # # # # # # # # # # # # # #"
          ),



  comment("# # # # # # # # # # # # # # # # # # #", "Extract data and create measures", "# # # # # # # # # # # # # # # # # # #"),

  # action(
  #   name = "extract_data",
  #   run = 'cohortextractor:latest generate_cohort --study-definition study_definition --index-date-range "2020-04-27 to 2022-04-27 by 4 weeks" --output-format feather',
  #   needs = list(),
  #   highly_sensitive = lst(
  #     cohort = "output/input.feather"
  #   )
  # ),

  extract_action(extract_dates),

  action(
    name = "measures",
    run = "cohortextractor:latest generate_measures --study-definition study_definition --output-dir=output/measures",
    needs = as.list(extract_action_names),
    moderately_sensitive = lst(
      cohort = "output/measures/measure_*.csv"
    )
  ),


  comment("# # # # # # # # # # # # # # # # # # #", "Calculate aggregated measures data", "# # # # # # # # # # # # # # # # # # #"),

  action(
    name = "aggregate",
    run = "r:latest analysis/aggregate.R",
    needs = list("measures"),
    moderately_sensitive = lst(
      csv = "output/analysis/*.csv",
      json = "output/analysis/*.json"
    )
  ),


comment("# # # # # # # # # # # # # # # # # # #", "End", "# # # # # # # # # # # # # # # # # # #")

)

project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

## convert list to yaml, reformat comments and whitespace ----
thisproject <- as.yaml(project_list, indent=2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1")


# if running via opensafely, check that the project on disk is the same as the project created here:
if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("expectations", "tpp")){

  thisprojectsplit <- str_split(thisproject, "\n")
  currentproject <- readLines(here("project.yaml"))

  stopifnot("project.yaml is not up-to-date with create-project.R.  Run create-project.R before running further actions." = identical(thisprojectsplit, currentproject))

# if running manually, output new project as normal
} else if (Sys.getenv("OPENSAFELY_BACKEND") %in% c("")){

## output to file ----
  writeLines(thisproject, here("project.yaml"))
#yaml::write_yaml(project_list, file =here("project.yaml"))

## grab all action names and send to a txt file

names(actions_list) %>% tibble(action=.) %>%
  mutate(
    model = action==""  & lag(action!="", 1, TRUE),
    model_number = cumsum(model),
  ) %>%
  group_by(model_number) %>%
  summarise(
    sets = str_trim(paste(action, collapse=" "))
  ) %>% pull(sets) %>%
  paste(collapse="\n") %>%
  writeLines(here("actions.txt"))

# fail if backend not recognised
} else {
  stop("Backend not recognised")
}

