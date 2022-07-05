# functions for savinf csv files but preserving filetypes
# from https://github.com/wjchulme/osutils/blob/main/R/saving.R


writetype_delim <- function(
    x,
    path, ## note OS readr version uses old `path` argument not  `file`, so use `path` for compatibility
    suffix = "",
    delim = " ",
    na = "NA",
    quote_escape = "double"#,
    #eol = "\n"
){


  concise_lookup <- tibble::tribble(
    ~concise, ~col_type, ~class, ~type,

    "c", "character", "character", "character",
    "f", "factor", "factor", "integer",
    "d", "double", "numeric", "double",
    "i", "integer", "integer", "integer",
    "l", "logical", "logical", "logical",
    #"n",  "number", NA, NA
    "D", "date", "Date", "double",
    #"T", "datetime", NA, "double"
    #"t", "time", NA, "double"
  )

  x_type <-
    tibble::tibble(
      col_name = names(x),
      class = purrr::map_chr(x, class),
      type = purrr::map_chr(x, typeof),
      attributes = purrr::map(x, attributes),
      levels = purrr::map(x, ~ levels(.) ),
      concise = concise_lookup$concise[match(class, concise_lookup$class)],
      col_type = concise_lookup$col_type[match(class, concise_lookup$class)]
    )

  jsonpath <- paste0(fs::path_ext_remove(path), suffix, ".json")

  jsonlite::write_json(x_type, path=jsonpath, pretty=TRUE)


  readr::write_delim(
    x=x,
    path=path,
    delim=delim,
    na=na,
    append=FALSE,
    col_names=TRUE,
    quote_escape=quote_escape#,
    #eol=eol
  )

}




writetype_csv <- function(
    x,
    path, ## note OS readr version uses old `path` argument not  `file`, so use `path` for compatibility
    suffix = "",
    na = "NA",
    quote_escape = "double"#,
    #eol = "\n"
){

  writetype_delim(
    x=x,
    path=path, ## note OS readr version uses old `path` argument not  `file`, so use `path` for compatibility
    suffix=suffix,
    delim=",",
    na=na,
    quote_escape=quote_escape#,
    #eol=eol
  )

}




readtype_delim <- function(
    file,
    suffix = "",
    delim,
    quote = "\"",
    escape_backslash = FALSE,
    escape_double = TRUE,
    locale = default_locale(),
    na = c("", "NA"),
    quoted_na = TRUE,
    comment = "",
    trim_ws = FALSE
){

  jsonpath <- paste0(fs::path_ext_remove(file), suffix, ".json")

  x_type <- jsonlite::read_json(jsonpath) %>%
    tibble::enframe(name=NULL) %>%
    tidyr::unnest_wider(value) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      #col_spec = list(get(paste0("col_",col_type))),
      levels = list(unlist(levels)) # to change from list of lists to list of character vectors
    )

  x <- readr::read_delim(
    file = file,
    delim = delim,
    col_types = paste(x_type$concise, collapse=""),
    quote = quote,
    escape_backslash = escape_backslash,
    escape_double = escape_double,
    locale = locale,
    na = na,
    quoted_na = quoted_na,
    comment = comment,
    trim_ws = trim_ws
  )

  factors <- dplyr::filter(x_type, class=="factor")$col_name

  for (fc in factors) {
    levels <- dplyr::filter(x_type, col_name==fc)$levels[[1]]
    x[[fc]] <- factor(x[[fc]], levels = levels)
  }

  x

}

readtype_csv <- function(
    file,
    suffix = "",
    delim,
    quote = "\"",
    escape_backslash = FALSE,
    escape_double = TRUE,
    locale = default_locale(),
    na = c("", "NA"),
    quoted_na = TRUE,
    comment = "",
    trim_ws = FALSE
){
  readtype_delim(
    file = file,
    suffix = suffix,
    delim = ",",
    quote = quote,
    escape_backslash = escape_backslash,
    escape_double = escape_double,
    locale = locale,
    na = na,
    quoted_na = quoted_na,
    comment = comment,
    trim_ws = trim_ws
  )
}
