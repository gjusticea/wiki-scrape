library(dplyr)
library(data.table)
library(XML)
library(httr)
library(RCurl)
library(rlist)
library(lubridate) # cheat sheet: https://rawgit.com/rstudio/cheatsheets/main/lubridate.pdf
library(tidyr)
library(stringr)
library(magrittr)


#' Download all wiki tables from a given URL
#'
#' @param url string containing the URL from which to download the tables
#'
#' @return a list with tables from the URL
download_tables <- function(url,rm_first = TRUE) {
  page_info = GET(url) %>%
    content("text") %>%
    readHTMLTable(doc = .)

  page_info <- lapply(page_info, clean_table, remove_first = rm_first)

  return(page_info)
}


# function to clean up entries and remove empty columns
clean_table = function(table, remove_first = TRUE){

  table <- table |>
    mutate(across(everything(), na_if, y = "")) %>%
    mutate(across(everything(), ~ gsub(x = .x, pattern = "[\r\n]", replacement = " "))) |>
    mutate(across(everything(), ~ gsub(x = .x, pattern = "\\p{Pd}", replacement = "-", perl=TRUE))) |>
    mutate(across(everything(), ~ gsub(x = .x, pattern = "\\[[0-9]+\\]", replacement = ""))) |>
    # to remove daggers and crosses
    # mutate(across(everything(), ~ gsub(x = .x, pattern = "[\x{2020}\x{2021}\x{271D}]", replacement = " "))) |>
    mutate(across(everything(), trimws)) %>%
    mutate(across(everything(), str_squish)) |>
    as.data.table()

  # set first row to column names and remove first row
  if(remove_first == TRUE){
    colnames(table) <- as.character(table[1, ])
    table <- table[-1, ]
  }

  return(table)
}


#' Try to automatically guess which tables to keep from a Wikipedia page
#'
#' It uses a few heuristics:
#' a) if some tables are named, then only keep the named ones
#' b) only keep tables that have more than one row / column
#' c) only keep the tables that have the maximum number of columns
#' d) check the number of cols of the table with the maximum number of rows
#'    and keep all tables that have that number of columns
#'
#' @param tables_list A list of tables as produced by [download_tables()}
#'
#' @return a list with three elements: A vector with the index of all named
#' tables, a vector with the index of all tables that have more than one col and
#' row and a vector with the index of all tables that have the maximum numbers
#' of columns found on that page
suggest_tables_to_keep <- function(tables_list) {

  # extract dimensions of all the tables
  dims <- lapply(tables_list, function(t) dim(t))

  meta_data <- data.table(
    index = 1:length(tables_list),
    name = names(tables_list),
    nrows = lapply(dims, function(dim) dim[1]) |> unlist(),
    ncols = lapply(dims, function(dim) dim[2]) |> unlist()
  )

  named_elements <- meta_data |>
    filter(!(name %in% c("NULL", "\n"))) |>
    pull(index)

  enough_dims <- meta_data |>
    filter(nrows > 1 & ncols > 1) |>
    pull(index)

  highest_cols <- meta_data |>
    filter(ncols == max(ncols)) |>
    pull(index)

  cols_max_row <- meta_data |>
    filter(nrows == max(nrows)) |>
    pull(ncols) |>
    unique() # this could in principle also be more than one if we're unlucky

  cols_highest_rows <- meta_data |>
    filter(ncols %in% cols_max_row) |>
    pull(index)

  out <- list(
    "named tables" = named_elements,
    "dims ok" = enough_dims,
    "max cols" = highest_cols,
    "cols max row" = cols_highest_rows,
    "all" = meta_data
  )
  return(out)
}


# Helper function that reads in the current meta data file and updates it with
# the current entry, then writes everything back
update_category_info_sheet <- function(new_metadata) {

  filelocation <- "output/category-metadata-info.csv"

  all_metadata <- fread(filelocation)

  # remove current entry if it is present
  current_category_name <- new_metadata$`Category name`
  all_metadata <- all_metadata |>
    filter(`Category name` != current_category_name)

  current_category_id = new_metadata$`Category ID`

  if(current_category_id %in% all_metadata$`Category ID`){
    stop(paste0("ID ",current_category_id," already in use"))
  }

  # add current entry
  all_metadata <- all_metadata |>
    list(new_metadata) %>%
    rbindlist(fill = TRUE)

  fwrite(all_metadata, filelocation)
}


# helper function for the repetetive task of adding a url and date column
# and for selecting only the columns that are needed in the end
add_and_keep_relevant_cols <- function(data, ref = url, access_date = Sys.Date()) {
  cols_to_keep <- c("Category ID", "Category", "Event", "Event description", "Location", "Timepoint start",
                    "Timepoint end", "subj. confidence", "Binary outcome", "Quantity outcome 1",
                    "Reference/link to data", "Accessed on", "Comment")

  data |>
    mutate(`Reference/link to data` = ref,
           `Accessed on` = access_date) |>
    select(one_of(cols_to_keep))
}

# parse a date using a specified parsing function. If that fails, just return
# the original input
parse_date <- function(date, parsing_function) {
  parsed_dates <- parsing_function(date)

  out <- ifelse(!is.na(parsed_dates),
                as.character(parsed_dates),
                date)
  return(out)
}


# try different date parsing functions until one of them sticks
# cheat sheet: https://rawgit.com/rstudio/cheatsheets/main/lubridate.pdf
try_to_parse_date <- function(date) {
  parsing_functions <- c(
    lubridate::ymd,
    lubridate::mdy,
    lubridate::dmy #, could add lubridate::my, but this may be dangerous as it replaces the month with an exact date
  )

  for (fun in parsing_functions) {
    date <- suppressWarnings(
      parse_date(date, fun)
    )
  }
  return(date)
}



# output rows with any info relevant to country tagging
get_country_raw_info = function(tbl){
  cat_name = unique(tbl$`Category ID`)
  tbl %>%
    select(`Category ID`,`Category`,Event,`Event description`,Country) %>%
    fwrite(paste0("ref/country key prep/",cat_name," by country.csv"))
}

