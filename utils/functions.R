library(dplyr)
library(data.table)
library(XML)
library(httr)
library(RCurl)
library(rlist)
library(lubridate) # cheat sheet: https://rawgit.com/rstudio/cheatsheets/main/lubridate.pdf



#' Download all wiki tables from a given URL
#'
#' @param url string containing the URL from which to download the tables
#'
#' @return a list with tables from the URL
download_tables <- function(url) {
  page_info = GET(url) %>%
    content("text") %>%
    readHTMLTable(doc = .)

  page_info <- lapply(page_info, clean_table)

  return(page_info)
}


# function to clean up entries and remove empty columns
clean_table = function(table){

  table <- table |>
    mutate(across(everything(), trimws)) %>%
    mutate(across(everything(), na_if, y = "")) %>%
    mutate(across(everything(), ~ gsub(x = .x, pattern = "[\r\n]", replacement = " "))) |>
    as.data.table()

  # set first row to column names and remove first row
  colnames(table) <- as.character(table[1, ])
  table <- table[-1, ]

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

  # add current entry
  all_metadata <- all_metadata |>
    rbind(new_metadata)

  fwrite(all_metadata, filelocation)
}



## need to add a function that takes all the csv files and combines them


