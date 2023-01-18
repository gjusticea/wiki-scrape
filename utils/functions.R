library(dplyr)
library(data.table)
library(XML)
library(httr)
library(RCurl)
library(rlist)


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
#' @param tables_list A list of tables as produced by [download_tables()}
#'
#' @return a list with three elements: A vector with the index of all named
#' tables, a vector with the index of all tables that have more than one col and
#' row and a vector with the index of all tables that have the maximum numbers
#' of columns found on that page
suggest_tables_to_keep <- function(tables_list) {
  # first approach: if any of the tables are named and others are not,
  # then only keep the named ones
  named_elements <- (1:length(tables_list))[
    !(names(tables_list) %in% c("NULL", "\n"))
  ]

  # second approach: check those that have both more than one row and more than
  # one column
  # also find the tables with the highest numbers of columns and keep only those
  dims <- lapply(tables_list, function(t) dim(t))

  max_cols <- lapply(dims, function(dim) dim[2]) |>
    unlist() |>
    max()

  enough_dims <- list()
  highest_dims <- list()
  for (i in 1:length(tables_list)) {
    if (all(dims[[i]] > 1)) {
      enough_dims[[i]] <- i
      if (dims[[i]][2] == max_cols) {
        highest_dims[[i]] <- i
      }
    }
  }
  enough_dims <- unlist(enough_dims)
  highest_dims <- unlist(highest_dims)

  out <- list(
    "named tables" = named_elements,
    "dims ok" = enough_dims,
    "max cols" = highest_dims,
    "dims" = dims
  )
  return(out)
}



## need to add a function that takes all the csv files and combines them


