library(tidyverse)
library(data.table)
library(writexl)
library(lubridate)
library(jsonlite)
library(stringdist)

source("utils/functions.R")

url = "https://datadashboard.fda.gov/ora/cd/recalls.htm"
cat_name = "FDA Drug Recalls"

# Read in tables and get suggested tables for cleaning
recalls = readxl::read_xlsx(paste0(data_dir,"fda recalls dashboard 02202023.xlsx"))

# Do the cleaning


select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "",
  "Category name" = "",
  "Description" = "",
  "Description quantity column 1" = "",
  "Period start" = "",
  "Period end" = "present",
  "How was the period selected" = "",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)
