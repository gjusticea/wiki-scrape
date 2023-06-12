source("utils/functions.R")

url = "https://www.un.org/en/about-us/growth-in-un-membership"
ref = "Nations, United. “Growth in United Nations Membership.” United Nations, https://www.un.org/en/about-us/growth-in-un-membership. Accessed 26 February 2023."
cat_name = "List of UN Member States"
cat_id = "G1"

# Read in tables and get suggested tables for cleaning
table = fread("ref/un member states.csv")

# Do the cleaning

table = table %>%
  group_by_all() %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = Year,
         `Event description` = paste0("Number of UN member states in ",Year),
         `Timepoint start` = as.Date(paste0(Year,"-01-01")),
         `Timepoint end` = as.Date(paste0(Year,"-12-31")),
         `Quantity outcome 1` = `Number of states`,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2022-02-26")) %>%
  ungroup() %>%
select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of un member states.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "Number of UN member states by year, proxy for number of countries by year",
  "Description quantity column 1" = "Number of countries",
  "Period start" = "1964",
  "Period end" = "present",
  "How was the period selected" = "From 1964 onward, country formation and UN membership are consistently within a year of each other, usually the same year",
  "Collected by" = "UN.org"
)

update_category_info_sheet(metadata)
