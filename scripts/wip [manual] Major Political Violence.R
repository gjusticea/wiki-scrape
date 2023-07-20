source("utils/functions.R")

url = "https://www.start.umd.edu/gtd/"
ref = "START (National Consortium for the Study of Terrorism and Responses to Terrorism). (2022). Global Terrorism Database, 1970 - 2020 [data file]. https://www.start.umd.edu/gtd"

cat_name = "Terrorist attacks"
cat_id = "G83"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning


select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,file = "output/")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "",
  "Description quantity column 1" = "",
  "Period start" = "",
  "Period end" = "present",
  "How was the period selected" = "",
  "Collected by" = ""
)

update_category_info_sheet(metadata)
