source("utils/functions.R")

url = ""
ref = ""

cat_name = ""
cat_id = ""

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning


select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
writexl::write_xlsx(table,path = "output/")

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
