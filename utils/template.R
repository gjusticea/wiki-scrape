source("utils/functions.R")

url = ""
cat_name = ""

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning




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
