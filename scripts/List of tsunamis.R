source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_tsunamis"
cat_name = "List of tsunamis"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning
table = do.call(rbind,tables[6:10]) %>%
  rename(Name = `Main Article`) %>%
  mutate(Category = cat_name,
         Event = Location,
         `Event description` = Description,
         `Timepoint start` = Year,
         `Timepoint end` = Year,
         `Quantity outcome 1` = NA,
         `Reference/link to data` = url,
         `Accessed on` = Sys.Date()) %>%


select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of tsunamis.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "List of tsunamis since 1900, worldwide",
  "Description quantity column 1" = "",
  "Period start" = "1900",
  "Period end" = "present",
  "How was the period selected" = "Appears from records to be approximately when global coverage and recording became reliable",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)
