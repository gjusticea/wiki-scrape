source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_structural_failures_and_collapses"
cat_name = "Structural failures and collapses"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning
table = do.call(rbind,tables[which(suggested_tables$all$ncols == 5)]) %>%
  mutate(Year = as.numeric(Year),
         Casualties = gsub(",","",Casualties),
         Casualties2 = gsub("dead|killed(.*)+injured","dead",Casualties),
         deaths = str_match(Casualties2,"[0-9]{4} dead|[0-9]{3} dead|[0-9]{2} dead|[0-9]{1} dead")) %>%
  filter(Year > 1950)
fwrite(table,file="ref/structural failures tmp.csv")
table = fread("ref/structural failures.csv") %>%
  mutate(Category = cat_name,
         Event = Structure,
         `Event description` = paste(Type,Location,sep=" - "),
         `Timepoint start` = as.Date(paste0(Year,"-01-01")),
         `Timepoint end` = as.Date(paste0(Year,"-12-31")),
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = url,
         `Accessed on` = as.Date("2023-03-20")) %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of structural failures.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "List of structural failures and collapses since 1950",
  "Description quantity column 1" = "Deaths or missing (if a range is given, center is chosen)",
  "Period start" = "1950",
  "Period end" = "present",
  "How was the period selected" = "Somewhat arbitrarily. Entries to go 226BC, but I have concerns about historic data quality the further back one goes",
  "Collected by" = "Greg"
)

update_category_info_sheet(metadata)
