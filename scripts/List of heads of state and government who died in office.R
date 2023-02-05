source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_heads_of_state_and_government_who_died_in_office"
cat_name = "List of heads of state and government who died in office"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning
table = do.call(rbind,tables[c(5,6)]) %>%
  mutate(Category = cat_name,
         Event = Name,
         `Event description` = unlist(mapply(FUN = paste0, Title, Country,
                                             `Cause of death`, `Place of Death`,
                                             sep=" - ")),
         `Timepoint start` = Year,
         `Timepoint end` = Year,
         `Quantity outcome 1` = NA,
         `Reference/link to data` = url,
         `Accessed on` = Sys.Date()) %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of deaths of heads of state.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "Deaths of heads of state while in office, from all causes",
  "Description quantity column 1" = "",
  "Period start" = "1950",
  "Period end" = "present",
  "How was the period selected" = "Post-war period when the modern world order largely took form",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)
