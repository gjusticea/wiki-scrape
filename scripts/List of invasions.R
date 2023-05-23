source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_invasions"
cat_name = "List of invasions"
cat_id = "G71"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning
# Remove some duplicates for american civil war
# Extract year from title
excl = fread("ref/List of invasions excl.csv") %>%
  select(Invasion)

table = do.call("rbind",tables[suggested_tables$`max cols`]) %>%
  clean_table(remove_first = FALSE) %>%
  filter(grepl("\\d{4}",Invasion)) %>%
  mutate(year = str_extract(Invasion,"\\d{4}")) %>%
  filter(year >= 1800) %>%
  anti_join(excl)

# put into uniform format
table = table %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = Invasion,
         `Event description` = paste0("Invading forces: ",`Invading forces`,
                                      "; Defending forces: ",`Defending forces`),
         `Timepoint start` = year,
         `Timepoint end` = NA,
         `Quantity outcome 1` = NA,
         `Reference/link to data` = url,
         `Accessed on` = Sys.Date()
  ) %>%
  select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)


# Write to outputs folder
fwrite(table,"output/list of invasions.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = "List of invasions",
  "Description" = "Military invasions of sovereign states, including of colonies, excluding civil wars and invasions of/by rebel groups, breakaway states, or provisional governments",
  "Description quantity column 1" = "",
  "Period start" = "",
  "Period end" = "present",
  "How was the period selected" = "",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)
