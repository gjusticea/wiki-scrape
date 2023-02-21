source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_deadliest_floods"
cat_name = "List of deadly floods"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning
table = tables[[2]] %>%
  rename(deaths = `Death count`) %>%
  filter(Year >= 1960) %>%
  mutate(deaths_raw = deaths,
         deaths = gsub(",|\\.|\\(.*?\\)|\\[.*?\\]|\\+|up to","",deaths)) %>%
  separate_wider_delim(cols = deaths, delim = "-",
                       names = c("deaths_low","deaths_high"),
                       too_few = "align_start") %>%
  mutate(across(c(deaths_low,deaths_high),as.numeric)) %>%
  filter(deaths_low >= 100)

table_clean = table %>%
  group_by_all() %>%
  mutate(Category = cat_name,
         `Event description` = unlist(mapply(FUN = paste0,
                                             "Year: ",Year,
                                             "; Location: ",Location,
                                             "; Deaths: ",deaths_raw)),
         `Timepoint start` = as.Date(paste0(Year,"-01-01")),
         `Timepoint end` = `Timepoint start`,
         `Quantity outcome 1` = mean(c(deaths_low,deaths_high),na.rm=TRUE),
         `Reference/link to data` = url,
         `Accessed on` = Sys.Date()) %>%
  ungroup() %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of deadly floods.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "Floods since 1960 that have killed at least 100 people",
  "Description quantity column 1" = "Number of deaths (midpoint if a range is given)",
  "Period start" = "1960",
  "Period end" = "present",
  "How was the period selected" = "Estimated approximate start of reliable worldwide reporting",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)
