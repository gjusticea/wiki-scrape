source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_fatal_crowd_crushes"
cat_name = "Crowd crushes"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning
table = do.call(rbind,tables[suggested_tables$`max cols`]) %>%
  rename(deaths_raw = `Est. Deaths`) %>%
  mutate(deaths_raw = gsub(",|\\+","",deaths_raw),
         Date = case_when(
           !is.na(as.Date(Date,format = "%d %B %Y")) ~ as.Date(Date,format = "%d %B %Y"),
           !is.na(as.Date(Date,format = "%d %b %Y")) ~ as.Date(Date,format = "%d %B %Y")
         )) %>%
  separate_wider_delim(cols = deaths_raw, delim="-", too_few = "align_start",
                       names = c("deaths_low","deaths_high")) %>%
  mutate(across(c(deaths_low,deaths_high),as.numeric)) %>%
  group_by(Date, Name, Country, Place, Description) %>%
  summarize(deaths = floor(mean(c(deaths_low,deaths_high),na.rm=TRUE)),
            .groups = "drop") %>%
  filter(Date >= as.Date("1900-01-01")) %>%

  mutate(Category = cat_name,
         Event = unlist(mapply(FUN = paste0,Name," - ",Place,", ",Country)),
         `Event description` = Description,
         `Timepoint start` = Date,
         `Timepoint end` = Date,
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = url,
         `Accessed on` = Sys.Date()) %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/crowd crushes.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "List of fatal crowd crushes",
  "Description quantity column 1" = "Death count (middle point chosen if range)",
  "Period start" = "1900",
  "Period end" = "present",
  "How was the period selected" = "Somewhat arbitrarily, based on concerns about data quality",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)
