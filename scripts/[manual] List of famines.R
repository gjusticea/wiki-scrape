source("utils/functions.R")

url = "https://ourworldindata.org/famines#famines-by-world-region-since-1860"
ref = "Joe Hasell and Max Roser (2013) - 'Famines'. Published online at OurWorldInData.org. Retrieved from: 'https://ourworldindata.org/famines' [Online Resource]"
cat_name = "List of famines"
cat_id = "G66"

# Read in tables and get suggested tables for cleaning
tables = download_tables(url,rm_first = FALSE)
suggested_tables = suggest_tables_to_keep(tables)

# Do the cleaning
table = tables$`tablepress-73` %>%
  select(-Source) %>%
  mutate(across(c(`Excess Mortality midpoint`,`Excess Mortality lower`,
                  `Excess Mortality upper`),
                function(x) gsub(",","",x)),
         Year = gsub("â\u0080\u0093","-",Year)) %>%
  separate_wider_delim(cols = Year, delim = "-",
                       names = c("year_start","year_end"),
                       too_few = "align_start") %>%
  group_by_all() %>%
  mutate(year_start = substr(year_start,1,4),
         year_end = paste0(substr(year_start,1,4-nchar(year_end)),year_end),
         year_end = ifelse(year_end == "NANA",year_start,year_end)) %>%

  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = paste0(Country,": ",year_start,"-",year_end),
         `Event description` = Event,
         `Timepoint start` = year_start,
         `Timepoint end` = year_end,
         `Quantity outcome 1` = `Excess Mortality midpoint`,
         `Reference/link to data` = ref,
         `Accessed on` = Sys.Date()) %>%
  ungroup()

# add country info
table %>%
  get_country_raw_info()

table %<>%
select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of famines.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of famines since 1846",
  "Description quantity column 1" = "Excess mortality midpoint",
  "Period start" = "1846",
  "Period end" = "2016",
  "How was the period selected" = "Availability of high-quality data via OWID",
  "Collected by" = "Our World in Data"
)

update_category_info_sheet(metadata)
