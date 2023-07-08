source("utils/functions.R")

url = "https://www.unhcr.org/refugee-statistics/download/?url=1QwB9G"
ref = "Internal Displacement Monitoring Centre. Refugee Data Finder - Internally Displaced Persons. https://www.unhcr.org/refugee-statistics/download/?url=1QwB9G."

cat_name = "Internally displaced persons"
cat_id = "G83"

# Read in tables and do the cleaning
table = fread("ref/idmc internally displaced persons.csv") %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = paste0("Internally displaced persons in ",Year),
         `Event description` = Event,
         `Timepoint start` = as.Date(paste0(Year,"-01-01")),
         `Timepoint end` = as.Date(paste0(Year,"-12-31")),
         `Quantity outcome 1` = Total,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-07-02")) %>%

select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,file = "output/list of internally displaced populations.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "# of internally displaced persons each year",
  "Description quantity column 1" = "# of internally displaced persons",
  "Period start" = "1989",
  "Period end" = "present",
  "How was the period selected" = "Max range available via UNHCR",
  "Collected by" = "IDMC"
)

update_category_info_sheet(metadata)
