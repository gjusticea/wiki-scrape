source("utils/functions.R")

url = "https://www.unhcr.org/refugee-statistics/download/"
ref = "UNHCR. Refugee Data Finder. https://www.unhcr.org/refugee-statistics/download/"

cat_name = "International refugee population"
cat_id = "G82"

# Read in tables and do the cleaning
table = fread("ref/unhcr refugees.csv") %>%
  select(Year, unhcr_refugee_count = `Refugees under UNHCR's mandate`) %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = unlist(mapply(FUN = paste0,"# of Refugees in ",Year)),
         `Event description` = Event,
         `Timepoint start` = as.Date(paste0(Year,"-01-01")),
         `Timepoint end` = as.Date(paste0(Year,"-12-31")),
         `Quantity outcome 1` = unhcr_refugee_count,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-07-02")) %>%

select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,file = "output/list of refugee population by year.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "# of refugees under the UNHCR's mandate, explained at https://emergency.unhcr.org/protection/legal-framework/unhcrs-mandate-refugees-stateless-persons-and-idps",
  "Description quantity column 1" = "# of refugees",
  "Period start" = "1951",
  "Period end" = "present",
  "How was the period selected" = "Max range available via UNHCR",
  "Collected by" = "UNHCR"
)

update_category_info_sheet(metadata)
