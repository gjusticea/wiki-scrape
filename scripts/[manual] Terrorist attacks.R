source("utils/functions.R")

url = "https://www.start.umd.edu/gtd/"
ref = "START (National Consortium for the Study of Terrorism and Responses to Terrorism). (2022). Global Terrorism Database, 1970 - 2020 [data file]. https://www.start.umd.edu/gtd"

cat_name = "Terrorist attacks"
cat_id = "G84"

# Read in tables and do the cleaning
table_raw = readxl::read_xlsx("ref/globalterrorismdb_0522dist.xlsx")
table = table_raw %>%
  filter(nkill >= 20) %>%
  select(iyear, imonth, iday, country_txt, city, attacktype1_txt, nkill) %>%
  mutate(dte = as.Date(paste0(iyear,"-",imonth,"-",iday))) %>%
  filter(!is.na(dte)) %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = paste0(country_txt," - ",dte),
         `Event description` = attacktype1_txt,
         `Timepoint start` = dte,
         `Timepoint end` = dte,
         `Quantity outcome 1` = nkill,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-07-04")) %>%

select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,file = "output/list of deadly terrorist attacks.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of terrorist attacks with 20 or more fatalities",
  "Description quantity column 1" = "# of fatalities",
  "Period start" = "1/1/1970",
  "Period end" = "12/31/2020",
  "How was the period selected" = "Full range available from data source",
  "Collected by" = "START (National Consortium for the Study of Terrorism and Responses to Terrorism)"
)

update_category_info_sheet(metadata)
