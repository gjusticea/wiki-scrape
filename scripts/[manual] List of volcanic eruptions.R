source("utils/functions.R")

url = "https://www.ngdc.noaa.gov/hazel/view/hazards/volcano/event-search/"
cat_name = "List of volcanic eruptions"
cat_id = "G65"

# Read in tables and get suggested tables for cleaning
table = fread("ref/volcano-events-2023-02-27_23-44-20_-0500.tsv")

# Do the cleaning
table = table %>%
  filter(!is.na(Year)) %>%
  # mutate(across(c(Mo,Dy),str_pad,width = 2,side = "left",pad = "0")) %>%
  select(Year, Name, Location, Country, VEI, Deaths) %>%

  group_by_all() %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = paste0(Name,", ",Year),
         `Event description` = paste(Name,Location,Country,Year,sep=" - "),
         `Timepoint start` = Year,
         `Timepoint end` = Year,
         `Quantity outcome 1` = VEI,
         `Reference/link to data` = "National Geophysical Data Center / World Data Service (NGDC/WDS): NCEI/WDS Global Significant Volcanic Eruptions Database. NOAA National Centers for Environmental Information. doi:10.7289/V5JW8BSH",
         `Accessed on` = as.Date("2023-02-27")) %>%
  ungroup() %>%

select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of volcanic eruptions")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of significant volcanic eruptions since 4360 BCE, defined as 'one that meets at least one of the following criteria: caused fatalities, caused moderate damage (approximately $1 million or more), with a Volcanic Explosivity Index (VEI) of 6 or larger, caused a tsunami, or was associated with a major earthquake'",
  "Description quantity column 1" = "Volcanic explosivity index (VEI)",
  "Period start" = "4360 BCE",
  "Period end" = "present",
  "How was the period selected" = "Longest timeframe available via NCEI database",
  "Collected by" = "National Geophysical Data Center / World Data Service (NGDC/WDS): NCEI/WDS Global Significant Volcanic Eruptions Database. NOAA National Centers for Environmental Information. doi:10.7289/V5JW8BSH"
)

update_category_info_sheet(metadata)
