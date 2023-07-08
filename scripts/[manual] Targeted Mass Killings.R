source("utils/functions.R")

url = "https://politicsir.cass.anu.edu.au/form/atrocity-forecasting-project-data-download-form"
ref = "Butcher, Charles, Benjamin E. Goldsmith, Sascha Nanlohy, Arcot Sowmya, and David Muchlinski. 2020. “Introducing the Targeted Mass Killing Dataset for the Study and Forecasting of Mass Atrocities,” Journal of Conflict Resolution [in press]"

cat_name = "Targeted Mass Killings"
cat_id = "G86"

# Read in table and do the cleaning
# Unique on event.name.description / year / actor.name
table = readxl::read_xls("ref/tmk_events_release_1.1.xls") %>%
  filter(tmk == 1) %>%
  select(country, actor.name, event.name.description, year, pl.ccode, tmk.st,
         tmk.end, deaths.est)


select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,file = "output/")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "",
  "Description quantity column 1" = "",
  "Period start" = "",
  "Period end" = "present",
  "How was the period selected" = "",
  "Collected by" = ""
)

update_category_info_sheet(metadata)
