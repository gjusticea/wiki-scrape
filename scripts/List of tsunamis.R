source("utils/functions.R")

# Citation: DOI:10.7289/V5PN93H7
# Update reference file from https://www.ngdc.noaa.gov/hazel/view/hazards/tsunami/event-data?maxYear=2023&minYear=1950
path = "ref/tsunamis-2023-02-02_21-31-04_-0500.tsv"
cat_name = "List of tsunamis"
update_date = as.Date("2023-02-02")

# Do the cleaning
table = fread(path) %>%
  .[c(2:nrow(.))] %>%
  mutate(across(c(Mo,Dy,Hr,Mn,Sec),formatC, width=2, flag="0"),
         Category = cat_name,
         Event = unlist(mapply(FUN = paste0,Country,"-",`Location Name`,": ",
                               Dy,"/",Mo,"/",Year," ",Hr,":",Mn,":",Sec)),
         `Timepoint start` = unlist(mapply(FUN = paste0,Year,"-",Mo,"-",Dy)),
         `Timepoint start` = as.Date(`Timepoint start`),
         `Timepoint end` = `Timepoint start`,
         `Event description` = paste0("Earthquake mag: ",`Earthquake Magnitude`,"; ",
                                      "Max water height (m): ",`Maximum Water Height (m)`,"; ",
                                      "Magnitude (Iida): ",`Tsunami Magnitude (Iida)`,
                                      "Deaths: ",Deaths),
         `Quantity outcome 1` = `Number of Runups`,
         `Reference/link to data` = "DOI:10.7289/V5PN93H7",
         `Accessed on` = update_date) %>%

  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of tsunamis.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "List of tsunamis since 1950, worldwide",
  "Description quantity column 1" = "Number of runups",
  "Period start" = "1950-01-01",
  "Period end" = as.character(update_date),
  "How was the period selected" = "Post-WWII when global coverage and recording became more reliable",
  "Collected by" = "NOAA"
)

update_category_info_sheet(metadata)
