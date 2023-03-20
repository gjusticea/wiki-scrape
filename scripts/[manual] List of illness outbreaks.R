source("utils/functions.R")

url = "https://wwwn.cdc.gov/norsdashboard/"
cat_name = "List of deadly foodborne or waterborne or enteric disease outbreaks"

# Read in tables and get suggested tables for cleaning
table = readxl::read_xlsx("ref/NationalOutbreakPublicDataTool.xlsx") %>%
  mutate(Month = str_pad(Month,2,side = "left",pad = "0"),
         dte = as.Date(unlist(mapply(FUN = paste0,Year,"-",Month,"-01"))),
         src = case_when(
           `Primary Mode` == "Water" ~ paste0(`Water Exposure`,"-",`Water Type`),
           `Primary Mode` == "Food" ~ paste0(`IFSAC Category`,"-",
                                             `Food Vehicle`,"-",
                                             `Food Contaminated Ingredient`),
           `Primary Mode` == "Animal Contact" ~ paste0(`Animal Type`,"-",
                                                       `Animal Type Specify`)
         ))

# Do the cleaning
table_clean = table %>%
  filter(Deaths > 0) %>%
  mutate(Category = cat_name,
         Event = unlist(mapply(FUN = paste0,
                               `Primary Mode`," - ",
                               State," - ",
                               Year)),
         `Event description` = unlist(mapply(FUN = paste0,
                                             "Etiology: ",Etiology,
                                             "; Setting: ",Setting,
                                             "; Source: ",src)),
         `Timepoint start` = dte,
         `Timepoint end` = dte,
         `Quantity outcome 1` = Deaths,
         `Quantity outcome 2` = Illnesses,
         `Reference/link to data` = url,
         `Accessed on` = as.Date("2023-02-21")) %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Quantity outcome 2`,
       `Reference/link to data`, `Accessed on`)

# Write to outputs folder
fwrite(table_clean,"output/list of deadly food water enteric outbreaks.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "CDC National Outbreak Reporting System reports of foodborne and waterborne disease outbreaks and enteric (intestinal) disease outbreaks spread by contact with environmental sources, infected people or animals, and other means.",
  "Description quantity column 1" = "Deaths",
  "Description quantity column 2" = "Illnesses",
  "Period start" = "1998",
  "Period end" = "2020",
  "How was the period selected" = "Recording for foodborne illnesses started in 1998, most recent data as of 2/21/2023 is through 2020",
  "Collected by" = "CDC"
)

update_category_info_sheet(metadata)
