source("utils/functions.R")
library(xml2)

url = "https://www.un.org/en/about-us/member-states"
cat_name = "UN Member States"

## USE A NEW SOURCE: https://www.un.org/en/about-us/growth-in-un-membership

# Read in tables and get suggested tables for cleaning
table = fread("ref/countries_raw.csv") %>%
  pivot_wider(names_from = type, values_from = dat) %>%
  mutate(dte1 = as.Date(Date,format = "%d/%m/%Y"),
         dte2 = as.Date(Date,format = "%d-%m-%Y"),
         Date = case_when(
           is.na(dte1) ~ dte2,
           is.na(dte2) ~ dte1
         )) %>%
  select(-group, -dte1, -dte2)

table2 = data.frame(Year = c(1960:2022),
                    Countries = NA)
for(i in 1:nrow(table2)){
  yr = table2[i,"Year"]
  table2[i,"Countries"] = length(which(year(table$Date) <= yr))
}
table2 = table2 %>%
  mutate(Category = cat_name,
         Event = Year,
         `Event description` = unlist(mapply(FUN = paste0,"Member states: ",Year)),
         `Timepoint start` = Year,
         `Timepoint end` = Year,
         `Quantity outcome 1` = Countries,
         `Reference/link to data` = url,
         `Accessed on` = as.Date("2023-02-04")) %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/un member states.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "Number of UN member states by year",
  "Description quantity column 1" = "Number of states",
  "Period start" = "1960",
  "Period end" = "2022",
  "How was the period selected" = "Appears to have taken ~15 years for UN membership to expand to all independent states",
  "Collected by" = "United Nations"
)

update_category_info_sheet(metadata)
