source("utils/functions.R")
library(magrittr)

url = "https://deathpenaltyinfo.org/database/executions"
ref = "Death Penalty Information Center. Death Penalty Census Database. 5 June 2023, https://deathpenaltyinfo.org/database/executions."

# For reference, data is available going back further, but using in our database
# would likely violate it's terms of use
url2 = "https://www.icpsr.umich.edu/web/NACJD/studies/8451"

cat_name = "US Executions"
cat_id = "G79"

# Read in table
table = fread("ref/DPIC Execution Database - U.S. Executions.csv")

# Do the cleaning
table %<>%
  mutate(`Execution Date` = as.Date(`Execution Date`,format="%m/%d/%y"),
         `Category ID` = cat_id,
         Category = cat_name,
         Event = trimws(unlist(mapply(FUN = paste0,`Last Name`,",",`First Name`," ",`Middle Name(s)`))),
         `Event description` = unlist(mapply(FUN = paste0,Race," ",Sex,", ",State)),
         `Timepoint start` = `Execution Date`,
         `Timepoint end` = `Execution Date`,
         `Quantity outcome 1` = `Number of Victims`,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-06-05")) %>%

select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of us executions.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of all individuals executed by state or the federal government in the US",
  "Description quantity column 1" = "Number of victims",
  "Period start" = "1977",
  "Period end" = "present",
  "How was the period selected" = "All cases after the SCOTUS decisions of Furman v. Georgia and Gregg v. Georgia, which established modern rules for when the death penalty is and is not constitutional (and substantially decreased its use)",
  "Collected by" = "Death Penalty Information Center (https://deathpenaltyinfo.org/)"
)

update_category_info_sheet(metadata)
