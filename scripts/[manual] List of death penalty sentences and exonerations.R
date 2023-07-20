source("utils/functions.R")

url = "https://deathpenaltyinfo.org/database/sentences"
ref = "Death Penalty Information Center. Death Penalty Census Database. https://deathpenaltyinfo.org/database/sentences."

cat_name = "US Death Sentences and Exonerations"
cat_id = "G80"

# Do the cleaning
table = fread("ref/death sentences.csv") %>%
  mutate(exonerated = `Outcome of Death Sentence` == "Exonerated") %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = unlist(mapply(FUN = paste,Name,`Multi-Sentence Identifier`)),
         `Event description` = unlist(mapply(FUN = paste0,
                                             "Status: ",`Current Case Status`,
                                             "; Location: ",
                                             `State / Jurisdiction`," - ",
                                             `County / Federal District`,
                                             "; Race: ",Race,"; Gender: ",Gender,";")),
         `Timepoint start` = `Year of Death Sentence`,
         `Timepoint end` = `Timepoint start`,
         `Quantity outcome 1` = as.numeric(exonerated),
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-06-11")) %>%

select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of death sentences and exonerations.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of all state and federal death sentences in the US, with an indicator for whether the person was exonerated",
  "Description quantity column 1" = "Exonerated status (1 = yes, 0 = no)",
  "Period start" = "1972",
  "Period end" = "January 1, 2021",
  "How was the period selected" = "Starting with SCOTUS case Furman v. Georgia, ending with most recent data available via DPIC",
  "Collected by" = "Death Penalty Information Center"
)

update_category_info_sheet(metadata)
