source("utils/functions.R")

url = "https://www.prio.org/journals/jpr/replicationdata"
ref = "Powell, Jonathan M., and Clayton L. Thyne. “Global Instances of Coups from 1950 to 2010: A New Dataset.” Journal of Peace Research, vol. 48, no. 2, Mar. 2011, pp. 249–59. DOI.org (Crossref), https://doi.org/10.1177/0022343310397436."

cat_name = "Coup attempts"
cat_id = "G81"

# Read in tables and do the cleaning
table = fread("ref/coup attempts files/powell_thyne_coups_final.txt") %>%
  mutate(successful = coup == 2,
         date = as.Date(unlist(mapply(FUN = paste,year,
                                      str_pad(month,2,"left",0),day,
                                      sep="-")))) %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = unlist(mapply(FUN = paste0,country," ",date)),
         `Event description` = case_when(
           successful == TRUE ~ paste0("Successful coup in ",country),
           successful == FALSE ~ paste0("Unsuccessful coup in ",country)
         ),
         `Timepoint start` = date,
         `Timepoint end` = date,
         `Quantity outcome 1` = as.numeric(successful),
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-06-24"))

# get country info
table %>%
  rename(Country = country) %>%
  get_country_raw_info()

table %<>%
select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,file = "output/list of coup attempts.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of coup attempts from 1950 to 2010",
  "Description quantity column 1" = "Whether the coup was successful (1 = successful, 0 = unsuccessful)",
  "Period start" = "1950",
  "Period end" = "2010",
  "How was the period selected" = "Data availability from source",
  "Collected by" = ref
)

update_category_info_sheet(metadata)
