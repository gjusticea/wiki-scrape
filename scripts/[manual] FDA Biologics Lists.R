library(tidyverse)
library(data.table)
library(writexl)
library(lubridate)
library(jsonlite)
library(stringdist)

source("utils/functions.R")

# Download data here: https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files
url = "https://purplebooksearch.fda.gov/downloads"
refresh_date = as.Date("2023-02-19")
data_dir = "ref/"

# Data layout:
# This is purple book data from the FDA, which is specifically biologics applications
# 'Proper name' refers to the type of vaccine
# Multiple products can exist for the same proper name
#   i.e. Comirnaty and Spikevax for proper name "COVID-19 Vaccine, mRNA"
# Each application number refers to a product (i.e Comirnaty)
# Each application/product's first approval is linked to it's "original" submission

# Secondary reference: https://www.fda.gov/vaccines-blood-biologics/vaccines/vaccines-licensed-use-united-states
# Lists some products instead of applications, excludes discontinued/revoked

drugs = fread(paste0(data_dir,"purplebook-search-january-data-download.csv"),
              skip = 3) %>%
  select(Applicant, ApplNo = `BLA Number`,trade_name = `Proprietary Name`,
         proper_name = `Proper Name`, type = `BLA Type`, Strength,
         dosage = `Dosage Form`, route = `Route of Administration`,
         presentation = `Product Presentation`, Status, Licensure,
         approval = `Approval Date`, sub_type = `Submission Type`,
         license = `License Number`, prod_number = `Product Number`) %>%
  filter(ApplNo != "",
         grepl("vaccine",proper_name,ignore.case = TRUE),
         sub_type == "Original") %>%
  mutate(approval = as.Date(approval,format="%m/%d/%Y")) %>%
  select(ApplNo,trade_name,proper_name,approval,Licensure
         #,Status,Licensure
         ) %>%
  distinct() # eliminates multiple


# Write the cleaned up data
cat_name = "List of FDA Approved Vaccines"

vaccines_clean = drugs %>%
  mutate(Category = cat_name,
         Event = trade_name,
         `Event description` = unlist(mapply(FUN = paste0,ApplNo," - ",trade_name," - ",proper_name)),
         `Timepoint start` = as_date(approval),
         `Timepoint end` = as_date(approval),
         `Quantity outcome 1` = NA,
         `Reference/link to data` = url,
         `Accessed on` = refresh_date) %>%
  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

fwrite(vaccines_clean,"output/fda approved vaccines.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "List of approved BLAs for vaccines with the FDA, including licenses since voluntarily revoked",
  "Description quantity column 1" = "",
  "Period start" = "1963",
  "Period end" = "present",
  "How was the period selected" = "Earliest data available in purple book data pull",
  "Collected by" = "FDA"
)

update_category_info_sheet(metadata)
