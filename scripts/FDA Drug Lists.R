library(tidyverse)
library(data.table)
library(writexl)
library(lubridate)

source("utils/functions.R")

# Download data here: https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files
url = "https://www.fda.gov/drugs/drug-approvals-and-databases/drugsfda-data-files"
refresh_date = as.Date("2023-02-11")
data_dir = "ref/drugsatfda20230207/"

# Data layout:
# A drug refers to a pharmaceutical product (not active ingredient or molecule)
# Each drug has an application number, which is based on its NDA
# Each drug (application) can include multiple products, i.e. different dosages
# Each drug (application) can also have multiple submissions, i.e. new dosage or formula
# Each product in an application has a current marketing status
# NDA is submitted AFTER clinical trials, an IND app is submitted before trials

# All NDA/ANDA/BLA application numbers, and NDA/ANDA/BLA status
# Appl_No A = ANDA (generics), N = NDA
# (is a list of application numbers)
# apps = read.delim(paste0(data_dir,"Applications.txt"))

# All submissions
# ORIG type submissions usually lack a submission class code
# All submissions are approved or temporarily approved
# Not all submissions map to an application number, unpaired are dropped
# These unpaired applications don't show up on Drugs@FDA or the products list
#   (with the exception of 200171 methotrexate, which appears to be an error)
#   (this drops 1102 records)
subs = read.delim(paste0(data_dir,"Submissions.txt")) %>%
  filter(SubmissionStatus == "AP",                            # Filter to approved submissions
         SubmissionClassCodeID %in% c(7,8)) %>%               # Filter to new molecular entity submissions
  select(ApplNo, SubmissionClassCodeID, SubmissionNo, SubmissionStatusDate) %>%
  group_by(ApplNo) %>%
  filter(SubmissionStatusDate == min(SubmissionStatusDate),
         ApplNo != "060904") %>%                              # remove known error case
  filter(SubmissionNo == min(SubmissionNo)) %>%               # Filter to the earliest approved submission
  merge(read.delim(paste0(data_dir,"SubmissionClass_Lookup.txt")) %>%
                     select(SubmissionClassCodeID, SubmissionClassCode,
                            SubmissionClassCodeDescription),  # Label submission type
        by = "SubmissionClassCodeID", all.x = TRUE) %>%
  merge(read.delim(paste0(data_dir,"Applications.txt")) %>%
          filter(ApplType %in% c("NDA","BLA")) %>%            # Filter out generic applications (ANDAs)
          mutate(ApplNo = str_pad(ApplNo, 6, pad = "0")) %>%
          select(ApplNo, ApplType),
        by = "ApplNo") %>%
  mutate(SubmissionStatusDate = as.Date(SubmissionStatusDate)) %>%
  merge(read.delim(paste0(data_dir,"Products.txt")) %>%       # Get active ingredient names
          select(ApplNo,ActiveIngredient) %>%
          mutate(ApplNo = str_pad(ApplNo, 6, pad = "0")) %>%
          distinct() %>%
          group_by(ApplNo) %>%
          summarize(ActiveIngredient = paste0(unique(ActiveIngredient),
                                              collapse=";; ")),
        by = "ApplNo")

# get a clean list
# some drugs have two NDAs as well, this will help clean
cat_name = "List of FDA Approved Drugs"

table_all_drugs = subs %>%
  group_by(ActiveIngredient, ApplType) %>%
  summarize(Approval_Date = min(SubmissionStatusDate)) %>%
  filter(year(Approval_Date) >= 1998) %>%
  mutate(Category = cat_name,
         Event = ActiveIngredient,
         `Event description` = unlist(mapply(FUN = paste0, ActiveIngredient, " - ", ApplType)),
         `Timepoint start` = as_date(Approval_Date),
         `Timepoint end` = as_date(Approval_Date),
         `Quantity outcome 1` = NA,
         `Reference/link to data` = url,
         `Accessed on` = refresh_date) %>%
  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)


# Number of approvals - done here

fwrite(table_all_drugs,"output/fda approved drugs.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "List of approved NDAs and BLAs for new molecular entities with the US FDA since 1998",
  "Description quantity column 1" = "",
  "Period start" = "1998",
  "Period end" = "present",
  "How was the period selected" = "Website: 'Drugs@FDA includes most of the drug products approved since 1939. The majority of patient information, labels, approval letters, reviews, and other information are available for drug products approved since 1998.'",
  "Collected by" = "FDA"
)

update_category_info_sheet(metadata)



# Number of abx approvals - done already elsewhere
# Number of vax approvals - separate data set
# Number of psych med approvals - need to tag based on drug type

