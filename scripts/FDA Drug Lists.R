library(tidyverse)
library(data.table)
library(writexl)
library(lubridate)
library(jsonlite)
library(stringdist)

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

# Read in and process classification data
# USP classification is a way of grouping drugs
url_usp = "https://www.genome.jp/kegg-bin/download_htext?htext=br08302.keg&format=json"
usp = fromJSON(url_usp)[['children']]

psych_categories = c(
  # "Anti-Addiction/Substance Abuse Treatment Agents",
  "Antidementia Agents",
  "Antidepressants",
  "Antipsychotics",
  "Anxiolytics"
  , "Bipolar Agents"
)

usp_frame = matrix(nrow = 0, ncol = 5)
for(i in c(1:length(usp[['children']]))){
  name1 = usp[i,"name"]
  tmp1 = usp[['children']][[i]]
  for(j in c(1:length(tmp1[['name']]))){
    name2 = tmp1[j,"name"]
    if(!is.null(tmp1[['children']][[j]])){
      tmp2 = tmp1[['children']][[j]]
      for(k in c(1:length(tmp2[['name']]))){
        name3 = tmp2[k,"name"]
        if(!is.null(tmp2[['children']][[k]])){
          tmp3 = tmp2[['children']][[k]]
          for(l in c(1:length(tmp3[['name']]))){
            name4 = tmp3[l,"name"]
            if(!is.null(tmp3[['children']][[l]])){
              tmp4 = tmp3[['children']][[l]]
              for(m in c(1:length(tmp4[['name']]))){
                name5 = tmp4[m,"name"]
                usp_frame = rbind(usp_frame,c(name1,name2,name3,name4,name5))
                rm(name5)
              }
            } else {
              usp_frame = rbind(usp_frame,c(name1,name2,name3,"",name4))
              rm(name4)
            }
          }
        } else {
          usp_frame = rbind(usp_frame,c(name1,name2,"","",name3))
          rm(name3)
        }
      }
    }
  }
}
usp_frame = as.data.frame(usp_frame)
colnames(usp_frame) = c("group1","group2","group3","group4","name")
usp_frame = usp_frame %>%
  mutate(name_clean = gsub("D[0-9]{5}|\\(.*?\\)","",name),
         name_clean = toupper(trimws(name_clean)))

# # Recursion is cleaner but runs into stack issues
# get_entry = function(json_entry){
#   x = json_entry
#   if(is.null(json_entry$children)){
#     return(json_entry %>% pull(name))
#   } else {
#     return(get_entry(x))
#   }
# }

# Fuzzy matching FDA active ingredients to USP names doesn't work
usp_key = subs %>%
  select(ActiveIngredient) %>%
  distinct() %>%
  crossing(usp_frame %>%
             mutate(name_clean = toupper(name_clean)) %>%
             #filter(group1 %in% psych_categories) %>%
             select(name_clean) %>%
             distinct()) %>%
  mutate(dist1 = stringdist(name_clean,ActiveIngredient,
                           method = "lv"),
         dist2 = stringdist(name_clean,ActiveIngredient,
                            method = "qgram")) %>%
  group_by(ActiveIngredient) %>%
  filter(dist1 == min(dist1)) %>%
  filter(dist2 == min(dist2))

# Identify drugs as psych-related using suffixes
# Psych drug suffixes
# A word needs to end with it, or needs to be end of string
# Excludes: barbiturates (-bital),
# tranquilizers (-bamate, -clone), sedatives (-pidem)
psych_suff = c("pramine","ridone","triptyline","zepam",
               "zodone","zolam","axine","giline",
               "oxetine","peridol","peridone","perone","plon",
               "troline","pram","azine","etine","promine")
psych_regex = paste0(paste0(psych_suff,collapse = " |"), " |",
                     paste0(psych_suff,collapse = "$|"), "$") %>%
  toupper()

# Test on USP names with known class
test = usp_frame %>%
  select(group1,name_clean) %>%
  mutate(psych = grepl(psych_regex,name_clean))
test_sum = test %>%
  count(group1,psych) %>%
  pivot_wider(names_from = psych,values_from = n) %>%
  mutate(tagged_percent = `TRUE`/unlist(mapply(FUN = sum,`TRUE`,`FALSE`)))

# Check unmatched potential psych names
test %>%
  filter(group1 %in% psych_categories) %>%
  View

# Check all matched names
test %>%
  filter(psych == TRUE) %>%
  View

# Prepare a formatted list of all approved drugs
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

