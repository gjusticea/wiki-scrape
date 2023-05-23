library(tidyverse)
source("utils/functions.R")

url = "https://data.worldbank.org/indicator/NY.GDP.MKTP.CD"

# Read in tables and get suggested tables for cleaning
mtd = fread("ref/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_4898871/Metadata_Country_API_NY.GDP.MKTP.CD_DS2_en_csv_v2_4898871.csv")
colnames(mtd) = mtd[1,] %>% unlist()
mtd = mtd[2:nrow(mtd),] %>% select(-V6)

table = fread("ref/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_4898871/API_NY.GDP.MKTP.CD_DS2_en_csv_v2_4898871.csv")
colnames(table) = table[1,] %>% unlist()
table = table[2:nrow(table),] %>%
  select(-`Indicator Name`, -`Indicator Code`) %>%
  pivot_longer(cols = -c(`Country Name`,`Country Code`),
               names_to = "Year",values_to = "GDP") %>%
  filter(!is.na(as.numeric(Year))) %>%
  group_by(`Country Name`,`Country Code`) %>%
  arrange(`Country Name`,Year) %>%
  mutate(yoy_change = (GDP - lag(GDP)),
         yoy_change_perc = yoy_change/lag(GDP))

# Do the cleaning
region_set = mtd %>% filter(Region == "") %>% select(`Country Code`)

table_clean = table %>%
  merge(region_set) %>%
  filter(!is.na(yoy_change)) %>%
  arrange(`Country Code`,Year) %>%
  select(cc = `Country Code`,Countries = `Country Name`,Year,yoy_change_perc) %>%
  mutate(recession = yoy_change_perc < 0)

# Check for any regions with years of missing data
missing_check = table_clean %>%
  mutate(Year = as.numeric(Year)) %>%
  group_by(Countries) %>%
  summarize(years = length(unique(Year)),
            check = (max(Year)-min(Year))+1,
            flag = years != check) %>%
  filter(flag == TRUE)
if(nrow(missing_check) > 0){
  stop(paste0("Areas with missing years: ",
              paste0(unique(missing_check$`Country Name`),collapse=", ")))
}

table_clean = table_clean %>%
  mutate(Category = paste0("Recessions - ",Countries),
         Event = Year,
         `Event description` = paste0("Year over year change in GDP, ",Year),
         `Timepoint start` = as.Date(paste0(Year,"-01-01")),
         `Timepoint end` = as.Date(paste0(Year,"-12-31")),
         `Quantity outcome 1` = yoy_change_perc,
         `Reference/link to data` = url,
         `Accessed on` = as.Date("2023-02-28")) %>%
select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`,cc)

cat_id_key = table_clean %>%
  arrange(cc) %>%
  select(Category,cc) %>%
  distinct() %>%
  mutate(`Category ID` = paste0("G",row_number()+16))

table_clean = table_clean %>%
  merge(cat_id_key)

# Write to outputs folder
fwrite(table_clean %>% select(-cc),
       "output/list of recessions.csv")

# create an entry for the category entry field.
for(i in 1:length(unique(table_clean$cc))){
  cat = unique(table_clean$cc)[[i]]
  cat_desc = mtd %>% filter(`Country Code` == cat)
  cat_id = cat_id_key[which(cat_id_key$cc == cat),"Category ID"]
  tbl_cat = table_clean %>%
    filter(cc == cat)

  metadata <- data.table(
    "Category ID" = cat_id,
    "Category name" = unique(tbl_cat$Category),
    "Description" = paste0("Each year labeled as recession y/n, meaning a negative year-over-year change in GDP. ",
                           "Region: ",cat_desc$TableName,
                           ". Additional notes from World Bank: ",cat_desc$SpecialNotes),
    "Description quantity column 1" = "% Change in GDP vs. prior year (.01 = 1%)",
    "Period start" = min(tbl_cat$Event),
    "Period end" = max(tbl_cat$Event),
    "How was the period selected" = "Data availability from World Bank",
    "Collected by" = "World Bank"
  )

  update_category_info_sheet(metadata)
}
