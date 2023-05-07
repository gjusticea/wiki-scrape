source("utils/functions.R")

url = "https://datadashboard.fda.gov/ora/cd/recalls.htm"
cat_name = "FDA Drug and Biologic Recalls"
access_date = as.Date("2022-02-20")

# Read in tables and get suggested tables for cleaning
recalls = readxl::read_xlsx("ref/fda recalls dashboard 02202023.xlsx")

# Do the cleaning
# "An event is a firm’s recall of one or more products"
table = recalls %>%
  filter(`Product Type` %in% c("Drugs","Biologics")) %>%
  select(`Recalling Firm Name`, `Product Type`, Status, `Recalling Firm Country`,
         `Center Classification Date`, `Product Description`, `Reason for Recall`,
         `Product ID`, `Event ID`, `Event Classification`) %>%
  distinct() %>%
  group_by(`Recalling Firm Name`, `Recalling Firm Country`, Status,
           Date = `Center Classification Date`, `Product Type`,
           `Event ID`, `Event Classification`) %>%
  summarize(`Product List` = paste0(`Product Description`,collapse=", "),
            product_count = length(unique(`Product ID`)),
            recall_reason = paste0(unique(`Reason for Recall`),collapse=", ")) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Category = cat_name,
         Event = paste0("Event ID #",`Event ID`," - ",`Event Classification`," - ",`Product Type`," - ",`Recalling Firm Name`),
         `Event description` = paste0(recall_reason,";; ",
                                      `Product List`),
         `Timepoint start` = Date,
         `Timepoint end` = Date,
         `Quantity outcome 1` = product_count,
         `Reference/link to data` = url,
         `Accessed on` = access_date) %>%
  ungroup() %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/drug recalls.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "FDA issued recalls of drug and biologic products",
  "Description quantity column 1" = "# of unique products included in the recall",
  "Period start" = as.character(as.Date(min(recalls$`Center Classification Date`))),
  "Period end" = as.character(access_date),
  "How was the period selected" = "Data availability/accessibility from FDA",
  "Collected by" = "FDA"
)

update_category_info_sheet(metadata)


################################################################################

rm(table,cat_name,metadata)

cat_name = "FDA Device Recalls"

# Do the cleaning
# "An event is a firm’s recall of one or more products"
table = recalls %>%
  filter(`Product Type` == "Devices") %>%
  select(`Recalling Firm Name`, `Product Type`, Status, `Recalling Firm Country`,
         `Center Classification Date`, `Product Description`, `Reason for Recall`,
         `Product ID`, `Event ID`, `Event Classification`) %>%
  distinct() %>%
  group_by(`Recalling Firm Name`, `Recalling Firm Country`, Status,
           Date = `Center Classification Date`, `Product Type`,
           `Event ID`, `Event Classification`) %>%
  summarize(`Product List` = paste0(`Product Description`,collapse=", "),
            product_count = length(unique(`Product ID`)),
            recall_reason = paste0(unique(`Reason for Recall`),collapse=", ")) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Category = cat_name,
         Event = paste0("Event ID #",`Event ID`," - ",`Event Classification`," - ",`Product Type`," - ",`Recalling Firm Name`),
         `Event description` = paste0(recall_reason,";; ",
                                      `Product List`),
         `Timepoint start` = Date,
         `Timepoint end` = Date,
         `Quantity outcome 1` = product_count,
         `Reference/link to data` = url,
         `Accessed on` = access_date) %>%
  ungroup() %>%

  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(table,"output/device recalls.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "FDA issued recalls of medical devices",
  "Description quantity column 1" = "# of unique products included in the recall",
  "Period start" = as.character(as.Date(min(recalls$`Center Classification Date`))),
  "Period end" = as.character(access_date),
  "How was the period selected" = "Data availability/accessibility from FDA",
  "Collected by" = "FDA"
)

update_category_info_sheet(metadata)


################################################################################

rm(table,cat_name,metadata)

cat_name = "FDA Food/Cosmetics Recalls"

# Do the cleaning
# "An event is a firm’s recall of one or more products"
table = recalls %>%
  filter(`Product Type` == "Food/Cosmetics") %>%
  select(`Recalling Firm Name`, `Product Type`, Status, `Recalling Firm Country`,
         `Center Classification Date`, `Product Description`, `Reason for Recall`,
         `Product ID`, `Event ID`, `Event Classification`) %>%
  distinct() %>%
  group_by(`Recalling Firm Name`, `Recalling Firm Country`, Status,
           Date = `Center Classification Date`, `Product Type`,
           `Event ID`, `Event Classification`) %>%
  summarize(`Product List` = paste0(`Product Description`,collapse=", "),
            product_count = length(unique(`Product ID`)),
            recall_reason = paste0(unique(`Reason for Recall`),collapse=", ")) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Category = cat_name,
         Event = paste0("Event ID #",`Event ID`," - ",`Event Classification`," - ",`Product Type`," - ",`Recalling Firm Name`),
         `Event description` = paste0(recall_reason,";; ",
                                      `Product List`),
         `Timepoint start` = Date,
         `Timepoint end` = Date,
         `Quantity outcome 1` = product_count,
         `Reference/link to data` = url,
         `Accessed on` = access_date) %>%
  ungroup() %>%

  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(table,"output/food cosmetics recalls.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "FDA issued recalls of food and cosmetic products",
  "Description quantity column 1" = "# of unique products included in the recall",
  "Period start" = as.character(as.Date(min(recalls$`Center Classification Date`))),
  "Period end" = as.character(access_date),
  "How was the period selected" = "Data availability/accessibility from FDA",
  "Collected by" = "FDA"
)

update_category_info_sheet(metadata)


################################################################################

rm(table,cat_name,metadata)

cat_name = "FDA Veterinary Recalls"

# Do the cleaning
# "An event is a firm’s recall of one or more products"
table = recalls %>%
  filter(`Product Type` == "Veterinary") %>%
  select(`Recalling Firm Name`, `Product Type`, Status, `Recalling Firm Country`,
         `Center Classification Date`, `Product Description`, `Reason for Recall`,
         `Product ID`, `Event ID`, `Event Classification`) %>%
  distinct() %>%
  group_by(`Recalling Firm Name`, `Recalling Firm Country`, Status,
           Date = `Center Classification Date`, `Product Type`,
           `Event ID`, `Event Classification`) %>%
  summarize(`Product List` = paste0(`Product Description`,collapse=", "),
            product_count = length(unique(`Product ID`)),
            recall_reason = paste0(unique(`Reason for Recall`),collapse=", ")) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(Category = cat_name,
         Event = paste0("Event ID #",`Event ID`," - ",`Event Classification`," - ",`Product Type`," - ",`Recalling Firm Name`),
         `Event description` = paste0(recall_reason,";; ",
                                      `Product List`),
         `Timepoint start` = Date,
         `Timepoint end` = Date,
         `Quantity outcome 1` = product_count,
         `Reference/link to data` = url,
         `Accessed on` = access_date) %>%
  ungroup() %>%

  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

# Write to outputs folder
fwrite(table,"output/veterinary recalls.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "FDA issued recalls of veterinary products",
  "Description quantity column 1" = "# of unique products included in the recall",
  "Period start" = as.character(as.Date(min(recalls$`Center Classification Date`))),
  "Period end" = as.character(access_date),
  "How was the period selected" = "Data availability/accessibility from FDA",
  "Collected by" = "FDA"
)

update_category_info_sheet(metadata)
