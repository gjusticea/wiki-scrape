source("utils/functions.R")

url = "https://site.warrington.ufl.edu/ritter/files/IPO-Statistics.pdf"
cat_name = "List of IPOs"
cat_id = "G16"
ref = "Ritter, J. R. (2023, March 8). Initial Public Offerings: Updated Statistics. Retrieved March 20, 2023, from https://site.warrington.ufl.edu/ritter/files/IPO-Statistics.pdf"

# Do the cleaning
table = fread("ref/ipo counts.csv") %>%
  filter(Year >= 1975) %>%
  select(Year,
         n = `Number of Offerings`,
         gross_proceeds = `Gross Proceeds, $ Millions`) %>%
  mutate(gross_proceeds = gsub(",","",gross_proceeds),
         gross_proceeds = as.numeric(gross_proceeds)*1000000) %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = Year,
         `Event description` = paste0("Number of IPOs in ",Year," excluding IPOs with an offer price of less than $5.00, ADRs, small best efforts offers, units, Regulation A offers (small issues, raising less than $1.5 million during the 1980s and $5 million until 2012), real estate investment trusts (REITs), SPACs, natural resource limited partnerships, and closed-end funds. Banks and S&L IPOs are included. From 2012 and later, Regulation A offerings (issues raising up to $50 million are eligible) are included."),
         `Timepoint start` = as.Date(paste0(Year,"-01-01")),
         `Timepoint end` = as.Date(paste0(Year,"-12-31")),
         `Quantity outcome 1` = n,
         `Quantity outcome 2` = gross_proceeds,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-03-20")) %>%

select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Quantity outcome 2`,
       `Reference/link to data`, `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of ipos.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "Count of IPOS by year from Jay Ritter. The number of offerings excludes IPOs with an offer price of less than $5.00, ADRs, small best efforts offers, units, Regulation A offers (small issues, raising less than $1.5 million during the 1980s and $5 million until 2012), real estate investment trusts (REITs), SPACs, natural resource limited partnerships, and closed-end funds. Banks and S&L IPOs are included. From 2012 and later, Regulation A offerings (issues raising up to $50 million are eligible) are included. Gross proceeds exclude overallotment options but include the international tranche, if any. No adjustments for inflation have been made.",
  "Description quantity column 1" = "Number of IPOs",
  "Description quantity column 2" = "Gross proceeds, $",
  "Period start" = "1975",
  "Period end" = "present",
  "How was the period selected" = "Data availability with consistent methodology",
  "Collected by" = "Jay Ritter"
)

update_category_info_sheet(metadata)
