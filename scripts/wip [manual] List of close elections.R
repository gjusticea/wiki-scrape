source("utils/functions.R")
library(jsonlite)

url = "https://www.electionguide.org/feed/digest/"
url2 = "https://publications.iadb.org/en/database-political-institutions-2020-dpi2020"
ref = "International Foundation for Electoral Systems. (2023). ElectionGuide. https://www.electionguide.org/ "

cat_name = "List of close elections"
cat_id = "G82"

countries = fromJSON("C:/Users/slapt/Downloads/countries.json")
clients = fromJSON("C:/Users/slapt/Downloads/clients.json")
metadata = fromJSON("C:/Users/slapt/Downloads/metadata.json")
digest = fromJSON("C:/Users/slapt/Downloads/digest.json")
elections_demo = fromJSON("C:/Users/slapt/Downloads/elections_demo.json")
api_elections = fromJSON("C:/Users/slapt/Downloads/api_elections.json")
elections = fromJSON("C:/Users/slapt/Downloads/elections.json")


# Read in tables and do the cleaning
table = fromJSON("C:/Users/slapt/Downloads/test.json")

select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,file = "output/")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "",
  "Description quantity column 1" = "",
  "Period start" = "",
  "Period end" = "present",
  "How was the period selected" = "",
  "Collected by" = ""
)

update_category_info_sheet(metadata)
