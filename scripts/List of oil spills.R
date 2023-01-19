source("utils/functions.R")

url <- "https://en.wikipedia.org/wiki/List_of_oil_spills"

tables <- download_tables(url)
suggested_tables <- suggest_tables_to_keep(tables)

# in this case the heuristic with the most columns leads us astray. Maybe a
# better approach would be to select the ones with the most rows and then
# chose the number of columns that table has.
ids_to_keep <- 2
tables <- tables[ids_to_keep]

table <- rbindlist(tables)
names(table)

## To do: more cleaning to get it into the format we want.

fwrite(table, "output/list of oil spills.csv")
