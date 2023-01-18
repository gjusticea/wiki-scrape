source("utils/functions.R")

url <- "https://en.wikipedia.org/wiki/List_of_wars_by_death_toll"

tables <- download_tables(url)
ids_to_keep <- suggest_tables_to_keep(tables)[["max cols"]]
tables <- tables[ids_to_keep]

table <- rbindlist(tables)

## To do: more cleaning to get it into the format we want.

fwrite(table, "output/list of wars by death toll.csv")
