source("utils/functions.R")

url <- "https://en.wikipedia.org/wiki/List_of_oil_spills"

tables <- download_tables(url)
suggested_tables <- suggest_tables_to_keep(tables)

# in this case the heuristic with the most columns leads us astray.
# selecting the table that has the highest number of columns works, though
ids_to_keep <- suggested_tables[["cols max row"]]
tables <- tables[ids_to_keep]
table <- rbindlist(tables)


## To do: more cleaning to get it into the format we want.

fwrite(table, "output/list of oil spills.csv")
