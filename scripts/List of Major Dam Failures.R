source("utils/functions.R")

url <- "https://en.wikipedia.org/wiki/Dam_failure"
cat_id = "7"

tables <- download_tables(url)
suggested_tables <- suggest_tables_to_keep(tables)

# in this case the heuristic with the most columns leads us astray.
# selecting the table that has the highest number of columns works, though
ids_to_keep <- suggested_tables[["cols max row"]]
tables <- tables[ids_to_keep]
table <- rbindlist(tables) |>
  clean_table(remove_first = FALSE)

table <- table |>
  rename(Event = `Dam/incident`,
         "Event description" = Details,
         "Timepoint start" = Date,
         "Quantity outcome 1" = Fatalities) |>
  mutate(Category = "Major dam failures",
         "Timepoint end" = NA,
         Location = paste(Location, Country, sep = " - ")) |>
  mutate("Timepoint start" = try_to_parse_date(`Timepoint start`)) |>
  mutate("Timepoint end" = try_to_parse_date(`Timepoint end`)) |>
  rowwise() |>
  mutate(`Category ID` = cat_id) |>
  # helper function to do some tidying like selecting cols at the end
  add_and_keep_relevant_cols()

fwrite(table, "output/list of major dam failures.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = "List of major dam failures",
  "Description" = "List of major dam failures sources from Wikipedia",
  "Description quantity column 1" = "Number of casualties, if known",
  "Period start" = "575",
  "Period end" = "present",
  "How was the period selected" = "Based on available data, starting with the first even listed on Wikipedia",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)

