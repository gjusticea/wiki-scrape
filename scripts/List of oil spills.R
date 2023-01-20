source("utils/functions.R")

url <- "https://en.wikipedia.org/wiki/List_of_oil_spills"

tables <- download_tables(url)
suggested_tables <- suggest_tables_to_keep(tables)

# in this case the heuristic with the most columns leads us astray.
# selecting the table that has the highest number of columns works, though
ids_to_keep <- suggested_tables[["cols max row"]]
tables <- tables[ids_to_keep]
table <- rbindlist(tables)

table <- table |>
  rename(Event = `Spill / Vessel`) |>
  mutate(Category = "Major oil spills",
         "Event description" = paste0("Location: ", Location, ", Owner: ", Owner)) |>
  tidyr::separate(Dates, c("Timepoint start","Timepoint end"), sep = " – ", fill = "right") |>
  mutate("Timepoint start" = try_to_parse_date(`Timepoint start`)) |>
  mutate("Timepoint end" = try_to_parse_date(`Timepoint end`)) |>
  # take the median estimated oil spill as continuous quantity
  mutate(`Min Tonnes` = as.numeric(gsub(",", "", `Min Tonnes`)),
         `Max Tonnes` = as.numeric(gsub(",", "", `Max Tonnes`))) |>
  rowwise() |>
  mutate("Quantity outcome 1" = mean(c_across(ends_with("Tonnes")))) |>
  # helper function to do some tidying like selecting cols at the end
  add_and_keep_relevant_cols()

fwrite(table, "output/list of oil spills.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = "List of oil spills",
  "Description" = "List of oil spills",
  "Description quantity column 1" = "Mean of min and max estimates for the number of Tonnes of oil spilled",
  "Period start" = "1903",
  "Period end" = "present",
  "How was the period selected" = "Should be a complete list starting with the first oil spill",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)


