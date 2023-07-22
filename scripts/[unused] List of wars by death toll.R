source("utils/functions.R")

url <- "https://en.wikipedia.org/wiki/List_of_wars_by_death_toll"
cat_id = "G68"
cat_name = "List of wars"

tables <- download_tables(url)
ids_to_keep <- suggest_tables_to_keep(tables)[["max cols"]]
tables <- tables[ids_to_keep]

table <- rbindlist(tables)

table <- table |>
  rename(Event = War,
         `Quantity outcome 1` = Deathrange,
         Comment = Notes) |>
  mutate("Event description" = paste0("Combatants: ", Combatants,
                                      "Location: ", Location)) |>
  mutate(Date = gsub("\\p{Pd}", "-", Date, perl=TRUE)) |>
  separate(Date, c("Timepoint start", "Timepoint end"), "-") |>
  mutate("Timepoint start" = try_to_parse_date(`Timepoint start`)) |>
  mutate("Timepoint end" = try_to_parse_date(`Timepoint end`)) |>
  add_and_keep_relevant_cols()

fwrite(table, "output/list of wars by death toll.csv")


