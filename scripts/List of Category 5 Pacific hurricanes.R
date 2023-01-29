source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_Category_5_Pacific_hurricanes"
cat_name = "List of Category 5 Pacific hurricanes"
cat_start = as.Date("")

tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# only one useful table included, heuristics 1/3/4 all work
table = tables[[suggested_tables$`max cols`]] %>%
  .[c(2:nrow(.)),c(1:4,6,7)]
colnames(table) = c("Name","Year","Dates","Time.as.cat.5","kmh","hPa")

table = table %>%
  # "Before the advent of reliable geostationary satellite coverage in 1966, the
  # number of eastern Pacific tropical cyclones was significantly underestimated"
  filter(Year >= 1966) %>%
  separate_wider_delim(Dates,delim = "-", names = c("start","end"),
                       too_few = "align_start") %>%
  # Split the date range column into actual start and end dates
  mutate(end = ifelse(is.na(end),start,end),
         end = ifelse(start != end, paste0(gsub("[^a-zA-z]", " ", start),end),end),
         across(c(start,end),paste0,", ",Year)
  ) %>%
  mutate(Category = cat_name,
         Event = Name,
         `Event description` = paste0("Peak one-minute sustained winds(km/h): ",kmh,
                                      "; Pressure(hPa): ",hPa),
         `Timepoint start` = as_date(start,format = "%B%d, %Y"),
         `Timepoint end` = as_date(end,format = "%B%d, %Y"),
         `Quantity outcome 1` = Time.as.cat.5,
         `Reference/link to data` = url,
         `Accessed on` = Sys.Date()
  ) %>%
  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

fwrite(table,"output/list of cat 5 pacific hurricanes.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = cat_name,
  "Description quantity column 1" = "Time spent as a category 5 storm, in hours",
  "Period start" = "1966",
  "Period end" = "present",
  "How was the period selected" = "Starts with reliable geostationary satellite coverage in 1966",
  "Collected by" = "Wikipedia"
)

update_category_info_sheet(metadata)
