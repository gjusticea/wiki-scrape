source("utils/functions.R")

url = "https://en.wikipedia.org/wiki/List_of_Category_5_Atlantic_hurricanes"
cat_name = "List of Category 5 Atlantic hurricanes"

tables = download_tables(url)
suggested_tables = suggest_tables_to_keep(tables)

# Table 2 is the one we want
# Table 5 may be interesting if we want a better picture of damage
table = tables[[2]] %>%
  .[,c(1:8)] %>%
  clean_table()
colnames(table) = c("Name","Dates","Duration","Wind","Pressure","Areas",
                    "Damage","Deaths")

table = table %>%
  filter(!grepl("Legend: ",Name)) %>%
  # Convert date to usable start/end format
  mutate(Dates = gsub("[^a-zA-Z0-9, -]","",Dates)) %>%
  separate_wider_delim(Dates,delim = "-", names = c("start","end"),
                       too_few = "align_start") %>%
  mutate(end = ifelse(is.na(end),start,end),
         end = ifelse(start != end, paste0(gsub("[^a-zA-z]", " ", start),end),end),
         start = gsub(",\\s(.+)","",start)) %>%
  separate_wider_delim(end,delim = ", ", names = c("end","Year")) %>%
  mutate(across(c(start,end),paste0,", ",Year),
         across(c(start,end),as_date,format = "%B%d, %Y")) %>%
  filter(start >= as.Date("1966-01-01")) %>%
  select(-Year) %>%
  # Convert duration to clean format
  separate_wider_delim(Duration,delim = "day",names = c("days","dur_tmp"),
                       too_few = "align_end") %>%
  mutate(dur_tmp = gsub("^s|^s\\s|^\\s","",dur_tmp)) %>%
  separate_wider_delim(dur_tmp,delim = "hour",names = c("hours","minutes"),
                       too_few = "align_end") %>%
  mutate(minutes = gsub("^s|^s\\s|^\\s|\\sminute|\\sminutes","",minutes),
         minutes = ifelse(minutes == "",0,minutes),
         across(c(days,hours,minutes),as.numeric)) %>%
  replace_na(list(days = 0,hours = 0)) %>%
  mutate(Duration = unlist(mapply(FUN = sum,days*24,hours,minutes/60))) %>%
  select(-days, -hours, -minutes) %>%
  # Clean up detail columns
  mutate(Wind = gsub("1[0-9][0-9] mph \\(|\\skm/h\\)","",Wind),
         Pressure = gsub(" hPa(.+)","",Pressure))

# put into uniform format
table = table %>%
  mutate(Category = cat_name,
         Event = Name,
         `Event description` = paste0("Peak winds(km/h): ",Wind,
                                      "; Pressure(hPa): ",Pressure,
                                      "; Areas affected: ",Areas,
                                      "; Damage: ",Damage,
                                      "; Deaths: ",Deaths),
         `Timepoint start` = start,
         `Timepoint end` = end,
         `Quantity outcome 1` = Duration,
         `Quantity outcome 2` = Deaths,
         `Reference/link to data` = url,
         `Accessed on` = Sys.Date()
  ) %>%
  select(Category, Event, `Event description`, `Timepoint start`,
         `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
         `Accessed on`)

fwrite(table,"output/list of cat 5 atlantic hurricanes.csv")

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





