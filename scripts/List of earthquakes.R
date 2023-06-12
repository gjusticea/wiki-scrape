source("utils/functions.R")

url = "https://earthquake.usgs.gov/earthquakes/search/"
ref = "Latest Earthquakes. https://earthquake.usgs.gov/earthquakes/map/?extent=-89.48224,-382.5&extent=89.47585,742.5&range=search&timeZone=utc&search=%7B%22name%22:%22Search%20Results%22,%22params%22:%7B%22starttime%22:%221900-01-01%2000:00:00%22,%22endtime%22:%222023-04-29%2000:00:00%22,%22minmagnitude%22:6,%22orderby%22:%22time%22%7D%7D. Accessed 29 April 2023."
cat_name = "List of Magnitude 7+ earthquakes"
cat_id = "G8"

# Read in tables and get suggested tables for cleaning
table <- fread("ref/usgs earthquake world 6 1900 present.csv") %>%
  filter(type == "earthquake",
         mag >= 7) %>%
  select(time,place,magnitude = mag,latitude,longitude) %>%
  mutate(coord = unlist(mapply(FUN = paste0,latitude,"°, ",longitude,"°")),
         `Category ID` = cat_id,
         Category = cat_name,
         Event = place,
         `Event description` = unlist(mapply(FUN = paste0,
                                             magnitude," earthquake, at ",
                                             coord,", ",
                                             format(time,format="%m/%d/%Y"))),
         `Timepoint start` = time,
         `Timepoint end` = time,
         `Quantity outcome 1` = magnitude,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-04-29")) %>%

select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of major earthquakes.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of magnitude 7+ earthquakes since 1900, excluding tremors due to explosions or nuclear explosions",
  "Description quantity column 1" = "Magnitude (Richter scale)",
  "Period start" = "1900",
  "Period end" = "present",
  "How was the period selected" = "USGS data noticeably drops off in quantity after this point",
  "Collected by" = "USGS"
)

update_category_info_sheet(metadata)
