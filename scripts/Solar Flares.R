source("utils/functions.R")
library(tidyverse)
library(magrittr)

url_ha = "https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-flares/"
url_xray = "https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-flares/x-rays/goes/"
ref = "NOAA. Solar X-Ray Flares from the GOES Satellite. https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-flares/x-rays/goes/. Accessed 8 July 2023."

cat_name = "Large Solar Flares"
cat_id = "G87"

# Read in tables and do the cleaning
# 2011 appears to have changed format and dropped intensity/completeness info
ha_flare_obs = data.frame()
for(i in c(1966:2010)){
  print(i)
  tmp = read.table(paste0("https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-flares/h-alpha/reports2/",i,"/flare-report_",i,".txt"),
                   header = FALSE,
                   sep="\n") %>%
    mutate(data_code = substr(V1,1,2),
           station_code = substr(V1,3,5),
           year = as.numeric(substr(V1,6,7)),
           month = as.numeric(substr(V1,8,9)),
           day = as.numeric(substr(V1,10,11)),
           start_time = substr(V1,14,17),
           end_time = substr(V1,19,22),
           max_time = substr(V1,24,27),
           importance = substr(V1,35,35),
           brightness = substr(V1,36,36),
           completeness = substr(V1,37,37),
           area_disk = as.numeric(substr(V1,42,46)),
           area_sq_degree = as.numeric(substr(V1,47,50))*.1,
           line_width = as.numeric(substr(V1,52,56)),
           intensity = as.numeric(substr(V1,57,59))
           ) %>%
    replace_na(list(month = 1, day = 1)) %>%
    mutate(dte = as.Date(paste0(i,"-",month,"-",day)))
  ha_flare_obs %<>%
    list(tmp) %>%
    rbindlist(fill = TRUE)
}
ha_flare_obs %<>%
  filter(!is.na(dte))

# Check valid observation counts over time
# Not great? A lot of observations from station_code 777 lack importance info
#   from below, station_code 777 appears to be for xray observations
ha_flare_obs %>%
  mutate(yr = year(dte)) %>%
  group_by(yr) %>%
  summarize(valid_brightness = length(which(brightness %in% c("F","N","B")))/n(),
            valid_importance = length(which(importance %in% c("S",1,2,3)))/n(),
            valid_completeness = length(which(completeness %in% c("C","P","V","S")))/n()) %>%
  pivot_longer(cols = c(valid_brightness,valid_importance,valid_completeness),
               names_to = "metric", values_to = "percentage") %>%
  ggplot(aes(x = yr, y = percentage, group = metric, color = metric)) +
  geom_path()

# Look at x-ray measurements instead
# MUCH cleaner, near-universal coverage for x-ray class
# Intensity measures match with known cases
xray_flare_obs = data.frame()
for(i in c(1975:2016)){
  print(i)
  tmp = read.table(paste0("https://www.ngdc.noaa.gov/stp/space-weather/solar-data/solar-features/solar-flares/x-rays/goes/xrs/goes-xrs-report_",i,".txt"),
                   header = FALSE,
                   sep="\n") %>%
    mutate(data_code = substr(V1,1,2),
           station_code = substr(V1,3,5),
           year = as.numeric(substr(V1,6,7)),
           month = as.numeric(substr(V1,8,9)),
           day = as.numeric(substr(V1,10,11)),
           start_time = substr(V1,14,17),
           end_time = substr(V1,19,22),
           max_time = substr(V1,24,27),
           xray_class = substr(V1,60,60),
           # Documentation says 62-63, but 61 is needed for >X9.9 storms like 2003
           xray_intensity = as.numeric(substr(V1,61,63))*.1,
           area = substr(V1,96,102)
    ) %>%
    replace_na(list(month = 1, day = 1)) %>%
    mutate(dte = as.Date(paste0(i,"-",month,"-",day)))
  xray_flare_obs %<>%
    list(tmp) %>%
    rbindlist(fill = TRUE)
}
table = xray_flare_obs %>%
  filter(!is.na(dte),
         xray_class == "X") %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = paste0("X-class solar flare on ",dte),
         `Event description` = Event,
         `Timepoint start` = dte,
         `Timepoint end` = dte,
         `Quantity outcome 1` = xray_intensity,
         `Reference/link to data` = ref,
         `Accessed on` = Sys.Date()) %>%

select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Plot rate over time
table %>%
  mutate(yr = year(`Timepoint start`)) %>%
  count(yr) %>%
  ggplot(aes(x = yr, y = n)) +
  geom_col()

xray_flare_obs %>%
  mutate(yr = year(dte)) %>%
  filter(xray_class %in% c("C","M","X","B")) %>%
  count(yr,xray_class) %>%
  ggplot(aes(x = yr, y = n, group = xray_class, fill = xray_class)) +
  geom_col() +
  xlab("Year") +
  ylab("# of Flares") +
  ggtitle("Solar Flares by Year")

# Write to outputs folder
fwrite(table,file = "output/list of solar flares.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "List of X-class solar flares",
  "Description quantity column 1" = "Peak intensity (W/m2) between 1 and 8 Angstroms",
  "Period start" = "1975",
  "Period end" = "2015",
  "How was the period selected" = "Max range data available via NOAA",
  "Collected by" = "NOAA, GOES satellite"
)

update_category_info_sheet(metadata)
