source("utils/functions.R")

url = "https://politicsir.cass.anu.edu.au/form/atrocity-forecasting-project-data-download-form"
ref = "Butcher, Charles, Benjamin E. Goldsmith, Sascha Nanlohy, Arcot Sowmya, and David Muchlinski. 2020. “Introducing the Targeted Mass Killing Dataset for the Study and Forecasting of Mass Atrocities,” Journal of Conflict Resolution [in press]"

cat_name = "Targeted Mass Killings"
cat_id = "G86"

country_ids = fread("ref/country_key.csv") %>%
  select(country = `Country name`,
         ccode = `Alpha-3 code`)

# Read in table and do the cleaning
# Unique on event.name.description / year / actor.name
# Design:
# One table listing per event.name.description
# Actors listed in description
# Time frame as min/max of year

# Old script that gets death toll estimates by group
# Issue is that not every group has its own death toll, and some conflicts like
#   Darfur don't have groups split out at all, so this would lead to an undercount
#   both in terms of fatalities as well as number of events

# table = readxl::read_xls("ref/tmk_events_release_1.1.xls") %>%
#   filter(tmk == 1) %>%
#   select(country, actor.name, event.name.description, year, total.deaths,
#          group1.name, group1.death = group1.best.fatalities.estimate,
#          group2.name, group2.death = group2.best.fatalities.estimate,
#          group3.name, group3.death = group3.best.fatalities.estimate,
#          group4.name, group4.death = group4.best.fatalities.estimate,
#          group5.name, group5.death = group5.best.fatalities.estimate
#   ) %>%
#   pivot_longer(cols = c(group1.name,group2.name,group3.name,group4.name,group5.name),
#                names_to = "group_name", values_to = "group") %>%
#   filter(group != "NA") %>%
#   mutate(group_name = substr(group_name,1,6)) %>%
#   pivot_longer(cols = c(group1.death,group2.death,group3.death,
#                         group4.death,group5.death),
#                names_to = "death_name", values_to = "deaths") %>%
#   filter(deaths != "NA") %>%
#   mutate(death_name = substr(death_name,1,6)) %>%
#   filter(group_name == death_name) %>%
#   mutate(across(c(total.deaths,deaths,year),as.numeric))

# Best option - group by tmk_id and deaths.est
# This allows us to get a death estimate for any event that has one, regardless
#     of whether it has a breakout by group or by time
#   Need to add event.name.desc for listing descriptions, and because Uganda in
#     1966 isn't unique

# Leaving off - clean up the dumb event.name.desc column

table = readxl::read_xls("ref/tmk_events_release_1.1.xls") %>%
  filter(tmk == 1) %>%
  mutate(event.name.description = trimws(event.name.description),
         event.name.description = toupper(event.name.description),
         event.name.description = str_to_title(event.name.description),
         event.name.description = case_when(
           event.name.description == "Car Civil War" ~ "Central African Republic Civil War",
           event.name.description %in% c("Jss/Sb Insurgency",
                                         "Chittagong Hill Tracts Conflict",
                                         "Chittagong Hill Tract Conflict") ~ "Chittagong Hill Tracts Conflict and PCJSS Insurgency",
           event.name.description %in% c("War In Darfur","Darfur Genocide") ~ "Darfur Genocide",
           event.name.description %in% c("Nlft Insurgency",
                                         "Attf Insurgency",
                                         "Nscn-Im Sepratist Insurgency") ~ "Northeast India Insurgency",
           event.name.description %in% c("Inyenzi Attacks/Killing Of Tutsis",
                                         "Massacres Of Tutsis") ~ "Inyenzi Attacks/Massacres Of Tutsis",

           event.name.description == "Afghan Civil War" & year %in% c(1978:1989) ~ "USSR Invasion and Afghan Civil War",
           country == "Afghanistan" & year %in% c(1990:2020) ~ "Afghan Conflict",

           actor.name == "Government of South Korea" & event.name.description == "Execution of Communist Supporters" ~ "South Korea - Execution of Communist Supporters",
           actor.name == "Government of South Vietnam" & event.name.description == "Execution of Communist Supporters" ~ "South Vietnam - Execution of Communist Supporters",
           actor.name == "Government of North Vietnam" & event.name.description == "Land Reform Campaign" ~ "North Vietnam - Land Reform Campaign",
           country == "China" &
             event.name.description %in% c("Land Reform Campaign","Land Reform Campaign/Campaign To Suppress Counter-Revolutionaries") &
             year %in% c(1947:1952) ~ "China - Land reform campaign and campaign to suppress counter-revolutionaries",

           country == "Cambodia" & grepl("Civil|Genocide",event.name.description) ~ "Cambodian civil war and genocide",
           country == "Burundi" & event.name.description %in% c("Burundi Genocide","Insurrection") ~ "Burundi insurrection and genocide",
           actor.name == "Government of Chile" & grepl("Pinochet|Leftist",event.name.description) ~ "Pinochet coup and anti-leftist killings",
           actor.name == "Government of Ethiopia" & grepl("Hausein|Civil War",event.name.description) ~ "Ethiopian Civil War and Destruction of Hawzen",
           actor.name == "Government of Haiti" & grepl("Papa Doc|Fignole",event.name.description) ~ "Military Coup Against Fignole and Papa Doc Repression",
           actor.name == "Government of Indonesia" & grepl("East Timor|Santa Cruz",event.name.description) ~ "East Timor repression and Santa Cruz massacre",
           actor.name == "Government of Iran" & year %in% c(1979:1985) ~ "Iranian revolution and persecution of opposition",
           actor.name == "Government of Iraq" & grepl("Balisan|Anfal",event.name.description) ~ "Balisan Valley Chemical Attacks and Anfal Massacre",
           country == "Ivory Coast" & year %in% c(2010:2011) ~ "Post election violence and Ivorian civil war",
           event.name.description %in% c("Suppression Of Student Protestors",
                                         "Myanmar Internal Conflicts",
                                         "Anti-Chinese Riots",
                                         "Myanmar Internal Conflicts/Suppression Of Anti-Government Protestors",
                                         "") &
             country %in% c("Myanmar (Burma)","Myanmar (Burma), Bangladesh") ~ "Myanmar internal conflicts and suppression of protestors",
           actor.name == "Government of Nigeria" & year %in% c(1967:1970) ~ "Igbo massacres and Biafran war",
           actor.name == "Government of Rwanda" & year %in% c(1991:1994) ~ "Rwandan genocide and civil war",
           TRUE ~ event.name.description
         )) %>%
  mutate(year = as.numeric(year))

group_key = table %>%
  select(event.name.description, year, actor.name, country,
         group1.name, group2.name, group3.name, group4.name, group5.name) %>%
  pivot_longer(cols = c(group1.name, group2.name, group3.name, group4.name, group5.name),
               names_to = "src", values_to = "group", values_drop_na = TRUE) %>%
  select(-src) %>%
  # split out countries
  separate_wider_delim(cols = country,delim = ", ",names_sep = "_",too_few = "align_start") %>%
  pivot_longer(cols = c(country_1,country_2,country_3,country_4),
               names_to = "src", values_to = "country", values_drop_na = TRUE) %>%
  select(-src) %>%
  # split out actors
  separate_wider_delim(cols = actor.name,delim = ", ",names_sep = "_",too_few = "align_start") %>%
  pivot_longer(cols = c(actor.name_1,actor.name_2),
               names_to = "src", values_to = "actor.name", values_drop_na = TRUE) %>%
  select(-src) %>%
  group_by(event.name.description) %>%
  summarize(countries = paste0(unique(country[country != "NA"]),collapse=", "),
            actors = paste0(unique(actor.name[actor.name != "NA"]),collapse=", "),
            target_groups = paste0(unique(group[group != "NA"]),collapse=", "),
            year_start = min(year),
            year_end = max(year))

table_clean = table %>%
  select(tmk_id, deaths.est, event.name.description) %>%
  distinct() %>%
  mutate(deaths.est = as.numeric(deaths.est)) %>%
  group_by(event.name.description) %>%
  summarize(deaths = sum(deaths.est, na.rm=TRUE)) %>%
  merge(group_key) %>%
  mutate(deaths = ifelse(deaths > 0,deaths,NA)) %>%
  mutate(`Category ID` = cat_id,
         Category = cat_name,
         Event = paste0(event.name.description," - ",countries),
         `Event description` = paste0("Actors: ",actors,
                                      "; Targeted groups: ",target_groups),
         `Timepoint start` = year_start,
         `Timepoint end` = year_end,
         `Quantity outcome 1` = deaths,
         `Reference/link to data` = ref,
         `Accessed on` = as.Date("2023-07-04"))

table_clean2 = table_clean %>%
select(`Category ID`, Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# write country info
country_set = table_clean %>%
  select(`Category ID`,Category, Event, `Event description`, countries) %>%
  separate_wider_delim(cols = countries,delim = ", ",names_sep = "_",too_few = "align_start") %>%
  pivot_longer(cols = c(countries_1,countries_2,countries_3,countries_4),
               names_to = "ctry_src", values_to = "country", values_drop_na = TRUE) %>%
  select(-ctry_src) %>%
  filter(country != "NA") %>%
  merge(country_ids, by = "country", all.x = TRUE) %>%
  select(`Category ID`, Category, Event, `Event description`,
         country, ccode)
fwrite(country_set, file="ref/country key prep/tmk from data.csv")


# Write to outputs folder
fwrite(table_clean2,file = "output/list of targeted mass killings.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = cat_id,
  "Category name" = cat_name,
  "Description" = "Number of targeted mass killings, as defined by Butcher et al.",
  "Description quantity column 1" = "Estimated number of deaths, excluding periods where data is unavailable",
  "Period start" = "1946",
  "Period end" = "2020",
  "How was the period selected" = "Max range available in data source",
  "Collected by" = "Butcher et al."
)

update_category_info_sheet(metadata)
