source("utils/functions.R")

# remove special characters
rm_special = function(x){
  x %>%
    gsub(pattern="<e3>",replacement="a") %>%
    gsub(pattern="<e9>",replacement="e") %>%
    gsub(pattern="<ed>",replacement="i") %>%
    gsub(pattern="<fc>",replacement="u") %>%
    gsub(pattern="<f4>",replacement="o") %>%
    gsub(pattern="<92>",replacement="'") %>%
    gsub(pattern="<c5>",replacement="A") %>%
    gsub(pattern="<a0>",replacement="&") %>%
    gsub(pattern="<e7>",replacement="c")
}

# import key files
country_codes = fread("ref/country_key.csv") %>%
  select(ccode = `Alpha-3 code`, country_name = `Country name`) %>%
  mutate(country_name = rm_special(country_name))

ckey = fread("ref/country key prep/Country key wide raw.csv", header = TRUE) %>%
  pivot_longer(cols = !c(`Category ID`,Category,Event,`Event description`),
               names_to = "tmp", values_to = "country_name") %>%
  select(-tmp) %>%
  filter(country_name != "") %>%
  mutate(country_name = rm_special(country_name),
         country_name = case_when(
           country_name %in% c("Bahamas","Bahamas, The") ~ "Bahamas (the)",
           country_name == "Bolivia" ~ "Bolivia (Plurinational State of)",
           country_name %in% c("Bosnia-Herzegovina","Bosnia & Herzegovina") ~ "Bosnia and Herzegovina",
           country_name %in% c("UK","Britain","United Kingdom") ~ "United Kingdom of Great Britain and Northern Ireland (the)",
           country_name == "British Virgin Islands" ~ "Virgin Islands (British)",
           country_name == "Brunei" ~ "Brunei Darussalam",
           country_name == "Cape Verde" ~ "Cabo Verde",
           country_name == "Central African Republic" ~ "Central African Republic (the)",
           country_name == "Comoros" ~ "Comoros (the)",
           country_name == "Congo" ~ "Congo (the)",
           country_name %in% c("Congo, Dem. Rep.",
                               "Democratic Republic of Congo",
                               "Democratic Republic of the Congo") ~ "Congo (the Democratic Republic of the)",
           country_name %in% c("Congo, Rep.",
                               "Republic of Congo",
                               "Republic of the Congo") ~ "Congo (the)",
           country_name == "Czech Republic" ~ "Czechia",
           country_name == "Dominican Republic" ~ "Dominican Republic (the)",
           country_name == "Equitorial Guinea" ~ "Equatorial Guinea",
           country_name %in% c("Gambia","Gambia, The") ~ "Gambia (the)",
           country_name %in% c("Iran","Iran, Islamic Rep.") ~ "Iran (Islamic Republic of)",
           country_name == "Ivory Coast" ~ "Cote d'Ivoire",
           country_name %in% c("Korea, Dem. People's Rep.","North Korea") ~ "Korea (the Democratic People's Republic of)",
           country_name %in% c("Korea, Rep.","South Korea") ~ "Korea (the Republic of)",
           country_name == "Laos" ~ "Lao People's Democratic Republic (the)",
           country_name == "Macedonia" ~ "North Macedonia",
           country_name == "Marshall Islands" ~ "Marshall Islands (the)",
           country_name %in% c("Micronesia","Micronesia, Fed. Sts.") ~ "Micronesia (Federated States of)",
           country_name == "Moldova" ~ "Moldova (the Republic of)",
           country_name == "Netherlands" ~ "Netherlands, Kingdom of the",
           country_name == "Niger" ~ "Niger (the)",
           country_name %in% c("Ottoman","Ottoman Empire") ~ "Ottoman Empire",
           country_name == "Philippines" ~ "Philippines (the)",
           country_name %in% c("Russia","Russian Federation") ~ "Russian Federation (the)",
           country_name == "Sao Tome & Principe" ~ "Sao Tome and Principe",
           country_name == "Saudi" ~ "Saudi Arabia",
           country_name %in% c("St. Kitts & Nevis","St. Kitts and Nevis") ~ "Saint Kitts and Nevis",
           country_name %in% c("St. Vincent & the Grenadines","St. Vincent and the Grenadines") ~ "Saint Vincent and the Grenadines",
           country_name %in% c("Sint Maarten") ~ "Sint Maarten (Dutch part)",
           country_name %in% c("St. Martin (French part)","St. Martin") ~ "Saint Martin (French part)",
           country_name == "St. Lucia" ~ "Saint Lucia",
           country_name == "Soviet Union" ~ "USSR",
           country_name == "Sudan" ~ "Sudan (the)",
           country_name == "Swaziland" ~ "Eswatini",
           country_name == "Syria" ~ "Syrian Arab Republic (the)",
           country_name == "Tanzania" ~ "Tanzania, the United Republic of",
           country_name == "Trinidad & Tobago" ~ "Trinidad and Tobago",
           country_name == "Turkey" ~ "Turkiye",
           country_name %in% c("Turks and Caicos","Turks and Caicos Islands") ~ "Turks and Caicos Islands (the)",
           country_name == "United Arab Emirates" ~ "United Arab Emirates (the)",
           country_name %in% c("U.S.","US","USA","United States","United States of America") ~ "United States of America (the)",
           country_name == "Vatican City/Holy See" ~ "Holy See (the)",
           country_name == "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
           country_name == "Vietnam" ~ "Viet Nam",
           country_name == "Virgin Islands" ~ "Virgin Islands (U.S.)",
           country_name %in% c("West Bank and Gaza","West Bank and Gaza Strip","Palestine") ~ "Palestine, State of",
           TRUE ~ country_name
         )) %>%
  merge(country_codes, by = "country_name", all.x = TRUE)

ckey %>% filter(is.na(ccode)) %>%
  count(country_name)

# Add label type groups, if a single applies to a whole category
ckey %<>%
  mutate(ccode_type = case_when(
    `Category ID` %in% paste0("G",c(16:63,67,7,81,9,3,84,14,15)) ~ "Polity",
    `Category ID` %in% paste0("G",c(65,76)) ~ "Geography"
  )) %>%
  select(`Category ID`,Category,Event,`Event description`,
         country_name, ccode, ccode_type) %>%
  arrange(`Category ID`,Event)

fwrite(ckey, file="ref/country key prep/Country key long raw.csv")


