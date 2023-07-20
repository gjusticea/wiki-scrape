
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
           country_name %in% c("Bahamas","Bahamas, The") ~ "The Bahamas",
           country_name == "Bolivia" ~ "Bolivia (Plurinational State of)",
           grepl("Bosnia",country_name) ~ "Bosnia and Herzegovina",
           country_name %in% c("UK","Britain","United Kingdom") ~ "United Kingdom of Great Britain and Northern Ireland (the)",
           country_name == "British Virgin Islands" ~ "Virgin Islands (British)",
           country_name == "Brunei" ~ "Brunei Darussalam",
           country_name == "Cape Verde" ~ "",
           country_name == "Central African Republic" ~ "",
           country_name == "Comoros" ~ "",
           country_name == "Congo" ~ "",
           country_name %in% c("Congo, Dem. Rep.",
                               "Democratic Republic of Congo",
                               "Democratic Republic of the Congo") ~ "",
           country_name %in% c("Congo, Rep.",
                               "Republic of Congo",
                               "Republic of the Congo") ~ "",
           country_name == "Czech Republic" ~ "",
           country_name == "Dominican Republic" ~ "",
           country_name == "Equitorial Guinea" ~ "",
           country_name == "Egypt, Arab Rep" ~ "",
           country_name %in% c("Gambia","Gambia, The") ~ "",
           country_name %in% c("Iran","Iran, Islamic Rep.") ~ "",
           country_name == "Ivory Coast" ~ "",
           country_name %in% c("Korea, Dem. People's Rep.","North Korea") ~ "",
           country_name %in% c("Korea, Rep.") ~ "",
           country_name == "Kosovo" ~ "",
           country_name == "Laos" ~ "",
           country_name == "Macedonia" ~ "",
           country_name == "Marshall Islands" ~ "",
           country_name == "Mauritani" ~ "",
           country_name %in% c("Micronesia","Micronesia, Fed. Sts.") ~ "",
           country_name == "Moldova" ~ "",
           country_name == "Netherlands" ~ "",
           country_name == "Niger" ~ "",
           country_name %in% c("Ottoman","Ottoman Empire") ~ "",
           country_name == "Palestine" ~ "",
           country_name == "Papua" ~ "",
           country_name == "Philippines" ~ "",
           country_name == "Rumania" ~ "",
           country_name %in% c("Russia","Russian Federation") ~ "",
           country_name == "Sao Tome & Principe" ~ "",
           country_name == "Saudi" ~ ""
         )) %>%
  merge(country_codes, by = "country_name", all.x = TRUE)





