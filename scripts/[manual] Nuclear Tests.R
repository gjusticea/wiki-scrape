source("utils/functions.R")

url1 = "https://www.ldeo.columbia.edu/~richards/my_papers/WW_nuclear_tests_IASPEI_HB.pdf"
url2 = "https://agupubs.onlinelibrary.wiley.com/doi/10.1029/2019JB017418"

ref1 = "Yang, X., North, R., Carl Romney, &amp; Richards, P. G. (n.d.). Worldwide Nuclear Explosions. Lamont-Doherty Earth Observatory. Retrieved May 1, 2023, from https://www.ldeo.columbia.edu/~richards/my_papers/WW_nuclear_tests_IASPEI_HB.pdf "
ref2 = "Voytan, D. P., Lay, T., Chaves, E. J., &amp; Ohman, J. T. (2019). Yield estimates for the six North Korean nuclear tests from teleseismicpwave modeling and intercorrelation ofpandpnrecordings. Journal of Geophysical Research: Solid Earth, 124(5), 4916–4939. https://doi.org/10.1029/2019jb017418 "

cat_name = "Nuclear Weapons Tests"

# Read in tables and get suggested tables for cleaning
table = readxl::read_xlsx("ref/nuclear tests.xlsx")

# Do the cleaning
table = table %>%
  mutate(Category = cat_name,
         Event = unlist(mapply(FUN = paste0,country," - ",date)),
         `Event description` = unlist(mapply(FUN = paste0,"Name (if applicable): ",
                                                          name2,
                                                          ", yield (if known): ",
                                                          yield)),
         `Timepoint start` = date,
         `Timepoint end` = date,
         `Quantity outcome 1` = yield,
         `Reference/link to data` = ifelse(country == "North Korea",
                                           ref2,ref1),
         `Accessed on` = as.Date("2023-05-01")) %>%

select(Category, Event, `Event description`, `Timepoint start`,
       `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
       `Accessed on`)

# Write to outputs folder
fwrite(table,"output/list of nuclear tests.csv")

# create an entry for the category entry field.
metadata <- data.table(
  "Category ID" = "tbd",
  "Category name" = cat_name,
  "Description" = "List of known nuclear weapons tests",
  "Description quantity column 1" = "Yield (kton)",
  "Period start" = "1945-07-16",
  "Period end" = "present",
  "How was the period selected" = "First test to present",
  "Collected by" = "Yang et al., and Voytan et al."
)

update_category_info_sheet(metadata)
