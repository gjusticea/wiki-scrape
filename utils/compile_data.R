library(magrittr)
library(rlist)

source("utils/_run all.R")

data_files = list.files(path = "output/", pattern = "list of") %>%
  .[. != "category-metadata-info.csv"]

data_sets = list()
for(i in data_files){
  print(i)
  tmp = fread(paste0("output/",i)) %>%
    select(`Category ID`,Category, Event, `Event description`, `Timepoint start`,
           `Timepoint end`, `Quantity outcome 1`, `Reference/link to data`,
           `Accessed on`) %>%
    mutate(across(c(`Timepoint start`,`Timepoint end`,`Accessed on`),
                  as.character))
  data_sets %<>%
    list.append(tmp)
}
data_sets %<>%
  rbindlist(fill = TRUE)
fwrite(data_sets,file="output/data_compilation.csv")
