source("utils/functions.R")

scripts <- list.files("scripts") %>%
  setdiff("_run all.R") %>%
  .[!. %like% "\\[unused\\]"]

for (script in scripts) {
  print(script)
  source(paste0("scripts/", script))
}
