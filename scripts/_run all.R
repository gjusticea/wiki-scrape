source("utils/functions.R")

scripts <- list.files("scripts") |>
  setdiff("_run all.R")

for (script in scripts) {
  source(paste0("scripts/", script))
}
