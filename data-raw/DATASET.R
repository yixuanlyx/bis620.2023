## code to prepare `DATASET` dataset goes here
library(haven)
library(purrr)
trial_path = file.path("data-raw","NCT00364013")
trial_files = list.files(trial_path, pattern = "*_pds2019.sas7bdat")

dl = map(file.path(trial_path, trial_files), ~ read_sas(.x))
names(dl) = gsub("*_pds2019.sas7bdat", "", trial_files)
usethis::use_data(dl, overwrite = TRUE)

## code to prepare `DATASET` dataset goes here

accel = readRDS("accel.rds")
usethis::use_data(accel, overwrite = TRUE)
