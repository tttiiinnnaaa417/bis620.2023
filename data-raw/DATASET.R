## code to prepare `DATASET` dataset goes here

accel = readRDS("accel.rds")
usethis::use_data(accel, overwrite = TRUE)

dl = readRDS("dl.rds")
usethis::use_data(dl, overwrite = TRUE)

accel = readRDS("adsl.rds")
usethis::use_data(adsl, overwrite = TRUE)

accel = readRDS("adlb.rds")
usethis::use_data(adlb, overwrite = TRUE)
