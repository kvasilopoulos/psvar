## code to prepare `DATASET` dataset goes here

library(readxl)

ramey <- readxl::read_excel("data-raw/econ214_monetarydat.xlsx", sheet = 2, na = ".")

usethis::use_data(ramey)
