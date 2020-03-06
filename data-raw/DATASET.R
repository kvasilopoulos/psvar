## code to prepare `DATASET` dataset goes here

library(readxl)
library(tidyverse)
library(lubridate)


# Ramey 2016 --------------------------------------------------------------

ramey2016 <- readxl::read_excel("data-raw/econ214_monetarydat.xlsx", sheet = 2, na = ".") %>% 
  mutate(DATES = seq(as.Date("1969/2/1"), as.Date("2007/11/30"), by = "month"))
info_ramey_2016 <- readxl::read_excel("data-raw/econ214_monetarydat.xlsx")
attr(ramey2016, "info") <- info_ramey_2016

usethis::use_data(ramey2016, overwrite = TRUE)

# Mertens and Ravn 2013 ---------------------------------------------------

mr2013_quarterly <- readxl::read_excel("data-raw/MR_AER_DATASET.xlsx", sheet = 1) %>% 
  mutate(DATES = seq(as.Date("1950/1/1"), as.Date("2006/12/31"), by = "quarter"))
info_mr2013_quarterly <- readxl::read_excel("data-raw/MR_AER_DATASET.xlsx", sheet = 3, skip = 7) %>% 
  slice(-1) %>% 
  select(1,3,10) %>% 
  drop_na()
attr(mr2013_quarterly, "info") <- info_mr2013_quarterly

mr2013_annual <- readxl::read_excel("data-raw/MR_AER_DATASET.xlsx", sheet = 2) %>% 
  mutate(DATES = ymd(DATES, truncated = 2))
info_mr2013_annual <- readxl::read_excel("data-raw/MR_AER_DATASET.xlsx", sheet = 3, skip = 33) %>% 
  select(1,3,10)
attr(mr2013_annual, "info") <- info_mr2013_annual

mr2013 <- list(
  mr2013_quarterly,
  mr2013_annual
  )
names(mr2013) <- c("quarterly", "annual")

usethis::use_data(mr2013, overwrite = TRUE)
