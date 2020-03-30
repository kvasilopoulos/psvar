## code to prepare `DATASET` dataset goes here

library(readxl)
library(tidyverse)
library(lubridate)


# Ramey econ214 --------------------------------------------------------------

ramey_econ214 <- readxl::read_excel("data-raw/econ214_monetarydat.xlsx", sheet = 2, na = ".") %>%
  mutate(DATES = seq(as.Date("1969/2/1"), as.Date("2007/11/30"), by = "month"))
info_ramey_econ214 <- readxl::read_excel("data-raw/econ214_monetarydat.xlsx")
attr(ramey_econ214, "info") <- info_ramey_econ214

usethis::use_data(ramey_econ214, overwrite = TRUE)


# Ramey 2016 --------------------------------------------------------------

ramey2016 <- readxl::read_excel("data-raw/RAMEY_MACROECONOMICS_SHOCKS.xlsx", sheet = "Monthly") %>% 
  mutate(DATES = seq(as.Date("1959/1/1"), as.Date("2015/12/30"), by = "month"))
info_ramey2016 <- readxl::read_excel("data-raw/RAMEY_MACROECONOMICS_SHOCKS.xlsx", col_names = FALSE) %>% 
  set_names(c("Name", "Description")) %>% 
  drop_na(1)
attr(ramey2016, "info") <- info_ramey2016

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


