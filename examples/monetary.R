library(tidyverse)

attributes(ramey2016)$info %>% 
  print(n = 42)

pdata <- ramey2016 %>% 
  select(DATES, GS1, LIP, EBP, LCPI, FF4_TC) %>% 
  drop_na() %>% 
  filter(DATES >= "1991-01-01") 

psvar(pdata[, 2:5], mshock = pdata[,6], shocksize = 0.2) %>% 
  autoplot()


