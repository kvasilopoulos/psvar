library(tidyverse)



boot_obj <- psvar_boot(vardata = ramey_econ214[, c(5, 2, 4, 6)], mshock = ramey_econ214[, 10], p = 12, irhor = 50)
plot_psvar(boot_obj, probs = c(0.1, 0.9))


# Gertler Karadi ----------------------------------------------------------

attributes(ramey2016)$info %>% 
  print(n = 42)

pdata <- ramey2016 %>% 
  select(DATES, GS1, LIP, EBP, LCPI, FF4_TC) %>% 
  drop_na() %>% 
  filter(DATES >= "1991-01-01") 

boot_obj <- psvar_boot(pdata[, 2:5], pdata[,6], p = 4, shocksize = 0.2)
plot_psvar(boot_obj, probs = c(0.1, 0.9))


# mr2013 ------------------------------------------------------------------

library(tidyverse)

vdata <- mr2013$quarterly %>% 
  select(APITR, ACITR, PITB, CITB, GOV, RGDP, DEBT)

mshock<- mr2013$quarterly %>% 
  select(m_PI)

psvar(vardat = vdata, mshock = mshock, p = 4)

boot_obj <- psvar_boot(vdata, mshock, p = 4, shocksize = -1)
plot_psvar(boot_obj, probs = c(0.1, 0.9))


