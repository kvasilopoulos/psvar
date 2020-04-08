library(tidyverse)


dat <- econdata::sw2001[,-1]
x <- estimate_var(dat) %>% irf_chol()
y <- estimate_var(dat) %>% 
  boot_chol(dat, nboot = ) 
plot_irf(x, y)


plot_psvar2(ramey_econ214[, c(5, 2, 4, 6)], ramey_econ214[, 10], p = 12, irhor = 50)

boot_obj <- psvar_boot(vardata = ramey_econ214[, c(5, 2, 4, 6)], mshock = ramey_econ214[, 10], p = 12, irhor = 50)
plot_psvar(boot_obj, probs = c(0.05, 0.95))

# Gertler Karadi ----------------------------------------------------------

attributes(ramey2016)$info %>% 
  print(n = 42)

pdata <- ramey2016 %>% 
  dplyr::select(DATES, GS1, LIP, EBP, LCPI, FF4_TC) %>% 
  tidyr::drop_na() %>% 
  dplyr::filter(DATES >= "1991-01-01") 

estimate_var(pdata[, 2:5], p = 12) %>% 
  irf_psvar(pdata[,6], shocksize = 0.2, irhor = 50) %>% 
  plot_psvar2()


plot_psvar2(pdata[, 2:5], pdata[,6], p = 4, shocksize = 0.2, irhor = 50)

boot_obj <- psvar_boot(pdata[, 2:5], pdata[,6], p = 4, shocksize = 0.2)
plot_psvar(boot_obj, probs = c(0.1, 0.9))

Y = as.matrix(pdata[, 2:5])
m = as.matrix(pdata[,6], , drop = FALSE)



# mr2013 ------------------------------------------------------------------

library(tidyverse)

vdata <- mr2013$quarterly %>% 
  select(APITR, ACITR, PITB, CITB, GOV, RGDP, DEBT)

vdata %>% 
  mutate(seq = 1:nrow(.)) %>% 
  pivot_longer(-seq) %>% 
  ggplot(aes(seq, value)) +
  geom_line() +
  facet_wrap(~name, scales = "free_y")


mshock <- mr2013$quarterly %>% 
  select(m_PI)

boot_obj <- psvar_boot(vdata, mshock, p = 4, shocksize = -1)
plot_psvar(boot_obj, probs = c(0.1, 0.9))


