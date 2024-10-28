
library(tidyverse)
killian2008 %>% 
  pivot_longer(-date) %>% 
  ggplot(aes(date, value)) +
  geom_line() +
  facet_wrap(~name)


mdata <- killian2008[,-c(1,5)]

fstat(
  data = killian2008, 
  endog = "dprod", 
  inst = "proxy_supply", 
  exo = c("dprod", "rea", "rpo"), 
  lags = 12
)

estimate_var(mdata, p = 12) %>% 
  irf_psvar(m = killian2008$proxy_supply, shocksize = -20, irhor = 48) %>% 
  plot_psvar()

# Robust ------------------------------------------------------------------

fstat2(mdata, killian2008[,5], p = 12)
  
estimate_var(mdata, p = 12) %>% 
  irf_psvar_robust(m = killian2008$proxy_supply, shocksize = -20, irhor = 48) %>% 
  plot_psvar_robust()
