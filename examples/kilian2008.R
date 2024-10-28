fstat(
  data = killian2008, 
  endog = "dprod", 
  inst = "proxy_supply", 
  exo = c("dprod", "rea", "rpo"), 
  lags = 12
)

vardata_killian2008 <- select(killian2008)

estimate_var()