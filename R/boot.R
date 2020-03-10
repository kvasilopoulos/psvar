

psvar_boot <- function(data = ramey2016[, c(5, 2, 4, 6)],
                       mshock = ramey2016[, 10],
                       p = 12,
                       irhor = 48,
                       shocksize = 1
) {
  x <- psvar(data = ramey2016[, c(5, 2, 4, 6)], mshock = ramey2016[, 10])
  
  
  VAR <- VARNA
  
  
  nboot = 500
  birs <- array(numeric(), c(VARNA$irhor, VARNA$n, nboot, VARNA$k))
  for(jj in 1:nboot) {
    rr <- 1 - 2 * (runif(VAR$t) > 0.5)
    eb <- VAR$e* (rr %*% ones(1, VAR$n-VAR$k))
    b11eTb = VAR$b11eT * (rr %*% ones(1, VAR$k))
    
    resb =  rbind(eye(VAR$k), VAR$b21ib11) %*% t(b11eTb) + VAR$D[, (VAR$k + 1):VAR$n, 1] %*% t(eb)
    
    varsb = zeros(VAR$p+VAR$t, VAR$n)
    varsb[1:VAR$p, ] = VAR$vars[1:VAR$p, ]
    for(j in (VAR$p + 1):(VAR$p + VAR$t)) {
      lvars <- t(varsb[seq(j - 1, j - VAR$p, -1), ])
      varsb[j,] <- t(c(lvars)) %*% VAR$bet[1:(VAR$p * VAR$n), ] +
        VAR$DET[j, ] %*% VAR$bet[(VAR$p * VAR$n + 1):nrow(VAR$bet), ] +
        t(resb[, j - VAR$p])
    }
    
    mshocksb <- rbind(VAR$mshocks[1:VAR$p,, drop = FALSE], VAR$mshocks[-c(1:VAR$p),] * (rr %*% ones(1, VAR$k)))
    VARBS <- psvar(varsb, p = p, irhor = 48, shocksize = 1, mshock = mshocksb)
    
    for(j in 1:VAR$k) {
      birs[,,jj,j] <- VARBS$irs[,,j]
    }
  }
  
  
  pr <- apply(birs[,,,1], c(1,2), quantile, probs = c(0.1, 0.90))
  lower <- pr[1,,]
  upper <- pr[2,,]
  
  
  
  
  format_long  <- function(x) {
    as.data.frame(x) %>%
      mutate(horizon = 1:nrow(.)) %>%
      pivot_longer(-horizon)
  }
  
  
  library(purrr)
  
  list(x$irs[,,1], lower, upper) %>%
    map(`colnames<-`, colnames(x$irs[,,1])) %>%
    map(format_long) %>%
    reduce(right_join, by = c("horizon", "name")) %>%
    set_names(c("horizon", "name", "irf", "lower", "upper")) %>%
    ggplot() +
    geom_hline(aes(yintercept = 0)) +
    geom_line(aes(horizon, irf)) +
    geom_line(aes(horizon, lower), linetype = "dashed") +
    geom_line(aes(horizon, upper), linetype = "dashed") +
    facet_wrap(~name, scales = "free_y") +
    theme_bw()
  
  
  
}

