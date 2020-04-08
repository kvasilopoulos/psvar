
# 
# estimate_var <- function(vardata, lags = 4) {
#   lmatrix <- lagmatrix(vardata, lags)
#   lhs <- as.matrix(vardata[-c(1:lags), ])
#   t <- nrow(lhs)
#   n <- ncol(lhs)
#   constant <- ones(t, 1) # constant
#   
#   rhs <- cbind(lmatrix, constant)
#   beta <-  reg_xy(rhs, lhs)
#   res <- lhs - rhs %*% beta
#   Sigma <- crossprod(res) / (t - n * lags - 1)
#   list(
#     lags = lags,
#     res = res,
#     beta = beta
#   )
# }


id_proxy <- function(model, mshocks) {
  
  res <- model$res
  lags <- model$lags
  nms <- colnames(res)
  t <- nrow(res)
  n <- ncol(res)
  k <- ncol(mshocks)
  
  if (k > n) {
    stop("the number of instruments must equal the number of shocks to be identified ")
  }
  
  SigmaU <- crossprod(res) / (t - n * lags - 1)
  SigmaU11 <- SigmaU[1:k, 1:k]
  SigmaU21 <- SigmaU[(k + 1):n, 1:k, drop = FALSE]
  SigmaU22 <- SigmaU[(k + 1):n, (k + 1):n]
  
  M <- cbind(ones(t, 1), mshocks) # include constant
  betaIV <- reg_xy(M, res)[-1,]
  betaIV1 <- betaIV[1:k,1:k] 
  betaIV2 <- betaIV[1:k, (k + 1):n]
  
  b21ib11 <- t(reg_xy(betaIV1, betaIV2))
  def <- SigmaU21 - b21ib11 %*% SigmaU11
  Z <- b21ib11 %*% SigmaU11 %*% t(b21ib11) - (SigmaU21 %*% t(b21ib11) + b21ib11 %*% t(SigmaU21)) + SigmaU22
  b12b12p <- t(SigmaU21 - b21ib11 %*% SigmaU11) %*% mat_div(Z, (SigmaU21 - b21ib11 %*% SigmaU11))
  b11b11p <- SigmaU11 - b12b12p
  
  b22b22p <- SigmaU22 - b21ib11 %*% b11b11p %*% t(b21ib11)
  # b22 <- t(chol(b22b22p))
  # b12 <- (t(SigmaU21 - b21ib11 %*% SigmaU11) + b12b12p %*% t(b21ib11)) %*% solve(t(b22))
  # b12ib22 <- b12 %*% inv(b22)

  b12ib22   = (t(SigmaU21- b21ib11%*%SigmaU11) + b12b12p %*% t(b21ib11)) %*% solve(t(b22b22p))
  
  b11iS1 <- inv(eye(k) - b12ib22 %*% b21ib11)
  b21iS1 <- b21ib11 %*% b11iS1
  S1S1p <- mat_div(b11iS1, b11b11p) %*% solve(t(b11iS1))
  S1 <- t(chol(S1S1p))
  # s1 <- t(chol(S1S1p[1,1]))
  # a <- S1S1p[2,1]/s1
  # s2 <-  t(chol(S1S1p[2,2] - a^2))
  # S1 <- cbind(
  #   rbind(s1, 0),
  #   rbind(a, s2)
  # )
  
  b1 <- rbind(b11iS1, b21iS1) %*% S1
  dimnames(b1) <- list(nms, nms[c(1:k)])
  
  # Fstat
  tempU <- res[,1:k] - mshocks %*% betaIV[,1:k]
  tempY <- mshocks %*% betaIV[,1:k] - mean(res[,1:k])
  kk <- k - 1
  Fmat <- (crossprod(tempY)/kk) %*% (crossprod(tempU)/(t-k-1))
  Fstat <- diag(Fmat)
  
  # realized shock sequences (Montiel-Olea, Stock and Watson)
  tempX <- cbind(ones(t,1), res)
  e <- tempX %*% reg_xy(tempX, mshocks)
  e <- apply(e, 2, function(x) x/sd(x))
  
  # Reliability
  
  b1
}

id_chol <- function(model) {
  lags <- model$lags
  res <- model$res
  nms <- colnames(res)
  t <- nrow(res)
  n <- ncol(res)
  SigmaU <- crossprod(res) / (t - n * lags - 1)
  B0_se <- t(chol(SigmaU))
  B0_unit <- B0_se/diag(B0_se)
  B0_unit
}


irf_proxy <- function(model, mshocks, n_imp = 20, shocksize = 1) {
  res <- model$res
  nms <- colnames(res)
  beta <- model$beta
  lags <- model$lags
  
  
  mshocks <- as.matrix(proxydata[-c(1:lags), , drop = FALSE])
  b1 <- id_proxy(model, mshocks)
  n <- ncol(res)
  
  irs <- matrix(0, nrow = lags + n_imp, n)
  colnames(irs) <- nms
  irs[lags + 1, ] = - b1[,1]/b1[1,1] * shocksize
  for (jj in 2:n_imp) {
    lvars <- t(irs[seq(lags + jj - 1, jj, -1), ])
    irs[lags + jj, ] <- t(c(lvars)) %*% beta[1:(lags * n), ]
  }
  irfs <- irs[-c(1:lags),]
  irfs
}

irf_chol <- function(model, n_imp = 20, shocksize = 1) {
  res <- model$res
  nms <- colnames(res)
  lags <- model$lags
  beta <- model$beta
  
  b1 <- id_chol(model)
  n <- ncol(res)
  
  irs <- matrix(0, nrow = lags + n_imp, n)
  colnames(irs) <- nms
  irs[lags + 1, ] = b1[,1]/b1[1,1] * shocksize
  for (jj in 2:n_imp) {
    lvars <- t(irs[seq(lags + jj - 1, jj, -1), ])
    irs[lags + jj, ] <- t(c(lvars)) %*% beta[1:(lags * n), ]
  }
  irfs <- irs[-c(1:lags),]
  irfs
}

# bootstrap ---------------------------------------------------------------


boot_chol <- function(model, vardata, nboot = 500, n_imp = 20) {
  res <- model$res
  beta <- model$beta
  lags <- model$lags
  t <- nrow(res)
  n <- ncol(res)
  nboot = 500
  n_imp = 20
  
  birs <- array(numeric(), c(n_imp, n, nboot))
  res_demean <- apply(res, 2, function(x) x - mean(x))
  for(jj in 1:nboot) {
    rr <- 1 - 2 * (runif(t) > 0.5)
    
    resb <- t(res_demean * (rr %*% ones(1, n)))
    
    varsb <- zeros(lags + t, n)
    varsb[1:lags, ] <- as.matrix(vardata[1:lags, ])
    
    for(j in (lags + 1):(lags + t)) {
      lvars <- t(varsb[seq(j - 1, j - lags, -1), ])
      varsb[j,] <- t(c(lvars)) %*% beta[1:(lags * n), ] +  beta[(lags * n + 1):nrow(beta), ] + t(resb[, j - lags])
    }
    
    modelb <- estimate_var(varsb)
    varbs <- irf_chol(modelb)
    
    birs[,,jj] <- varbs
  }
  birs
}

boot_proxy <- function(model, vardata, proxydata, nboot = 500, n_imp = 20) {
  res <- model$res
  beta <- model$beta
  lags <- model$lags
  mshocks <- as.matrix(proxydata[-c(1:lags), , drop = FALSE])
  t <- nrow(res)
  n <- ncol(res)
  k <- ncol(mshocks)
  
  # b1 <- id_proxy(model, mshocks)
  nboot = 500
  n_imp = 20
  birs <- array(numeric(), c(n_imp, n, nboot))
  # 
  # eb <- VAR$e* (rr %*% ones(1, VAR$n-VAR$k))
  # b11eTb = VAR$b11eT * (rr %*% ones(1, VAR$k))
  # 
  # resb =  rbind(eye(VAR$k), VAR$b21ib11) %*% t(b11eTb) + VAR$D[, (VAR$k + 1):VAR$n, 1] %*% t(eb)
  
  res_demean <- apply(res, 2, function(x) x - mean(x))
  for(jj in 1:nboot) {
    rr <- 1 - 2 * (runif(t) > 0.5)
    
    resb <- t(res_demean * (rr %*% ones(1, n)))
    
    varsb <- zeros(lags + t, n)
    varsb[1:lags, ] <- as.matrix(vardata[1:lags, ])
    
    for(j in (lags + 1):(lags + t)) {
      lvars <- t(varsb[seq(j - 1, j - lags, -1), ])
      varsb[j,] <- t(c(lvars)) %*% beta[1:(lags * n), ] +  beta[(lags * n + 1):nrow(beta), ] + t(resb[, j - lags]) #  +
    }
   
    modelb <- estimate_var(varsb)
    mshocksb <- mshocks * (rr %*% ones(1, k))
    varbs <- irf_proxy(modelb, mshocksb)
    
    birs[,,jj] <- varbs
  }
  birs
}



plot_irf <- function(x, y, alpha = 0.68) {
  ci <- 1 - alpha
  pr <- apply(y, c(1,2), quantile, probs = c(ci/2, 1 - ci/2), na.rm = TRUE)
  lower <- pr[1,,]
  upper <- pr[2,,]
  library(purrr)
  
  list(x, lower, upper) %>%
    map(`colnames<-`, colnames(x)) %>%
    map(format_long) %>%
    reduce(right_join, by = c("horizon", "name")) %>%
    arrange(name) %>% 
    set_names(c("horizon", "name", "irf", "lower", "upper")) %>%
    ggplot() +
    geom_hline(aes(yintercept = 0)) +
    geom_line(aes(horizon, irf)) +
    geom_line(aes(horizon, lower), linetype = "dashed") +
    geom_line(aes(horizon, upper), linetype = "dashed") +
    facet_wrap(~name, scales = "free_y") +
    theme_bw() +
    theme(
      strip.background = elem
    )
  
}

format_long  <- function(x) {
  as.data.frame(x) %>%
    mutate(horizon = 1:nrow(.)) %>%
    pivot_longer(-horizon)
}


