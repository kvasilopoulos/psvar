
#' @export
psvar <- function(vardata, mshock, p = 12, irhor = 48, shocksize = 1) {
  
  VARNA <- list()
  VARNA$vars <- as.matrix(vardata)
  VARNA$p <- p
  t <- nrow(VARNA$vars)
  n <- ncol(VARNA$vars)
  VARNA$DET <- ones(t, 1)
  VARNA$irhor <- irhor
  VARNA$mshocks <- as.matrix(mshock)
  
  # Demean the shock
  VARNA$mshocks - mean(VARNA$mshocks, na.rm = TRUE)
  VARNA$mshocksize <- shocksize
  
  VARSLAGS <- lagmatrix(VARNA$vars, VARNA$p)
  VARS <- VARNA$vars[-c(1:VARNA$p), ]
  MSHOCKS <- VARNA$mshocks[-c(1:VARNA$p), , drop = FALSE]
  
  VARNA$t <- nrow(VARS)
  VARNA$n <- ncol(VARS)
  VARNA$k <- ncol(MSHOCKS)
  
  # Reduced-form VAR
  matX <- cbind(VARSLAGS, VARNA$DET[-c(1:VARNA$p), ])
  bet <- VARNA$bet <- reg_xy(matX, VARS)
  res <- VARS - matX %*% bet
  
  
  sigma <- VARNA$Sigma <- crossprod(res) / (VARNA$t - VARNA$n * VARNA$p - 1)
  Sig11 <- VARNA$Sigma[1:VARNA$k, 1:VARNA$k]
  Sig21 <- VARNA$Sigma[(VARNA$k + 1):VARNA$n, 1:VARNA$k, drop = FALSE]
  Sig22 <- VARNA$Sigma[(VARNA$k + 1):VARNA$n, (VARNA$k + 1):VARNA$n]
  
  # Narrative Identification
  LAMb = reg_xy(MSHOCKS, res)
  LAMb11  = LAMb[1:VARNA$k, 1:VARNA$k]
  LAMb21  = LAMb[1:VARNA$k, (VARNA$k + 1):VARNA$n]
  
  # Restriction
  b21ib11 <- t(reg_xy(LAMb11, LAMb21))
  ZZp <- b21ib11 %*% Sig11 %*% t(b21ib11) - (Sig21 %*% t(b21ib11) + b21ib11 %*% t(Sig21)) + Sig22
  b12b12p <- t(Sig21 - b21ib11 %*% Sig11) %*% mat_div(ZZp, (Sig21 - b21ib11 %*% Sig11))
  b11b11p <- Sig11 - b12b12p
  b22b22p <- ZZp + (Sig21 - b21ib11 %*% Sig11) %*% t(b21ib11) + b21ib11 %*% t(Sig21 - b21ib11 %*% Sig11) + b21ib11 %*% b12b12p %*% t(b21ib11)
  
  b22 <- t(chol(b22b22p))
  b12 <- (t(Sig21 - b21ib11 %*% Sig11) + b12b12p %*% t(b21ib11)) %*% solve(t(b22))
  b12ib22 <- b12 %*% inv(b22)
  
  # VARNA$sigmaG <- b22[1, 1]
  # VARNA$thetaG <- b12ib22[, 1]
  # VARNA$thetaY <- b12ib22[, 2]
  # VARNA$thetaW <- b12ib22[, 3]
  
  b11iSig <- inv(eye(VARNA$k) - b12ib22 %*% b21ib11)
  b21iSig <- b21ib11 %*% b11iSig
  
  # VARNA$gammaT <- t(b21iSig[1, ])
  
  # F2 <- b22[2, 1] / VARNA$sigmaG - b21iSig[2, ] %*% VARNA$thetaG
  # F1 <- t(-F2 %*% t(VARNA$gammaT) + (1 - t(VARNA$gammaT) %*% VARNA$thetaG) %*% b21iSig[2.])
  # VARNA$zetaT <- mat_div(eye(VARNA$k) %*% (1 - t(VARNA$gammaT) %*% VARNA$thetaG) + F1 %*% t(VARNA$thetaY), F1)
  # VARNA$zetaG <- mat_div((1 - t(VARNA$zetaT) %*% VARNA$thetaY), (1 - t(VARNA$gammaT) %*% VARNA$thetaG) %*% F2)
  # VARNA$sigmaY <- (1 - t(VARNA$zetaT) %*% VARNA$thetaY) %*% b22[2, 2]
  SigmaTSigmaTp <- mat_div(b11iSig, b11b11p) %*% solve(t(b11iSig))
  
  # Impulse Response to a monetary Shock
  VARNA$SigmaT <- array(numeric(), c(1, 1, VARNA$k))
  VARNA$D <- array(numeric(), c(VARNA$n, VARNA$n, 1))
  irs <- matrix(0, nrow = VARNA$p + VARNA$irhor, VARNA$n)
  VARNA$irs <- VARNA$irsTRY <- array(numeric(), c(VARNA$irhor, VARNA$n, VARNA$k))
  for (j in 1:VARNA$k) {
    ind <- 1:VARNA$k
    ind[VARNA$k] <- j
    ind[j] <- VARNA$k
    
    SSp <- SigmaTSigmaTp[ind, ind]
    SigmaT <- t(chol(SSp))
    
    VARNA$SigmaT[, , j] <- SigmaT[ind, ind]
    VARNA$D[, , j] <- rbind(
      cbind(b11iSig %*% VARNA$SigmaT[, , j], b12),
      cbind(b21iSig %*% VARNA$SigmaT[, , j], b22)
    )
    
    # normalize be multiplying with b11
    irs[VARNA$p + 1, ] <- VARNA$D[, j, j] %*% solve(VARNA$D[j, j, j]) %*% VARNA$mshocksize[j]
    for (jj in 2:VARNA$irhor) {
      lvars <- t(irs[seq(VARNA$p + jj - 1, jj, -1), ])
      irs[VARNA$p + jj, ] <- t(c(lvars)) %*% bet[1:(VARNA$p * VARNA$n), ]
    }
    VARNA$irs[, , j] <- irs[-c(1:VARNA$p), ]
    
    if (VARNA$k == 1) {
      VARNA$irsTRY <- VARNA$irs[, 1, ] - VARNA$irs[, 3, ]
    }
  }
  
  # Extract the structrual shocks
  e <- t(reg_xy(VARNA$D[,,1], t(res)))
  
  VARNA$et <- e[,1:VARNA$k]
  VARNA$e <- e[, (VARNA$k + 1):VARNA$n]
  
  VARNA$b11eT <- t(VARNA$D[1:VARNA$k, 1:VARNA$k, 1] %*% t(e[, 1:VARNA$k]))
  VARNA$b21ib11 <- b21ib11
  
  colnames(VARNA$irs) <- colnames(VARNA$vars)
  class(VARNA) <- "psvar"
  VARNA
}


psvar_boot <- function(vardata = ramey_econ214[, c(5, 2, 4, 6)],
                       mshock = ramey_econ214[, 10],
                       p = 12,
                       irhor = 20,
                       shocksize = 1,
                       nboot = 500
) {
  
  VAR <- psvar(vardata, mshock, p = p, shocksize = shocksize, irhor = irhor)
  birs <- array(numeric(), c(VAR$irhor, VAR$n, nboot, VAR$k))
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
    VARBS <- psvar(varsb, p = p, irhor = irhor, shocksize = shocksize, mshock = mshocksb)
    
    for(j in 1:VAR$k) {
      birs[,,jj,j] <- VARBS$irs[,,j]
    }
  }
  list(
    irs = VAR$irs,
    birs = birs
  )
}

format_long  <- function(x) {
  as.data.frame(x) %>%
    mutate(horizon = 1:nrow(.)) %>%
    pivot_longer(-horizon)
}

plot_psvar <- function(boot_obj, probs = c(0.34, 0.66)) {
  irs <- boot_obj$irs[,,1]
  birs <- boot_obj$birs[,,,1]
  
  pr <- apply(birs, c(1,2), quantile, probs = probs)
  lower <- pr[1,,]
  upper <- pr[2,,]
  
  library(purrr)
  
  list(irs, lower, upper) %>%
    map(`colnames<-`, colnames(irs)) %>%
    map(format_long) %>%
    reduce(full_join, by = c("horizon", "name")) %>%
    arrange(name) %>% 
    set_names(c("horizon", "name", "irf", "lower", "upper")) %>%
    ggplot() +
    geom_hline(aes(yintercept = 0)) +
    geom_line(aes(horizon, irf)) +
    geom_line(aes(horizon, lower), linetype = "dashed") +
    geom_line(aes(horizon, upper), linetype = "dashed") +
    facet_wrap(~name, scales = "free_y") +
    theme_bw()
}


