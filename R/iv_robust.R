

#' @export
irf_psvar_robust <- function(model, m, irhor = 20, clevel = 68, nboot = 500, shocksize = 1) {
  irs <- irf_psvar_single(model, m = m, irhor = irhor, shocksize = shocksize)
  birs <- irf_robust_psvar_single(model, m = m, clevel = clevel, irhor = irhor, shocksize = shocksize)
  list(irfs = irs, upper = birs[[1]], lower = birs[[2]])
}

#' @importFrom dplyr full_join
#' @importFrom purrr map2
#' @export
tidy_irf_psvar_robust <- function(x, names = NULL) { 
  cnames <- colnames(x$irfs)
  if(!is.null(names)) {
    cnames <- names
  }
  colnames(x[[1]]) <- colnames(x[[2]]) <- colnames(x[[3]]) <- cnames
  map(x, format_long) %>% 
    purrr::map2(c("irf", "lower", "upper"), ~ set_names(.x, c("horizon", "name", .y))) %>% 
    reduce(dplyr::full_join, c("horizon", "name"))
}


#' @export
plot_psvar_robust <- function(irf_obj, ncol = NULL, ...) {
  dots <- list(...)
  
  gg <- 
    ggplot(tidy_irf_psvar_robust(irf_obj, ...)) +
    # geom_hline(aes(yintercept = 0), linetype = "dotted") +
    geom_line(aes(horizon, irf)) +
    geom_line(aes(horizon, lower), linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(horizon, ymax = upper, ymin = lower), alpha = 0.2, fill = "grey50") +
    geom_line(aes(horizon, upper), linetype = "dashed", color = "grey50") +
    facet_wrap(~name, scales = "free", ncol = ncol) +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      axis.title = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank()
      panel.grid.major = element_line(linetype = "dashed")
    )
  if (!is.null(dots$sec_probs)) {
    gg <- gg +
      geom_line(aes(horizon, lower_sec), linetype = "dashed", color = "grey75") +
      geom_ribbon(aes(horizon, ymax = lower, ymin = lower_sec), alpha = 0.2, fill = "grey75") +
      geom_line(aes(horizon, upper_sec), linetype = "dashed", color = "grey75") +
      geom_ribbon(aes(horizon, ymax = upper_sec, ymin = upper), alpha = 0.2, fill = "grey75")
  }
  gg
}

irf_robust_psvar_single <- function(mdl, m, clevel = 68, irhor = 20, nboot = 500, shocksize = 1, nwlags = NULL) {
  
  p <- mdl$p
  X <- mdl$matX
  Y <- mdl$matY
  Z <-  as.matrix(m[-c(1:p)])
  eta <- t(mdl$res)
  if (is.null(nwlags)) {
    nwlags <- floor(4 * (((NROW(Y) - p)/100)^(2/9)))
  }
  What <- CovAhat_Sigmahat_Gamma(p, X, Z, eta, nwlags)
  tt <- NROW(Y)
  n <- NROW(eta)
  AL <- t(mdl$bet[-1,]) 
  Gamma <- eta %*% Z / tt
  
  Caux <- cbind( eye(n), ma_rep(AL, p, irhor - 1))
  C <- array(Caux, c(n, n, irhor)) 
  G <- Gmatrices(AL, Caux, p, irhor, n)
  
  W1  <- What[1:(n^2 * p),1:(n^2 * p)]
  W12 <- What[1:(n^2 * p), (1 + n^2 * p):NCOL(What)]
  W2  <- What[(1 + (n^2) * p):NROW(What), (1 + n^2 * p):NCOL(What)]
  
  for (jj in 1:length(clevel)) {
    critval <- qnorm(1 - ((1 - clevel[jj]/100)/2), 0,1)^2 
    
    e <- eye(n)
    ahat <- zeros(n,irhor) 
    bhat <- zeros(n,irhor)
    chat <- zeros(n,irhor)
    Deltahat <- zeros(n,irhor)
    MSWlbound <- zeros(n,irhor)
    MSWubound <- zeros(n,irhor)
    casedummy <- zeros(n,irhor)
    
    scale <-  shocksize
    
    for (j in 1:n) {
      for (ih in 1:irhor) {
        
        ahat[j,ih] <- tt * Gamma[1,1]^2 - critval %*% W2[1,1]
        bhat[j,ih] <- -2 * tt * scale * (t(e[,j]) %*% C[,,ih] %*% Gamma) * Gamma[1,1] + 
          2 * critval * scale * kronecker(t(Gamma), t(e[,j])) %*% G[,,ih] %*% W12[,1, drop = FALSE] +
          2 * critval * scale * t(e[,j]) %*% C[,,ih] %*% W2[,1, drop = FALSE]
        chat[j,ih] <- ((tt^.5) * scale * t(e[,j])  %*% C[,,ih] %*% Gamma)^2 - critval * 
          (scale^2) * kronecker(t(Gamma), t(e[,j])) %*% G[,,ih] %*% W1 %*% 
          t(kronecker(t(Gamma), t(e[,j])) %*% G[,,ih]) - 2 * critval * scale^2 * 
          kronecker(t(Gamma), t(e[,j])) %*% G[,,ih] %*% W12 %*% t(C[,,ih]) %*% e[,j, drop = FALSE] - critval * 
          scale^2 %*% t(e[,j]) %*% C[,,ih] %*% W2 %*% t(C[,,ih]) %*% e[,j]
        Deltahat[j,ih] = bhat[j,ih]^2 - (4 * ahat[j,ih] * chat[j,ih])
        
        if (ahat[j,ih] > 0 && Deltahat[j,ih] > 0) {
          casedummy[j,ih] = 1
          MSWlbound[j,ih] = (-bhat[j, ih] - (Deltahat[j,ih]^.5))/(2*ahat[j, ih]);
          MSWubound[j,ih] = (-bhat[j, ih] + (Deltahat[j,ih]^.5))/(2*ahat[j, ih]);
        } else if (ahat[j,ih] < 0 && Deltahat[j,ih] > 0) {
          casedummy[j,ih] = 2;
          MSWlbound[j,ih] = (-bhat[j,ih] + (Deltahat[j,ih]^.5))/(2*ahat[j, ih]);
          MSWubound[j,ih] = (-bhat[j,ih] - (Deltahat[j,ih]^.5))/(2*ahat[j, ih]);
        } else if (ahat[j,ih] > 0 && Deltahat[j,ih] < 0) {
          casedummy[j,ih] = 3;
          MSWlbound[j,ih] = NaN;
          MSWubound[j,ih] = NaN;
        } else {
          casedummy[j,ih] = 4;
          MSWlbound[j,ih] = -Inf;
          MSWubound[j,ih] = Inf;
        }
      }
    }
    MSWlbound[1,1] <- scale
    MSWubound[1,1] <- scale
  }
  
  irsH <- irsL <- array(NA, c(irhor, n, length(clevel)))
  irsL[,,jj] <- t(MSWubound)
  irsH[,,jj] <- t(MSWlbound)
  list(upper = irsH[,,1], lower = irsL[,,1])
}


#' @export
fstat2 <- function(y, z, p = 12, nwlags = NULL) {
  
  mdl <- estimate_var(y, p)
  X <- mdl$matX
  Y <- mdl$matY
  eta <- t(mdl$res)
  Z <-  as.matrix(z[-c(1:p),, drop = FALSE])
  if (is.null(nwlags)) {
    nwlags <- floor(4 * (((NROW(Y) - p)/100)^(2/9)))
  }
  
  WHat <- CovAhat_Sigmahat_Gamma(p, X, Z, eta, nwlags)
  tt <- NROW(Y)
  n <- NROW(eta)
  Gamma <- eta %*% Z / tt
  Fstat <- mrdivide((tt^(.5) * Gamma[1,1])^2 , WHat[n^2 * p + 1, n^2 * p + 1])
  drop(Fstat)
}


