
#' @export
estimate_var <- function(Y, p = 2) {
  YY <- as.matrix(Y)
  n <- ncol(YY)
  t <- nrow(YY)
  lmatY <- lagmatrix(YY, lags = p)
  matY <- YY[-c(1:p), ]
  matX <- cbind(1, lmatY)
  
  # Run VAR
  bet <- mldivide(matX, matY)
  res <- matY - matX %*% bet
  sigmau <- crossprod(res) / (t - n * p - p - 1)
  
  list(
    matY = matY, 
    res = res, bet = bet,
    t = t, n = n, p = p
  )
  
}

irf_psvar_single <- function(model, m, irhor = 20, shocksize = -1) {
  
  mres <- model$res
  t <- nrow(mres)
  n <- ncol(mres)
  p <- model$p
  bet <- model$bet
  matM <- as.matrix(m)[-c(1:p),, drop = FALSE]
  
  # Identification
  gamma <- crossprod(mres, matM) / t
  
  # Impulse responses
  irs <- matrix(0, irhor + p, n, dimnames = list(NULL, colnames(model$matY)))
  irs[p + 1, ] <- shocksize * gamma / gamma[1]
  for (jj in 2:irhor) {
    lvars <- c(t(irs[(p + jj - 1):jj, ])) # collapse to vector
    irs[p + jj, ] <- lvars %*% bet[-1, ] # remove constant
  }
  irs <- irs[-c(1:p), ]
  irs
}

irf_boot_psvar_single <- function(model, m, irhor = 20, nboot = 500, shocksize = 1) {
  
  # demean by column - wb not needed but MR have it
  mres <- apply(model$res, 2, function(y) y - mean(y)) 
  t <- nrow(mres)
  n <- ncol(mres)
  bet <- model$bet
  p <- model$p
  matM <- as.matrix(m)[-c(1:p),, drop = FALSE]

  birs <- array(0, dim = c(irhor, n, nboot))
  varsb <- matrix(0, p + t, n)
  varsb[1:p, ] <- model$matY[1:p, ] # initial values
  for (jj in 1:nboot) {
    wb <- sample(size = t, c(-1, 1), replace = T)
    resb <- mres * wb
    for (j in (p + 1):(p + t)) {
      lvars <- c(t(varsb[(j - 1):(j - p), ]))
      varsb[j, ] <- lvars %*% bet[-1,] + bet[1, ] + resb[(j - p), ]
    }
    mboot <- c(matM[1:p], matM[-c(1:p)] * wb[-c(1:p)])
    model <- estimate_var(varsb[-c(1:p),], p = p)
    birs[, , jj] <- irf_psvar_single(model, m = mboot,
                                     irhor = irhor, shocksize = shocksize)
  }
  birs
}

#' @export
irf_psvar <- function(model, m, irhor = 20, nboot = 500, shocksize = 1) {
  irs <- irf_psvar_single(model, m = m, irhor = irhor, shocksize = shocksize)
  birs <- irf_boot_psvar_single(model, m = m, irhor = irhor, shocksize = shocksize)
  dimnames(birs) <- list(NULL, colnames(irs), NULL)
  list(irfs = irs, boot_irfs = birs)
}



#' @importFrom tibble enframe
#' @importFrom purrr map_dbl
#' @importFrom dplyr bind_cols
#' @importFrom dplyr select rename arrange
#' @export
tidy_irf_psvar <- function(x, names = NULL, 
                           probs = c(0.16, 0.84), sec_probs = c(0.05, 0.95)) {
  if (!is.null(names)) {
    colnames(x$irfs) <- names
  }
  irfs <- x$irfs %>% 
    format_long() %>% dplyr::rename("irf" = value)
  boot_dist <- array_to_list(x$boot_irfs, margin = c(1,2)) %>%
    enframe() %>% select(distr = value)
  
  bind_cols(dplyr::arrange(irfs, name), boot_dist) %>% 
    mutate(
      lower = map_dbl(distr, quantile, probs = probs[1]),
      upper = map_dbl(distr, quantile, probs = probs[2]),
      lower_sec = map_dbl(distr, quantile, probs = sec_probs[1]),
      upper_sec = map_dbl(distr, quantile, probs = sec_probs[2])
    )
}

#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_ribbon
#' @importFrom ggplot2 facet_wrap theme_bw theme element_blank element_line
#' @export
plot_psvar <- function(irf_obj, title = NULL, ...) {
    ggplot(tidy_irf_psvar(irf_obj, ...)) +
    # geom_hline(aes(yintercept = 0), linetype = "dotted") +
    geom_line(aes(horizon, irf)) +
    geom_line(aes(horizon, lower), linetype = "dashed", color = "grey50") +
    geom_ribbon(aes(horizon, ymax = upper, ymin = lower), alpha = 0.2, fill = "grey50") +
    geom_line(aes(horizon, upper), linetype = "dashed", color = "grey50") +
    geom_line(aes(horizon, lower_sec), linetype = "dashed", color = "grey75") +
    geom_ribbon(aes(horizon, ymax = lower, ymin = lower_sec), alpha = 0.2, fill = "grey75") +
    geom_line(aes(horizon, upper_sec), linetype = "dashed", color = "grey75") +
    geom_ribbon(aes(horizon, ymax = upper_sec, ymin = upper), alpha = 0.2, fill = "grey75") +
    facet_wrap(~name, scales = "free") +
    theme_bw() +
    theme(
      strip.background = element_blank(),
      axis.title = element_blank(),
      panel.grid.minor = element_blank(),
      # panel.grid.major = element_blank()
      panel.grid.major = element_line(linetype = "dashed")
    )
}
