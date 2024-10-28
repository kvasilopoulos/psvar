
#' @export
estimate_var <- function(Y, p = 2) {
  YY <- as.matrix(Y)
  n <- ncol(YY)
  t <- nrow(YY)
  lmatY <- mat_lag(YY, lags = p)
  matY <- YY[-c(1:p), ]
  matX <- cbind(1, lmatY)
  
  # Run VAR
  bet <- mldivide(matX, matY)
  res <- matY - matX %*% bet
  sigmau <- crossprod(res) / (t - n * p - p - 1)
  
  list(
    matY = matY, 
    matX = matX,
    res = res, bet = bet,
    t = t, n = n, p = p,
    sigma = sigmau
  )
}

#' @export
calc_fstat <- function(Y, m, p = 2) {
  dt <- cbind(Y, m) 
  nms <- colnames(dt)
  idm <- length(nms)
  # all_fstat <- list()
  # for (i in 1:length(nms)) {
    all_fstat <- fstat(endog = nms[1], exo = nms[-idm], inst = nms[idm],  
                            lags = p, data = dt)
  # }
  all_fstat
}

# https://github.com/danmrc/weak-instruments-r


#' @importFrom sandwich vcovHC
#' @export
fstat <- function(endog, exo, inst, lags = 2, data) {
  
  exo_vars <- tbl_lag(data[,exo, drop = FALSE], dimension = lags)
  endo_vars <- data[,endog, drop = FALSE]
  inst_vars <- data[,inst, drop = FALSE]
  full_data <- cbind(endo_vars, inst_vars, exo_vars)
  rhs <- paste(c(inst, names(exo_vars)), collapse = " + ")
  form <- paste(endog, "~", rhs)
  
  model <- lm(formula = form, data = full_data)
  coefnames <- names(model$coefficients)
  idx <- which(inst ==  coefnames)
  vcov <- vcov(model) # sigma_v_sq / n
  # homoskedastic case
  vcov_n <- sandwich::vcovHC(model, type = "const") # sigma_v_sq  * Qzz_inv / n
  # heteroskedastic case
  vcov_r <-  sandwich::vcovHC(model, type = "HC1") # Sigma_pp
  coef_matrix <- matrix(coef(model))
  nomdf <- length(inst) # k ~ number of instruments
  # Cragg-Donald Wald F statistic) - Non Robust F statistic
  f_n <- crossprod(coef_matrix[idx], solve(vcov_n[idx, idx]) %*% coef_matrix[idx]) / nomdf
  # Kleibergen-Paap rk Wald F statistic - Robust F statistic
  f_r <- crossprod(coef_matrix[idx], solve(vcov_r[idx, idx]) %*% coef_matrix[idx]) / nomdf
  list("Fn" = f_n, "Fr" = f_r, "chi_crit" = 3.84) # qchisq(.95, df=1) 
  
}

# reliability <- function(model, m) {
#   
#   mres <- model$res
#   t <- nrow(mres)
#   n <- ncol(mres)
#   p <- model$p
#   bet <- model$bet
#   matM <- as.matrix(m)[-c(1:p),, drop = FALSE]
#   
#   # Identification
#   gamma <- crossprod(mres, matM) / t
#   
#   
#   Sigmm <- crossprod(matM)/ t
#   ED <- eye(k) * sum(sum(VAR.m,2)~=0)/tt
#   mu1 <- t(m) %*% res / t
#   PhiPhip <- mu1 %*% pinv(b11pb11p) * mu1
#   RM <- pinv(Sigmm) %*% PhiPhip %*% pinv(ED)
#   RMeigs <- sort(eigen(RM)$values)
#   list(RM = RM, RMeigs = RMeigs)
# }


irf_psvar_single <- function(model, m, irhor = 20, shocksize = -1, res_on = NULL) {
  
  # component <- match.arg(component)
  mres <- model$res
  t <- nrow(mres)
  n <- ncol(mres)
  p <- model$p
  bet <- model$bet
  
  matM <- as.matrix(m)[-c(1:p),, drop = FALSE]
  
  # Identification
  gamma <- crossprod(mres, matM) / t
  
  #  component = c("simple","reverse")
  # if(component == "reverse") {
  #   gamma <- eye(n, 1) - gamma
  # }
  
  # Impulse responses
  irs <- matrix(0, irhor + p, n, dimnames = list(NULL, colnames(model$matY)))
  irs[p + 1, ] <- shocksize * gamma / gamma[1]
  
  if(!is.null(res_on)) {
    bet[,res_on] <- 0
    irs[,res_on] <- 0
  }
  
  for (jj in 2:irhor) {
    lvars <- c(t(irs[(p + jj - 1):jj, ])) # collapse to vector
    irs[p + jj, ] <- lvars %*% bet[-1, ] # remove constant
  }
  irs <- irs[-c(1:p), ]
  irs
}

irf_boot_psvar_single <- function(model, m, irhor = 20, nboot = 500, shocksize = 1, res_on = NULL) {
  
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
                                     irhor = irhor, shocksize = shocksize, res_on = res_on)
  }
  birs
}

#' @export
irf_psvar <- function(model, m, irhor = 20, boot = TRUE, nboot = 500, shocksize = 1, res_on = NULL) {
  irs <- irf_psvar_single(model, m = m, irhor = irhor, shocksize = shocksize, res_on = res_on)
  if(boot) {
    birs <- irf_boot_psvar_single(model, m = m, irhor = irhor, shocksize = shocksize, res_on = res_on)
    dimnames(birs) <- list(NULL, colnames(irs), NULL)
  }else{
    birs <- NULL
  }
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
  
  out <- bind_cols(dplyr::arrange(irfs, name), boot_dist) %>% 
    mutate(
      lower = map_dbl(distr, quantile, probs = probs[1]),
      upper = map_dbl(distr, quantile, probs = probs[2])
    )
  if (!is.null(sec_probs)) {
    out <- out %>% 
      mutate(
        lower_sec = map_dbl(distr, quantile, probs = sec_probs[1]),
        upper_sec = map_dbl(distr, quantile, probs = sec_probs[2])
      )
  }
  out
}

#' @importFrom ggplot2 ggplot aes geom_line geom_hline geom_ribbon
#' @importFrom ggplot2 facet_wrap theme_bw theme element_blank element_line
#' @export
plot_psvar <- function(irf_obj, ncol = NULL, ...) {
  dots <- list(...)
  
  gg <- 
    ggplot(tidy_irf_psvar(irf_obj, ...)) +
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
  if(!is.null(dots$sec_probs)) {
    gg <- gg +
      geom_line(aes(horizon, lower_sec), linetype = "dashed", color = "grey75") +
      geom_ribbon(aes(horizon, ymax = lower, ymin = lower_sec), alpha = 0.2, fill = "grey75") +
      geom_line(aes(horizon, upper_sec), linetype = "dashed", color = "grey75") +
      geom_ribbon(aes(horizon, ymax = upper_sec, ymin = upper), alpha = 0.2, fill = "grey75")
  }
  gg
}
