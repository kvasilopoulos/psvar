ones <- function(n, p = NULL) {
  if (missing(p)) {
    matrix(1, nrow = n, ncol = n)
  }else{
    matrix(1, nrow = n, ncol = p)
  }
}


eye <- function(n, p = NULL) {
  if (missing(p)) {
    diag(1, nrow = n, ncol = n)
  }else{
    diag(1, nrow = n, ncol = p)
  }
}

lagmatrix <- function(x, lags = 1) {
  dt <- as.matrix(x)
  nc <- ncol(dt)
  embed(dt, dimension = lags + 1)[, -c(1:nc), drop = FALSE]
}

inv <- function(x) {
  # solve(x)
  # qr.solve(x)
  chol2inv(chol(x))
}

mat_div <- reg_xy <- function(x, y) {
  inv(crossprod(x)) %*% crossprod(x, y)
}

right_div <- function(x, y) {
  inv(crossprod(x)) %*% crossprod(x, y)
}