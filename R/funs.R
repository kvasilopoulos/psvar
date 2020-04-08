ones <- function(n, m = n) {
  stopifnot(is.numeric(n), length(n) == 1, is.numeric(m), length(m) == 1)
  n <- floor(n)
  m <- floor(m)
  if (n <= 0 || m <= 0) {
    return(matrix(1, 0, 0))
  } else {
    return(matrix(1, n, m))
  }
}

zeros <- function(n, m = n) {
  stopifnot(is.numeric(n), length(n) == 1, is.numeric(m), length(m) == 1)
  n <- floor(n)
  m <- floor(m)
  if (n <= 0 || m <= 0) {
    return(matrix(0, 0, 0))
  } else {
    return(matrix(0, n, m))
  }
}

eye <- function(n, m = n) {
  stopifnot(is.numeric(n), length(n) == 1, is.numeric(m), length(m) == 1)
  n <- floor(n)
  m <- floor(m)
  if (n <= 0 || m <= 0) {
    return(matrix(NA, 0, 0))
  } else {
    return(base::diag(1, n, m))
  }
}

#' Create a lagmatrix
#' @importFrom purrr set_names
tbl_lag <- function(x, dimension = 1) {
  temp <- vector("list", length = dimension)
  for (dims in 1:dimension) {
    temp[[dims]] <- purrr::map_df(x, dplyr::lag, dims) %>%
      rlang::set_names(glue::glue("{names(x)}_lag{dims}"))
  }
  purrr::reduce(temp, bind_cols)
}

mat_lag <- function(x, lags = 1) {
  dt <- as.matrix(x)
  nc <- ncol(dt)
  embed(dt, dimension = lags + 1)[, -c(1:nc), drop = FALSE]
}


#' Taken from pracma::pinv
pinv <- function(A, tol = .Machine$double.eps^(2/3))  {
  stopifnot(is.numeric(A), length(dim(A)) == 2, is.matrix(A))
  s <- svd(A)
  p <- (s$d > max(tol * s$d[1], 0))
  if (all(p)) {
    mp <- s$v %*% (1/s$d * t(s$u))
  }
  else if (any(p)) {
    mp <- s$v[, p, drop = FALSE] %*% (1/s$d[p] * t(s$u[, p, drop = FALSE]))
  }
  else {
    mp <- matrix(0, nrow = ncol(A), ncol = nrow(A))
  }
  return(mp)
}

#' Matlab mldivide (\) operator 
#' 
#' Solve systems of linear equations Ax = B for x
#' x = `A\\B` solves the system of linear equations A*x = B. 
#' The matrices A and B must have the same number of rows. 
#' MATLAB® displays a warning message 
#' * if A is badly scaled or nearly singular, but performs the calculation regardless.
#' If A is a scalar, then `A\\B` is equivalent to `A.\\B.`
#' If A is a square n-by-n matrix and B is a matrix with n rows, then x = `A\\B` is a solution to the equation A*x = B, if it exists.
#' If A is a rectangular m-by-n matrix with m ~= n, and B is a matrix with m rows, then `A\\B` returns a least-squares solution to the system of equations A*x= B.
mldivide <- function(A, B, pinv = TRUE) {
  stopifnot(is.numeric(A) || is.complex(A), is.numeric(B) ||  is.complex(B))
  if (is.vector(A)) 
    A <- as.matrix(A)
  if (is.vector(B)) 
    B <- as.matrix(B)
  if (nrow(A) != nrow(B)) 
    stop("Matrices 'A' and 'B' must have the same number of rows.")
  if (pinv) {
    pinv(t(A) %*% A) %*% t(A) %*% B
  }
  else {
    qr.solve(A, B)
  }
}

#' Matlab mrdivide (/) operator
#' 
#' Solve systems of linear equations xA = B for x
#' x = B/A solves the system of linear equations x*A = B for x. The matrices A and B must contain the same number of columns. MATLAB® displays a warning message if A is badly scaled or nearly singular, but performs the calculation regardless.
#' If A is a scalar, then B/A is equivalent to B./A.
#' If A is a square n-by-n matrix and B is a matrix with n columns, then x = B/A is a solution to the equation x*A = B, if it exists.
#' If A is a rectangular m-by-n matrix with m ~= n, and B is a matrix with n columns, then x = B/A returns a least-squares solution of the system of equations x*A = B.
mrdivide <- function(A, B, pinv = TRUE) {
  stopifnot(is.numeric(A) || is.complex(A), is.numeric(B) || is.complex(B))
  if (is.vector(A)) 
    A <- t(A)
  if (is.vector(B)) 
    B <- t(B)
  if (ncol(A) != ncol(B)) 
    stop("Matrices 'A' and 'B' must have the same number of columns.")
  t(mldivide(t(B), t(A), pinv = pinv))
}

inv <- function(a) {
  if (length(a) == 0) 
    return(matrix(0, nrow = 0, ncol = 0))
  if ((!is.numeric(a) && !is.complex(a)) || !is.matrix(a)) 
    stop("Argument 'a' must be a numeric or complex matrix.")
  if (nrow(a) != ncol(a)) 
    stop("Matrix 'a' must be square.")
  e <- try(b <- solve(a), silent = TRUE)
  if (inherits(e, "try-error")) {
    warning("Matrix appears to be singular.")
    b <- rep(Inf, length(a))
    dim(b) <- dim(a)
  }
  return(b)
}



right_div <- mat_div <- reg_xy <- function(x, y) {
  inv(crossprod(x)) %*% crossprod(x, y)
}

left_div <- function(x, y) {
  x %*% solve(y)
}