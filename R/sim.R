sim_var2 <- function(t = 100) {
  set.seed(1)
  # nc = 2
  p = 2
  k = 1
  A1 <- matrix(c(0.2, 0.5, 0, 0.2), 2, 2)
  A2 <- matrix(c(0.1, -.3, 0.2, 0.1), 2, 2)
  Y <- matrix(0, nrow = t, ncol = 2)
  B <- matrix(c(-0.592, 0.592, 0.806, 0.806), 2, 2)
  epsilon <- cbind(e1 = rnorm(t), e2 = rnorm(t))
  for (i in (p + 1):t) {
    Y[i, ] <- A1 %*% Y[i - 1, ] + A2 %*% Y[i - 2, ] + B %*% epsilon[i, ]
  }
  m <- 5 * epsilon[, 1] + rnorm(t)
  colnames(Y) <- c("y1", "y2")
  cbind(Y, m)
}
