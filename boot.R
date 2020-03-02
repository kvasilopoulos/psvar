
VAR <- psvar(data = DATA[, c(5, 2, 4, 6)], mshock = DATA[, 10])

jj = 1



rr <- 1 - 2 * (runif(VAR$t) > 0.5)

eb     =  VAR.e.*(rr*ones(1,VAR.n-VAR.k));

