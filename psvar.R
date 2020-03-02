library(readxl)

info <- readxl::read_excel("econ214_monetarydat.xlsx")
DATA <- readxl::read_excel("econ214_monetarydat.xlsx", sheet = 2) # as.matrix()

source("funs.R")

psvar(data = DATA[, c(5, 2, 4, 6)], mshock = DATA[, 10])

# Data preparation --------------------------------------------------------


VARNA <- list()
VARNA$vars <- as.matrix(DATA[, c(5, 2, 4, 6)])
VARNA$p <- 12
t <- nrow(VARNA$vars)
n <- ncol(VARNA$vars)
VARNA$DET <- ones(t, 1)
VARNA$irhor <- 48
VARNA$mshocks <- as.matrix(DATA[, 10])

# Demean the shock
VARNA$mshocks - mean(VARNA$mshocks, na.rm = TRUE)

DAT <- list()
DAT$TRY <- 0.1746 # Average ratio of federal tax revenues to GDP
DAT$GY <- 0.0982 # Average ratio of federal expenditures to GDP

VARNA$mshocksize <- 1
VARNA$gshocksize <- 0.01 / DAT$GY # left in because one of the other programs needs it


# Main Function -----------------------------------------------------------

VARSLAGS <- lagmatrix(VARNA$vars, VARNA$p)
VARS <- VARNA$vars[-c(1:VARNA$p), ]
MSHOCKS <- VARNA$mshocks[-c(1:VARNA$p), , drop = FALSE]

VARNA$t <- nrow(VARS)
VARNA$n <- ncol(VARS)
VARNA$k <- ncol(MSHOCKS)

# Reduced-form VAR
matX <- cbind(VARSLAGS, VARNA$DET[-c(1:VARNA$p), ])
bet <- reg_xy(matX, VARS)
res <- VARS - matX %*% bet


sigma <- VARNA$Sigma <- crossprod(res) / (VARNA$t - VARNA$n * VARNA$p - 1)
Sig11 <- VARNA$Sigma[1:VARNA$k, 1:VARNA$k]
Sig21 <- VARNA$Sigma[(VARNA$k + 1):VARNA$n, 1:VARNA$k, drop = FALSE]
Sig22 <- VARNA$Sigma[(VARNA$k + 1):VARNA$n, (VARNA$k + 1):VARNA$n]

# Narrative Identification
LAMb = reg_xy(MSHOCKS, res)
LAMb11  = LAMb[1:VARNA$k, 1:VARNA$k]
LAMb21  = LAMb[1:VARNA$k, (VARNA$k+1):VARNA$n]

# Restriction
b21ib11 <- t(reg_xy(LAMb11, LAMb21))
ZZp <- b21ib11 %*% Sig11 %*% t(b21ib11) - (Sig21 %*% t(b21ib11) + b21ib11 %*% t(Sig21)) + Sig22
b12b12p <- t(Sig21 - b21ib11 %*% Sig11) %*% mat_div(ZZp, (Sig21 - b21ib11 %*% Sig11))
b11b11p <- Sig11 - b12b12p
b22b22p <- ZZp + (Sig21 - b21ib11 %*% Sig11) %*% t(b21ib11) + b21ib11 %*% t(Sig21 - b21ib11 %*% Sig11) + b21ib11 %*% b12b12p %*% t(b21ib11)


b22 <- t(chol(b22b22p))
b12 <- (t(Sig21 - b21ib11 %*% Sig11) + b12b12p %*% t(b21ib11)) %*% solve(t(b22))
b12ib22 <- b12 %*% inv(b22)


VARNA$sigmaG <- b22[1, 1]
VARNA$thetaG <- b12ib22[, 1]
VARNA$thetaY <- b12ib22[, 2]
VARNA$thetaW <- b12ib22[, 3]

b11iSig <- inv(eye(VARNA$k) - b12ib22 %*% b21ib11)
b21iSig <- b21ib11 %*% b11iSig

VARNA$gammaT <- t(b21iSig[1, ])

F2 <- b22[2, 1] / VARNA$sigmaG - b21iSig[2, ] %*% VARNA$thetaG
F1 <- t(-F2 %*% t(VARNA$gammaT) + (1 - t(VARNA$gammaT) %*% VARNA$thetaG) %*% b21iSig[2.])
VARNA$zetaT <- mat_div(eye(VARNA$k) %*% (1 - t(VARNA$gammaT) %*% VARNA$thetaG) + F1 %*% t(VARNA$thetaY), F1)
VARNA$zetaG <- mat_div((1 - t(VARNA$zetaT) %*% VARNA$thetaY), (1 - t(VARNA$gammaT) %*% VARNA$thetaG) %*% F2)
VARNA$sigmaY <- (1 - t(VARNA$zetaT) %*% VARNA$thetaY) %*% b22[2, 2]
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
  
  b11 <- b11iSig %*% VARNA$SigmaT[, , j]
  b21 <- b11iSig %*% VARNA$SigmaT[, , j]
  VARNA$D[, , j] <- rbind(
    cbind(b11, b12),
    cbind(b21, b22)
  )

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

# % Reliability
# %%%%%%%%%%%%%%

# m = MSHOCKS;
# ED    = eye(VAR$k) %*% sum(sum(m,2)~=0)/VAR.T;
# mu1   = m'*res(:,1:VAR.k)/VAR.T;
# b11 = sqrt(b11b11p);
#
# Bi = [1/b11+(Sig21-Sig11*b21ib11)'*inv(ZZp)/b11*b21ib11 -(Sig21-Sig11*b21ib11)'*inv(ZZp)/b11];
# VAR.et = (Bi*res')'; % Estimated monetary Shocks
#
# PHI = mu1/b11;
# GAM = inv(ED)*PHI;
# E  = GAM*VAR.et(sum(m,2)~=0);
# V  = m(sum(m,2)~=0)-E;
# VAR.RM = inv(E'*E+V'*V)*E'*E;
