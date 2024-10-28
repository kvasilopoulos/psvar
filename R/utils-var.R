


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

ma_rep <- function(AL, p, irhor) {
  n <- NROW(AL)
  vecAL <- array(AL, c(n, n, p))
  vecALrevT <- array(0, c(n, n, irhor))
  
  for (i in 1:irhor) {
    if (i <  irhor - p + 1) {
      vecALrevT[,,i] <- zeros(n,n)
    }else{
      vecALrevT[,,i] <- t(vecAL[,, irhor - i + 1])
    }
  }
  vecALrevT <- array(vecALrevT, c(n, n * irhor))
  C <- repmat(vecAL[,,1], 1, irhor)
  for (i in 1:(irhor - 1)) {
    C[, (n * i + 1):(n * (i + 1))] <- 
      cbind(eye(n), C[, 1:(n * i)]) %*% t(vecALrevT[, (irhor * n - n * (i + 1) + 1):NCOL(vecALrevT)])  
  }
  C
}

#' @importFrom expm %^%
Gmatrices <- function(AL, C, p, irhor, n) {
  
  ## A and J matrices in Lutkepohl's formula for the derivative of C with respect to A
  J <- cbind(eye(n), zeros(n, (p - 1) * n))
  Alut <- rbind(AL, cbind(eye( n * (p - 1)), zeros(n * (p - 1), n)))
  
  ## AJ is a 3D array that contains A^(k-1) J' in the kth 2D page of the the 3D array
  AJ <- array(0, c(n * p, n, irhor))
  for (k in 1:irhor) {
    AJ[,,k] = (Alut %^% (k - 1)) %*% t(J)
  }
  
  ## matrix [ JA'^0; JA'^1; ... J'A^{k-1} ];
  JAp = t(array(AJ, c(n * p, n * irhor)))
  
  ## G matrices
  AJaux = array(0, c(nrow(JAp) * n, ncol(JAp) * n, irhor))
  Caux = array(cbind(eye(n), C[,1:((irhor - 1) * n)]), c(n, n, irhor))
  for (i in 1:irhor) {
    AJaux[((n^2) * (i - 1) + 1):nrow(AJaux),,i] <- kronecker( JAp[1:(n * (irhor + 1 - i )),], Caux[,,i]) 
  }
  Gaux <- aperm(array(t(apply(AJaux, c(1,2) , sum)), c(n^2 * p, n^2, irhor)), c(2,1,3))
  Gdim <- dim(Gaux)
  G <- array(0, c(Gdim[1], Gdim[2], Gdim[3] + 1))
  G[,,2:dim(G)[3]] <- Gaux              
  return(G)
  ## Cumulative version of G matrices
  Gcum <- aperm(apply(G, c(1,2), cumsum), c(2,3,1))
  Gcum
}

NW_hac_STATA <- function(varss,lags) {
  Sigma0 <- 1/nrow(varss) * crossprod(varss)
  Sigma_cov <- function(k) {
    len <- nrow(varss)
    1/len * t(varss[1:(len - k),]) %*% varss[(1 + k):len,]
  } 
  Sigma <- Sigma0
  for (n in 1:lags) {
    Sigma <- Sigma + (1 - n/(lags + 1)) * (Sigma_cov(n) + t(Sigma_cov(n)))
  }
  Sigma
}

CovAhat_Sigmahat_Gamma <- function(p, X, Z, eta, lags) {
  
  n <- NROW(eta)
  k <- NCOL(Z)
  m <- NCOL(X) - (n * p)
  
  XSVARp <- X
  matagg <- t(cbind(XSVARp, t(eta), Z))
  
  T1aux <- NCOL(eta)
  T2aux <- NROW(matagg)
  
  etaaux <- array(eta, c(n, 1, T1aux))
  mataggaux <- aperm(array(matagg, c(T2aux, 1, T1aux)), c(2,1,3))
  
  temp <- array_times(etaaux,mataggaux)
  auxeta <- aperm(apply(temp, c(1,2), demean), c(2,3,1))
  
  vecAss1 <- array(auxeta, c((n * m) + (p * (n^2)) + n^2 + (n * k), 1, T1aux))
  AuxHAC1 <- vecAss1
  AuxHAC2 <- t(array(AuxHAC1, c(dim(vecAss1)[c(1,3)])))
  
  WhatAss1 <- NW_hac_STATA(AuxHAC2, lags = lags)
  
  # Construct the selector matrix Vaux that gives: vech(Sigma)=Vaux*vec(Sigma)
  I <- eye(n)
  V <- kronecker(I[1,,drop = FALSE], I)
  for (i_vars in 2:n) {
    V <- rbind(V, kronecker(I[i_vars,,drop = FALSE], I[i_vars:nrow(I),,drop = FALSE]))
  }
  # This is the estimator of What based on Montiel-Olea, Stock, and Watson
  Q1 <- crossprod(XSVARp) / T1aux
  Q2 <- t(Z) %*% XSVARp / T1aux
  Shat <- rbind(
    cbind(kronecker(mrdivide(cbind(zeros(n * p,m), eye(n * p)), Q1), eye(n)), zeros((n^2) * p, n^2 + (k * n))),
    cbind(zeros(n * (n + 1)/2,((n^2) * p) + n * m), V, zeros(n * (n + 1)/2, k * n)),
    cbind(-kronecker(mrdivide(Q2, Q1), eye(n)), zeros(k * n, n^2), eye(k * n) )
  )
  WHataux <- Shat %*% WhatAss1 %*% t(Shat)
  WHat <- rbind(
    cbind(
      WHataux[1:(n^2 * p), 1:(n^2 * p)], 
      WHataux[1:(n^2 * p),(n^2*p + (n * (n + 1)/2) + 1):NCOL(WHataux)]
    ),
    cbind(
      t(WHataux[1:(n^2 * p), (n^2 * p + n*(n + 1)/2 + 1):NCOL(WHataux)]),
      WHataux[(n^2 * p + n * (n + 1)/2 + 1):NCOL(WHataux), (n^2 * p +  n * (n + 1)/2 + 1):NCOL(WHataux)]
    )
  )
  WHat
}


