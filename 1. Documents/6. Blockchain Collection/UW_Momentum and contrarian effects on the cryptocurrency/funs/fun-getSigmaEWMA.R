getSigmaEWMA <- function(rtn, lambda = 0.96){
  ## Compute exponentially weighted moving average covariance matrix.
  ## If lambda <= 0, likelihood function is used to estimate lambda.

  if (!is.matrix(rtn)) rtn <- as.matrix(rtn)
  
  Mean <- apply(rtn, 2, mean)
  T <- dim(rtn)[1]
  k <- dim(rtn)[2]
  x <- rtn

  for (i in 1:k) {
    x[, i] <- rtn[, i] - Mean[i]
  }

  XX <<- x
  
  #
  if (lambda <= 0) {
    # perform QMLE of lambda
    ##library("mnormt")
    library("mvtnorm")
    par <- c(lambda = 0.96)
    S <- 10^{-6}
    lowerb <- c(lambda = S);
    upperb <- c(lambda = 1 - S)
    fit <- nlminb(start = par,
                  objective = MGAUS,
                  lower = lowerb,
                  upper = upperb)

    #
    epsilon <- 0.0001 * fit$par
    npar <- length(par)
    Hessian <- matrix(0,
                      ncol = npar,
                      nrow = npar)

    for (i in 1:npar) {
      for (j in 1:npar) {
        x1 = x2 = x3 = x4  = fit$par
        x1[i] <- x1[i] + epsilon[i]; x1[j] <- x1[j] + epsilon[j]
        x2[i] <- x2[i] + epsilon[i]; x2[j] <- x2[j] - epsilon[j]
        x3[i] <- x3[i] - epsilon[i]; x3[j] <- x3[j] + epsilon[j]
        x4[i] <- x4[i] - epsilon[i]; x4[j] <- x4[j] - epsilon[j]
        Hessian[i, j] <- (MGAUS(x1) - MGAUS(x2) - MGAUS(x3) + MGAUS(x4)) /
          (4 * epsilon[i] * epsilon[j])
      }
    }

    # Step 6: Create and Print Summary Report:
    se.coef <- sqrt(diag(solve(Hessian)))
    tval <- fit$par/se.coef
    matcoef <- cbind(fit$par, se.coef, tval, 2 * (1 - pnorm(abs(tval))))
    dimnames(matcoef) <- list(names(tval), c(" Estimate",
                                             " Std. Error",
                                             " t value",
                                             "Pr(>|t|)"))
    cat("\nCoefficient(s):\n")
    printCoefmat(matcoef, digits = 4, signif.stars = TRUE)
    lambda = fit$par
  }

  ##
  if (lambda > 0) {
    #h1=1-lambda; Sigt=cov(XX[1:(k+30),]); V1=c(Sigt)
    h1   <- 1 - lambda
    Sigt <- cov(XX)
    V1   <- c(Sigt)
    for (t in 2:T) {
      xx <- as.numeric(x[t - 1, ])
      for (i in 1:k) {
        Sigt[i, ] <- h1 * xx * xx[i] + lambda * Sigt[i, ]
      }
      V1 <- rbind(V1, c(Sigt))
    }
  }
  #

  EWMAvol <- list(Sigma.t = V1, return = rtn, lambda = lambda)
  return(EWMAvol)
}



##
MGAUS <- function(par){
  k <- dim(XX)[2]
  T <- dim(XX)[1]
  #lambda=par[1]; h1=1-lambda; Sigt=cov(XX[1:(k+30),])
  lambda <- par[1]
  h1 <- 1 - lambda
  Sigt <- cov(XX)
  lk <- 0
  for (t in 2:T) {
    xx <- as.numeric(XX[t - 1, ])
    for (i in 1:k) {
      Sigt[i, ] <- h1 * xx[i] * xx + lambda * Sigt[i, ]
    }
    ll <- dmvnorm(XX[t, ], rep(0, k), Sigt)
    lk <- lk - log(ll)
  }

  lk
}
