################################################################
## Functions for the GMM Estimators to the Binary Spatial ######
## Regression Model are kept in this file ######################
################################################################
#' Moments
#' -----------------
#' Generate the moment conditions need the weighting 
#' A = (I-rho*W)^-1 matrix
#' m(theta)
#' -----------------
#' Objective Function
#' -----------------
#' The function to be minimised
#' m(theta)' %*% S %*% m(theta)
#' Needs CPP packages and functions
#' 
################################################################

obj_func <- function(theta, X, Z, W, y, weighting_mat) {
  n <- nrow(X)
  rho <- theta[1]
  beta <- theta[-1]
  B <- -rho * W
  diag(B) <- 1
  x <- X %*% beta
  C <- solveCppLU(B,x)
  f_x <- exp(C)/(1+exp(C))
  u <- y - f_x
  m <- as.vector(crossprod(Z,u))/n
  return(as.numeric(tcrossprod(crossprod(m, weighting_mat), m)))
}

gradients <- function(theta, X, Z, W, y, weighting_mat){
  n <- nrow(X)
  rho <- theta[1]
  beta <- as.vector(theta[-1])
  B <- -rho * W 
  diag(B) <- 1 #(I-rho W)
  x <- X %*% beta
  C <- solveCppLU(B,x) # Solve for deriv half (I-rho*W)^-1*X*beta
  D <- solveMatCppLU(B,W) # Solve for derivative half (I-rho*W)^-1*W
  E <- D %*% C
  f_x <- exp(C)/(1+exp(C))
  u <- y - f_x
  m <- as.vector(crossprod(Z,u))/n
  f_xx <- exp(C)/((1+exp(C)^2))
  G <- solveMatCppLU(B,X) # solve for the beta returns n*k matrix
  rho_grad <- f_xx * E # get rho grads
  beta_grad <- f_xx * G # get grads
  grads <- cbind(rho_grad, beta_grad) # bind gradients 
  D_inf <- -crossprod(Z,grads)/n # derive s_n(theta) = 2*D(theta)*S*m(theta)
  return(list(gradients = crossprod(D_inf, weighting_mat) %*% m,
              jacobian = D_inf))# multiply
}

moments <- function(theta, X, Z, W, y) {
  n <- nrow(X)
  k <- ncol(Z)
  rho <- theta[1]
  beta <- theta[-1]
  B <- -rho * W
  diag(B) <- 1
  x <- X %*% beta
  C <- solveCppLU(B,x)
  f_x <- exp(C)/(1+exp(C))
  u <- y - f_x
  return(list(moments = as.vector(crossprod(Z,u)),
              ms = Z * matrix(u, nrow = length(u), ncol = k, byrow = F)))
}

moments_test <- function(theta, X, Z, W, y) {
  n <- nrow(X)
  k <- ncol(Z)
  rho <- theta[1]
  beta <- theta[-1]
  B <- -rho * W
  diag(B) <- 1
  x <- X %*% beta
  C <- solveCppLU(B,x)
  f_x <- exp(C)/(1+exp(C))
  u <- y - f_x
  return(as.vector(crossprod(Z,u)/n))
}