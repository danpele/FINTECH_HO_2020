################################################################
#' All optimization for GMM is wrapped into this
#' function
################################################################

gmm_opt <- function(obj_func, 
                    gr_func = NULL, 
                    theta, y, X, Z, 
                    Spatial_Weight, 
                    Opt_Weight, 
                    Steps = 'one',
                    all_opt = FALSE,
                    opt_method = 'BFGS',
                    Newey_West = F) {
  
  # Argument checks
  if(!(Steps %in% c('one','two','inf'))) stop("Steps must be either: 'one','two' or 'inf'")
  if(!(is.function(obj_func))) stop("A function must be supplied for obj_func ")
  # if(!(is.na(gr_func))) stop("A function must be supplied for gr_func ")
  
  if(all_opt){
    print('This option is for testing different optimisation methods and compare results - best to used on smaller samples 
          and will only use one step')
    Out <- optimx(par=theta, fn=obj_func,
                  control=list(all.methods = TRUE, save.failures=TRUE, maxit=1000, trace = 3, reltol = 1e-6),
                  weighting_mat = Opt_Weight, X = X,
                  Z = Z, W = Spatial_Weight, y = y)
    return(Out)
  } else {
    
    j <- ncol(Z)
    
    if(Steps == 'one'){
      
      Out <- optimx(par=theta, fn=obj_func,
                    method = opt_method,
                    control=list(save.failures=TRUE, maxit=1000, trace = 3, reltol = 1e-6),
                    weighting_mat = Opt_Weight, X = X,
                    Z = Z, W = Spatial_Weight, y = y)
      
      k <- length(theta)
      estimates <- Out[1:k]
      return(estimates)
      
    } else if(Steps == 'two'){
      
      Out <- optimx(par=theta, fn=obj_func,
                    method = opt_method,
                    control=list(save.failures=TRUE, maxit=1000, trace = 3, reltol = 1e-6),
                    weighting_mat = Opt_Weight, X = X,
                    Z = Z, W = Spatial_Weight, y = y)
      
      k <- length(theta)
      estimates <- Out[1:k]
      attributes(estimates) <- NULL
      estimates <- unlist(estimates)
      
      ## Get Opt Weighting Mat
      m <- moments(estimates, 
                   X = X, Z = Z, y = y,
                   W = Spatial_Weight)
      ms <- m$ms
      Omega <- crossprod(ms)/n
      S_opt <- try(chol2inv(chol(Omega)))
      
      if (class(S_opt) == "try-error"){
        cat("Caught an error using Cholesky decomposition inverting the matrix. It may not be positive definite.
            Trying alternative method... \n")
        S_opt <- try(solve(Omega))
        if(class(S_opt) == "try-error"){
          cat("Caught an error using standard solver the matrix is most likely singular. Trying alternative method... \n")
          eye <- matrix(diag(j), ncol = j)  
          S_opt <- solveMatCppLU(as.matrix(Omega), eye)
          if(class(S_opt) == "try-error"){
            stop("The matrix could not be inverted for two-step estimation")
          }
        } 
      }

      
      Out <- optimx(par = estimates, fn=obj_func,
                    method = opt_method,
                    control=list(save.failures=TRUE, maxit=1000, trace = 3, reltol = 1e-6),
                    weighting_mat = S_opt, X = X,
                    Z = Z, W = Spatial_Weight, y = y)
      
      k <- length(theta)
      estimates <- Out[1:k]
      attributes(estimates) <- NULL
      estimates <- unlist(estimates)
      
      # Standard Errors
      # D <- matrix(unlist(gradients(estimates, X=X, Z=Z, y=y, W=W_mat, weighting_mat = S_opt)['jacobian']), ncol = k) THESE NOT COMPUTED CORRECTLY WHEN COMPARED WITH NUMERICAL SO USE NUMERICAL FOR NOW
      D <- jacobian(moments_test, x = estimates, X = X, Z =Z , y =y , W = W_mat)
      m <- moments(estimates, X=X, Z=Z, y=y, W=W_mat) 
      ms <- m$ms
      if(Newey_West){
        lags <- round(n^(1/3))
        Om <- NeweyWest(ms,lags)}
      else {
        Om <- crossprod(ms)/n
      } 
      
      S_opt <- try(chol2inv(chol(Om)))
      
      if (class(S_opt) == "try-error"){
        cat("Caught an error using Cholesky decomposition inverting the matrix. It may not be positive definite.
            Trying alternative method...")
        S_opt <- try(ginv(Om))
        if(class(S_opt) == "try-error"){
          cat("Caught an error using ginv, trying alternative method...")
          eye <- matrix(diag(j), ncol = j)  
          S_opt <- solveMatCppLU(as.matrix(Om), eye)
          if(class(S_opt) == "try-error"){
            stop("The matrix could not be inverted for two-step estimation")
          }
        }
      }
      
      VCov_Mat <- try(chol2inv(chol((crossprod(D,S_opt) %*% D))))
      if (class(VCov_Mat) == "try-error"){
        cat("Caught an error using Cholesky decomposition inverting the matrix. It may not be positive definite.
            Trying alternative method...")
        VCov_Mat <- try(ginv(crossprod(D,S_opt) %*% D))
        if(class(VCov_Mat) == "try-error"){
          cat("Caught an error using ginv, trying alternative method...")
          eye <- matrix(diag(j), ncol = j)  
          VCov_Mat <- solveMatCppLU(as.matrix(crossprod(D,S_opt) %*% D), eye)
          if(class(S_opt) == "try-error"){
            stop("The matrix could not be inverted for two-step estimation")
          }
        }
      }
      
      VCov_Mat <- VCov_Mat/n
      V_hat_1  <- sqrt(diag(VCov_Mat))
      
      return(list(estimates = estimates,
                  std_errors = V_hat_1))
      
    } else {
      
      print("To be implemented...")  
      
      }
    
  }
        
}

NeweyWest <- function(Z,bandwith){
  
  # % Returns the Newey-West estimator of the asymptotic variance matrix
  # % INPUTS: Z, a nxk matrix with rows the vector zt'
  # %         nlags, the number of lags
  # %
  # % OUTPUTS: omegahat, the Newey-West estimator of the covariance matrix
  n <- nrow(Z)
  k <- ncol(Z)
  # de-mean the variables
  Z <- scale(Z, scale = F)
  omegahat = crossprod(Z)/n
  if(bandwith > 0){
    # Sample autocovs
    for(i in 1:bandwith){
      Zlag = Z[1:(n-i),]
      ZZ = Z[(i+1):n,]
      gamma = crossprod(ZZ,Zlag)/n
      weight = 1 - (i/(bandwith+1));
      omegahat = omegahat + weight*(gamma + t(gamma))
    }
  }
  return(omegahat)
}

