
linear      = function(Qy, Qxx, Qp, Qi, Qj) {
  fit       = qrL1(Qxx, Qy, Qp, 50)
  isnum     = which(fit$Cgacv == min(fit$Cgacv))
  lambda_in = (-fit$lambda[isnum])
  beta.in   = fit$beta[isnum, ]
  finalresults = list()
  finalresults$lambda.in = lambda_in
  return(finalresults)
}