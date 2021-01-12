
# # factor_an_static(X) performs the Factor Analysis;
# The initial factor pattern is extracted using the principal component
# method, followed by a VARIMAX rotation to insure orthogonality of the factors.
factor_an_static <- function(X) 
{
  n1 = nrow(X)
  m  = colMeans(X, na.rm = FALSE, dims = 1)
  # standardizing data
  
  
  Rho=cor(X)
  
  
  std=repmat(sqrt((colVars(X))),n1,1)
  
  zz = as.matrix((X-repmat(m,n1,1))/std)
  Cor=cor(zz);
  
  
  # Principal Component Method with varimax rotation
  ev <- eigen(Cor)
  # extract components
  eigval <- ev$values
  eigvec<-ev$vectors
  
  E = ones(nrow(eigvec),3)%*%diag(eigval[1:3], 3, 3)
  
  # the estimated factor loadings matrix - the initial factor pattern
  Q     =sqrt(E)*eigvec[,1:3];
  
  
  
  # Rotating loadings
  ld=varimax(Q, normalize = TRUE, eps=0.0001)
  
  loadings=ld$loadings
  
  loadings[,2]=ld$loadings[,3];
  loadings[,3]=ld$loadings[,2];
  
  # f2 contains the standardized scoring coefficients;
  f2=inv(Rho)%*%loadings;
  
  # F contains the final scores after the varimax rotation;
  F=zz%*%f2
  return(list(F=F, loadings=loadings, f2=f2,eigval=eigval,eigvec=eigvec))
  
}
