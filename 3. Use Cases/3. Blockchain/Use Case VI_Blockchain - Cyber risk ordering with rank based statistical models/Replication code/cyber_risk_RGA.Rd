#################################
##### RGA INDEX COMPUTATION #####
#################################

grindex<-function(y,yhat){   
    ly<-length(y)
    ryhat<-rank(yhat,ties.method="average") # ranks of the estimated values
    support<-tapply(y,ryhat,mean)
    myrord<-NULL
    lsupport<-length(support)
    matrice<-matrix(NaN,lsupport,ly)
    names(ryhat)<-c(1:ly)
    for (jj in 1:lsupport)
      {
      matrice[jj,as.numeric(names(ryhat[ryhat==names(support[jj])]))]<-support[jj]
      }         
    myrord<-colSums(matrice,na.rm=TRUE)
    ystar<-myrord[order(yhat)] 
    n<-length(y)                                             
    I<-1:length(y)
    i<-I/n
    cumystar<-cumsum(ystar)/sum(y)
    grnum<-(cumystar-i)^2
    grindex<-sum(grnum/i)
    grindex
}

####################################
##### RGA MAX INDEX COMPUTATION ####
####################################

grindexmax<-function(y,yhat){ 
	yhat<-sort(yhat,decreasing=FALSE) 
    y<-sort(y,decreasing=FALSE)
	ystar<-y[order(yhat)] 
    n<-length(y)                                             
    I<-1:length(y)
    i<-I/n
    cumystar<-cumsum(ystar)/sum(y)
    grnum<-(cumystar-i)^2
    grindexmax<-sum(grnum/i)
    grindexmax
}

####################################################
### ORDERED DEPENDENT VARIABLE CASE - CYBER RISK ###
####################################################

cybrisk<-read.csv("path/cyber_risk_analysis.csv",sep=";",dec=".")

dim(cybrisk)

attach(cybrisk)

colnames(cybrisk) # names of variables

str(cybrisk)

######################
##### FULL MODEL #####
######################

Severity_rank<-rank(Severity,ties.method="min") # substituting the target variable ordered categories into ranks

n<-length(Severity_rank)

Lin_mod_full<-lm(Severity_rank~Attaccante+Bersaglio+Tecnica+Continente,data=cybrisk) # linear model built on the whole dataset

summary(Lin_mod_full) # results
#R2=0.6183
#Adjusted R-squared=0.602
#p-value<2.2e-16

cybrisk_hat_full<-fitted(Lin_mod_full)

RMSE<-sqrt((1/n)*sum((cybrisk_hat_full-Severity_rank)^2))
RMSE
#105.1964

grindex_full<-grindex(Severity_rank,cybrisk_hat_full)
grindex_full
#63.1848

grindexmax_full<-grindexmax(Severity_rank,cybrisk_hat_full)
grindexmax_full
#85.49169

grindexnorm_full<-grindex_full/grindexmax_full
grindexnorm_full
#0.7390754

#########################
##### REDUCED MODEL #####
#########################

Lin_mod_reduced<-lm(Severity_rank~Bersaglio+Tecnica+Continente,data=cybrisk) # linear model built on the reduced dataset

summary(Lin_mod_reduced) # results
#R2=0.4806
#Adjusted R-squared=0.4606
#p-value<2.2e-16

cybrisk_hat_reduced<-fitted(Lin_mod_reduced)

RMSE<-sqrt((1/n)*sum((cybrisk_hat_reduced-Severity_rank)^2))
RMSE
#122.7062

grindex_reduced<-grindex(Severity_rank,cybrisk_hat_reduced)
grindex_reduced
#47.42577

grindexmax_reduced<-grindexmax(Severity_rank,cybrisk_hat_reduced)
grindexmax_reduced
#85.49169

grindexnorm_reduced<-grindex_reduced/grindexmax_reduced
grindexnorm_reduced
#0.5547413

