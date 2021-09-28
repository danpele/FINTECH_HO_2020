#################################################################
#### Spatial regression models to improve P2P credit risk management - Agosto, Giudici, Leach (2019) #####
#################################################################
# This code replicates the work in the Paper ####################
#################################################################

#################################################################
#- Clear Environment -#

rm(list=ls())
setwd("/home/rstudio")

#- Libraries -#

library(MASS)
library(numDeriv)
library(Rcpp)
library(RcppEigen)
library(microbenchmark)
library(optimx)
library(reshape2)
library(ROCR)

#- Source C++ functions to be used -#
sourceCpp('solve-cpp.cpp')

#- Source R functions to be used -#
source('Moments.R')
source('GMM_Optimization.R')



#################################################################
#- Data to read -#
# Set path directories and file names

#my_dir <- 'the name of your directory'
#path_to_data <- '\\Data\\'
data_WIOT_file <- '2014-Table 1.csv'
company_data_file <- 'sme_dataset.csv'
#path_to_save_results <- '\\Results\\'



# Get and manage WIOT data
col_classes <- c(NA, rep("numeric", 56))

# WIOT_flows_mat <- read.csv(paste(my_dir, path_to_data,data_WIOT_file,sep=""),
#                            colClasses = col_classes,
#                            row.names = 1)

WIOT_flows_mat <- read.csv(paste("Data/",data_WIOT_file,sep=""),
                           colClasses = col_classes,
                           row.names = 1)


WIOT_flows_mat[is.na(WIOT_flows_mat)] <- 0



# Set col/row names
col_names <- colnames(WIOT_flows_mat)
col_names <- gsub('\\.', '_', col_names)
colnames(WIOT_flows_mat) <- col_names
rownames(WIOT_flows_mat) <- col_names


#############################################################################

## Get the company specific data

# Assign sector codes to be used
col_names <- colnames(WIOT_flows_mat)
sector_codes <- col_names

data_company_spec <- read.csv(paste("Data/", company_data_file, sep=""),
                              sep = ";",
                              header = T)

# Match sectors in WIOT database and fix formats
data_company_spec <- data_company_spec[(data_company_spec$DPO > 0 & data_company_spec$DSO > 0), ]

data_company_spec$WIOT_CODE <- gsub('\\-', '_', data_company_spec$WIOT_CODE)

data_company_spec <- data_company_spec[data_company_spec$WIOT_CODE %in% 
                                         sector_codes,] 

data_company_spec <- droplevels(data_company_spec) 



#################################################################
#' Get W matrix
#'
#' Build the spatial weighting matrix 
#' 
#################################################################

# Calculate the proportion of each company outflows by sector
pp_outflow <- ave(data_company_spec$turnover,
                  data_company_spec$WIOT_CODE,
                  FUN=function(x) {x/sum(x)})

# Calculate the proportion of each company inflows by sector
pp_inflow <- ave(data_company_spec$turnover, 
                 data_company_spec$WIOT_CODE,
                 FUN=function(x) {x/sum(x)}) 

# Get matrix of individual proportions (R matrix)
in_out_mat <- outer(pp_outflow,pp_inflow)

# Get matrix of intersectoral flows from WIOT data (F matrix)
W_A2B <- WIOT_flows_mat[data_company_spec$WIOT_CODE, data_company_spec$WIOT_CODE]
diag(W_A2B) <- 0

# Multiply the two matrices elementwise to get the W matrix 
W_mat <- as.matrix(in_out_mat * W_A2B)

# Normalise rows
W_mat <- W_mat/apply(W_mat, 1, sum)

# Clear space 
rm(in_out_mat, W_A2B)

#################################################################
## Spatial Econometric Analysis
#' We want to estimate the following model
#' y = (I-rhoW)*X'beta + (I-rhoW)eps
#' Where:
#' y is a binary discrete choice variable
#' W is a spatial weighting matrix calculated above
#' X are the regressors
#' rho is the spatial parameter
#' beta are the covariate estimators
#' We estimate using a GMM model 
#' Z id the matrix of instruments (X, WX)
#' X consists of the following variables: ROE (ratio012), activity ratio (ratio018), solvency ratio (ratio027)
#' --------------------------------------------------------------
#' Method of moments are calculated in functions : Moment_Functions.R


#################################################################
# y = 1 if Default / 0 if Active
y <- data_company_spec$status

pct_def <- round(sum(y)/length(y), 4)*100

print(paste("Percentage of defaults is", pct_def,"%"))



  
### First run a basic logit regression

sample_data <- data_company_spec[, which(names(data_company_spec) %in% c('ratio012','ratio018','ratio027'))]

lg_model <- glm(y ~.,family=binomial(link='logit'), data = sample_data)
y_hat_logit <- lg_model$fitted.values
logit_summary <- summary.glm(lg_model)$coefficients
# write.csv(logit_summary, paste0(path_to_save_results,"results_logit",Sys.time(),".csv"))



k <- ncol(sample_data) #Number of columns of X
n <- nrow(sample_data) # Number of obs.


X <- matrix(c(  rep(1,n),
                data_company_spec$ratio012,
                data_company_spec$ratio018,
                data_company_spec$ratio027), ncol = k+1)
  
  
Z <- matrix(cbind(X, as.matrix(W_mat) %*% X), ncol = 2*(k+1))
Z <- Z[,-1]


# Set weighting matrix for GMM <- first step let S be Identity Matrix
S <- matrix(diag(ncol(Z)), 
            ncol=ncol(Z))

theta0 <- as.vector(c(0.2, -0.5, -0.5, -0.5, -0.5))  #initial parameter values

# Run one step to adjust initial values
Output_One_Step <- gmm_opt(obj_func = obj_func, theta=theta0, y = y,
                           X = X, Z = Z, Spatial_Weight = W_mat, Opt_Weight = S,
                           Steps = 'one')
Output_One_Step <- round(Output_One_Step, 3)
rownames(Output_One_Step) <- c("Estimates")
colnames(Output_One_Step) <- c("Rho","beta_0","beta_1","beta_2", "beta3")

theta <- unlist(Output_One_Step)


# Run two step 
Output_Two_Step <- gmm_opt(obj_func = obj_func, theta = theta, y = y, 
                           X = X, Z = Z, Spatial_Weight = W_mat, Opt_Weight = S,
                           Steps = 'two')
Output_Two_Step <- round(matrix(unlist(Output_Two_Step), nrow = 2, byrow = T),3)
rownames(Output_Two_Step) <- c("Estimates", "Standard Errors")
colnames(Output_Two_Step) <- colnames(Output_One_Step)
# write.csv(Output_Two_Step, file = paste(my_dir,path_to_save_results,'results.csv', sep=''))

theta <- unlist(Output_Two_Step)

#################################################################
#Model performance assessment
rho <- theta[1,1]
beta <- theta[1, 2:5]


B <- -rho * W_mat
diag(B) <- 1
x <- X %*% beta
C <- solveCppLU(B,x)
y_hat <- exp(C)/(1+exp(C))


y_pred_logit <- rep(0,n)
for (i in 1:n) {
  if (y_hat_logit[i] > 0.5) {
    y_pred_logit[i]=1}
}

y_pred <- rep(0,n)
for (i in 1:n) {
  if (y_hat[i] > 0.5) {
    y_pred[i]=1}
}



# Plot the ROC curves
preds <- cbind(p1 = y_hat_logit, 
               p2 = y_hat)

colors <- c('red', 'blue') 
for (i in 1:2) {
  plot(performance(prediction(preds[,i],y),"tpr","fpr"), 
       add=(i!=1),col=colors[i],lwd=2)
}
legend("topleft", legend=c("Logit model", "Spatial logit model"),
       col=c("red", "blue"), lty=1, cex=0.8)


# Calculate AUC
perf_logit <- performance(prediction(y_hat_logit,y), measure = 'auc')
perf <- performance(prediction(y_hat,y), measure = 'auc')

perf_logit@y.values
perf@y.values
