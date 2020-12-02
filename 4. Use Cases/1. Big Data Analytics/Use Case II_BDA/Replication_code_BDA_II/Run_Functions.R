Estimate_Factor_Network <- function(X)
{
  nvar <- ncol(X)
  nobs <- nrow(X)
  cx <- sweep(X, 2, colMeans(X), "-")
  sv <- svd(cx)
  
  U <- sv$u
  D <- sv$d
  
  eig_values <- sv$d^2/(nobs - 1)
  variance.explained <- 100*prop.table(sv$d^2)
  #ratio_pc <- round(100*(sv$d^2/sum(sv$d^2)), 2)
  cumperc <- cumsum(variance.explained)
  EigenTab <- round(cbind(eig_values, variance.explained, cumperc), 2)
  
  idk <- which( cumperc >= 95 )
  
  k <- idk[1]
  
  # Obtain Factors
  Fx <- U[,1:k]%*%diag(D[1:k])/sqrt(nvar)

  theta <- qnorm(2/(nobs-1))
  #theta <- 0
  EZ <- Fx %*% t(Fx) + theta
  
  # Link prediction
  P_G <- pnorm(EZ, mean = 0, sd = 1, lower.tail = TRUE, log.p = FALSE)
  
  # set the diagonal do zero
  diag(P_G) <- 0
  
  Out <-list(P_G = P_G, EigenTab = EigenTab)
}

# Lasso Logistic Regression 
Lasso_Logistic_Regression <- function(Y, X)
{
  colnames(Y) <- c("status")
  
  # Model 
  set.seed(12345)
  nobs <- nrow(Y)
  perc <- floor(0.7*nobs)
  ord <- sample(nobs)
  
  y.train <- as.matrix(Y[ord[1:perc], ])
  y.test <- as.matrix(Y[ord[(perc+1):nobs], ])
  
  x.train <- X[ord[1:perc], ]                
  x.test <- X[ord[(perc+1):nobs], ]
  
  # perform grid search to find optimal value of lambda
  # family= binomial => logistic regression, alpha=1 => lasso
  cvfit <- cv.glmnet( x.train, y.train, alpha=1, family ="binomial", type.measure = "mse", nfolds=10 )
  
  EstMdl <- glmnet(x.train, y.train, alpha = 1, family ="binomial", lambda=cvfit$lambda.min)
  
  #regression coefficients
  s="lambda.min"
  Mdl_Coef <- as.matrix(coef(EstMdl, s))
  
  EstMdl$Pred_prob <- predict(EstMdl, newx = x.test, s, type = "response")
  EstMdl$Pred_status <- factor(1*(EstMdl$Pred_prob > 0.5))
  
  EstMdl$Act_status <- factor(as.matrix(y.test))
  
  # Create Confusion Matrix
  levels(EstMdl$Pred_status) <- c("0", "1")
  ConfMat <- confusionMatrix(data=EstMdl$Pred_status, reference= EstMdl$Act_status, positive='1')
  
  # #confusion matrix
  Tab <- table(pred=EstMdl$Pred_status,true = EstMdl$Act_status)
  # 
  # #accuracy
  ACC <- mean(EstMdl$Pred_status==EstMdl$Act_status)
  
  obj <- prediction(predictions = EstMdl$Pred_prob, labels = EstMdl$Act_status)
  Perf <- list(obj = obj)
  
  # Get data for ROC curve
  Perf$roc <- performance(Perf$obj, measure="tpr", x.measure="fpr")
  Perf_auc <- performance(Perf$obj, measure = "auc")
  Perf$auc <- Perf_auc@y.values
  Perf$ACC <- ACC
  
  Out <-list(EstMdl = EstMdl, ConfMat = ConfMat, Perf = Perf, ConfTab = Tab, Coeff = Mdl_Coef)
}



Combined_Model <- function(Conn_Sub, Non_Conn_Sub)
{
  Pred_prob <- c(Conn_Sub$EstMdl$Pred_prob, Non_Conn_Sub$EstMdl$Pred_prob)
  Segmented_Model <- list(Pred_prob = Pred_prob)
  Act_status <- c(Conn_Sub$EstMdl$Act_status, Non_Conn_Sub$EstMdl$Act_status)-1
  Segmented_Model$Act_status <- factor(Act_status)
  
  #translate probabilities to Predicted status Variable.
  Segmented_Model$Pred_status <- factor(1*(Segmented_Model$Pred_prob > 0.5))
  
  # Create Confusion Matrix
  levels(Segmented_Model$Pred_status) <- c("0", "1")
  Segmented_Model$ConfMat <- confusionMatrix(data=Segmented_Model$Pred_status, 
                                          reference = Segmented_Model$Act_status, positive='1')
  Segmented_Model$ConfTab <- table(pred=Segmented_Model$Pred_status,true = Segmented_Model$Act_status)
  # Tab1 <- table(pred=Mdl_pred_status,true = df.testset$status)
  
  obj <- prediction(predictions = Segmented_Model$Pred_prob, labels = Segmented_Model$Act_status)
  Segmented_Model$Perf <- list(obj = obj)
  
  # Get data for ROC curve
  Segmented_Model$Perf$roc <- performance(Segmented_Model$Perf$obj, measure="tpr", x.measure="fpr")
  Segmented_Model$Perf_auc <- performance(Segmented_Model$Perf$obj, measure = "auc")
  Segmented_Model$Perf$auc <- Segmented_Model$Perf_auc@y.values
  Segmented_Model$Perf$ACC <- mean(Segmented_Model$Pred_status==Segmented_Model$Act_status)
  return(Segmented_Model)
}