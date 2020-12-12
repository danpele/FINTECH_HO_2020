rm(list = ls(all = TRUE))
graphics.off()
cat("\014")

### change working directory
setwd("/home/rstudio")

source("Run_Functions.R")


library("readxl")
library("readr")
library("clusterSim")
library("igraph")
library("DescTools")
library("ggplot2")
library("aod")
library("ROCR")
library("dplyr")
library("caret")
library("MASS")
library("stargazer")
library("xtable")
library("glmnet")
library("outliers")








###=====================================================================================##
###============= Load Data and Run Descriptive Statistics ==============================##
###=====================================================================================##

data <- read_excel("final_dataset_smes.xlsx", sheet=1)
data2 <- read_excel("final_dataset_smes.xlsx", sheet=2)
status <- as.matrix(data$status)

Xtemp <- as.matrix(data[,3:ncol(data)])
Xtemp[is.na(Xtemp)] <- 0

# #### status == 0 -> Non_Defaulted;  status == 1 -> Defaulted
Num_of_Defaulted <- length(which(status==1))
Num_of_Non_Defaulted <- length(which(status==0))

Defaulted_Percentage <- length(which(status==1))/ length(status)
Non_Defaulted_Percentage <- length(which(status==0))/ length(status)

X1 <- Xtemp[which(status==1),]
X0 <- Xtemp[which(status==0),]

# S1 <- apply(X1,2,function(x) sd(x))
M1 <- apply(X1,2,function(x) mean(x))
# S0 <- apply(X0,2,function(x) sd(x))
M0 <- apply(X0,2,function(x) mean(x))

#### TABLE 1: List and Summary Statistics of the variables in our sample.
Tab1 <- round(cbind(M0, M1), 2)
Tab2 <- cbind(data2[,2], Tab1)
Tab2


###=====================================================================================##
###=============== Estimate the factor network on the original dataset =================##
###=====================================================================================##

Y <- as.matrix(status)

#### Delete column of variables with zero variance
Var0 <- which( diag(var(Xtemp, na.rm = T)) == 0 )
if (length(Var0)!=0){
  Xtemp <- Xtemp[,-Var0]
}

# Standardize Dataset
X <- sweep(Xtemp, 2, colMeans(Xtemp), "-")
X <- sweep(X, 2, apply(X, 2 , sd), "/")

F_Net <- Estimate_Factor_Network(X)

##### TABLE 2: The eigenvalues of the singular value decomposition to determine the factors to retain.
EigenTab <- F_Net$EigenTab
EigenTab

P_G <- F_Net$P_G

####====================================================================##
####==================== Scoring models : Single-CSM   =================##
####====================================================================##
X <- as.matrix(data[,3:(ncol(data))])
X[is.na(X)] <- 0 

Single_CSM <- Lasso_Logistic_Regression(Y, X)

####====================================================================##
####===================== Scoring models : NetSeg-CSM   ================##
####====================================================================##

# threshold values
gamma_vec <- c(0.01, 0.05, 0.1)

Res <- NULL

for (t in 1:length(gamma_vec)) {
  G <- 1*(P_G > gamma_vec[t])
  links <- length(which(G == 1))
  
  w <- which(G==1, arr.ind=TRUE)
  cid <- unique(c(w[,1], w[,2]))
  ir <- order(cid)
  cid <- cid[ir]
  
  all_id <- c(1:ncol(G))
  ncid <- all_id[-cid]
  
  Conn_Default <- c(sum(status[cid]), mean(status[cid]))
  Non_Conn_Default <- c(sum(status[ncid]), mean(status[ncid]))
  Conn_Non_Default <- c(length(which(status[cid]==0)), length(which(status[cid]==0))/length(status[cid]))
  Non_Conn_Non_Default <- c(length(which(status[ncid]==0)), length(which(status[ncid]==0))/length(status[ncid]))
  Conn_Total <- c(length(cid), length(cid)/length(status))
  Non_Conn_Total <- c(length(ncid), length(ncid)/length(status))
  
  Stat_Net <- cbind(rbind(Conn_Default, Conn_Non_Default, Conn_Total), 
                    rbind(Non_Conn_Default, Non_Conn_Non_Default, Non_Conn_Total))
  rownames(Stat_Net) <- c("Default", "Non-Default", "Total")
  colnames(Stat_Net) <- c("Connected-Sub",  "Rate", "Non-Conn-Sub", "Rate")
  
  ####====================================================================##
  ####================ Scoring Connected Sub-Population   ================##
  ####====================================================================##
  
  Y.con <- as.matrix(Y[cid,])
  X.con <- X[cid,]
  
  Conn_Sub <- Lasso_Logistic_Regression(Y.con, X.con)
  
  ####====================================================================##
  ####================ Scoring Non-Connected Sub-Population ==============##
  ####====================================================================##
  
  Y.non <- as.matrix(Y[ncid,])
  X.non <- X[ncid,]
  
  Non_Conn_Sub <- Lasso_Logistic_Regression(Y.non, X.non)
  
  ####====================================================================##
  ####================ Combining Sub-Population Models  ==================##
  ####====================================================================##
  
  Net_Seg_CSM <- Combined_Model(Conn_Sub, Non_Conn_Sub)
  
  ####====================================================================##
  ####====================================================================##
  AUC <- c(Single_CSM$Perf$auc[[1]], Conn_Sub$Perf$auc[[1]], Non_Conn_Sub$Perf$auc[[1]], Net_Seg_CSM$Perf$auc[[1]])
  
  Confusion_Mat <- as.matrix(Net_Seg_CSM$ConfTab)
  
  Out <- list(AUC = AUC, Stat_Net = Stat_Net, Confusion_Mat = Confusion_Mat, Net_Seg_CSM = Net_Seg_CSM, 
              Conn_Sub = Conn_Sub, Non_Conn_Sub = Non_Conn_Sub)
  Res[[t]] <- Out
}


# Net_Seg_CSM_gam_0.01 <- Res[[1]]
# Net_Seg_CSM_gam_0.05 <- Res[[2]]
# Net_Seg_CSM_gam_0.1 <- Res[[3]]

#### TABLE 3: Summary statistic of connected and non-connected sub-population
rbind(Res[[1]]$Stat_Net, Res[[2]]$Stat_Net, Res[[3]]$Stat_Net)


#### TABLE 4: Comparing the Lasso logistic model estimated coefficients
round(cbind(Single_CSM$Coeff, Res[[3]]$Conn_Sub$Coeff, Res[[3]]$Non_Conn_Sub$Coeff), 6)



#### TABLE 5: Confusion matrices obtained from the prediction of the probability of default
cbind(Single_CSM$ConfTab, Res[[1]]$Confusion_Mat, Res[[2]]$Confusion_Mat, Res[[3]]$Confusion_Mat)


#### TABLE 6: Comparing model performance of the prediction of the probability of default
c(Single_CSM$Perf$auc[[1]], Res[[1]]$Net_Seg_CSM$Perf$auc[[1]], Res[[2]]$Net_Seg_CSM$Perf$auc[[1]], 
  Res[[3]]$Net_Seg_CSM$Perf$auc[[1]])



#### FIGURE 2: Plot of the ROC curves

colo <- c("black", "red", "blue","green3")

# pdf(paste0(savepath, paste("ROC_CURVES_COM.pdf")), width=6, height=4)
par(mar=c(5,4,1.8,0.5)) ### c(5, 4, 4, 2) + 0.1 # c(bottom, left, top, right)
plot(Single_CSM$Perf$roc, main="ROC Curves", col=colo[1], lwd=2)
plot(Res[[1]]$Net_Seg_CSM$Perf$roc, main="", col=colo[2], add = TRUE, lwd=2)
plot(Res[[2]]$Net_Seg_CSM$Perf$roc, main="", col=colo[3], add = TRUE, lwd=2)
plot(Res[[3]]$Net_Seg_CSM$Perf$roc, main="ROC Curves", xlab="False Positive Rate",
     ylab="True Positive Rate", col=colo[4], add = TRUE, lwd=2)
abline(0,1,col="grey", lty = 2, lwd=2)
legend("bottomright", legend=c(expression(paste(Single-CSM)), expression(paste(Net-Seg-CSM (gamma==0.01))), 
                               expression(paste(Net-Seg-CSM (gamma==0.05))), 
                               expression(paste(Net-Seg-CSM (gamma==0.1)))), 
       xpd = TRUE,  bty = "n", col=colo[1:4], lty=1,  lwd=3)
# dev.off()


# FIGURE 1: A graphical representation of the estimated factor network
G <- 1*(P_G > 0.05)
graph <- graph.adjacency(G)

# Properties of the graph
V(graph)$status <- status

V(graph)$color = ifelse(V(graph)$status==0, 'darkcyan','tomato')
graph <- as.undirected(graph)


Layout <- layout.auto(graph)

# Plot
#par(mar=c(0,0,0,0)) ###  c(bottom, left, top, right)
plot.igraph(graph, #layout= Layout,
            edge.arrow.width = 0.5,
            vertex.size = 1.8,
            vertex.label = NA,
            edge.arrow.size = 0.5,
            vertex.size2 = 3,
            vertex.label.cex = 1, asp = 0.55)

# Remove the nodes with no links
deg <- igraph::degree(graph)
Layout2 <- Layout[which(deg>0),]
deg2 <- deg[which(deg>0)]
graph_1=delete.vertices(graph,which(deg < 1))


# Default (red) -- Active (green)
V(graph_1)$color = ifelse(V(graph_1)$status==0, 'darkcyan','tomato')


# Plot
graph_1 <- as.undirected(graph_1)

plot.igraph(graph_1, #layout= Layout2,
            edge.arrow.width = 0.5,
            vertex.size = 1.8,
            vertex.label = NA,
            edge.arrow.size = 0.5,
            vertex.size2 = 3,
            vertex.label.cex = 1, asp = 0.55)
