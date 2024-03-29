#################################################################
#### A Statistical Classification of Cryptocurrencies
##Pele, Wesselhoft, Hardle, Kolossiatis, Yatracos (2020) #####
# #################################################################
# Name of Quantlet: SFA_cryptos
# ################################################
# Description: 'Performes Factor Analysis on a dataset of 23 variables, 
#  describing cryptos, stocks, FX and commodities.'
# 
# Keywords: cryptocurrency, genus proximum, classiffication, multivariate analysis, 
#  factor models
# 
# Author: Daniel Traian Pele

#################################################################
#- Clear Environment -#
rm(list = ls())
graphics.off()

#setwd("D:\\PROIECTE\\HORIZON 2020\\Use Case DP\\rstudio-export\\SFA_Cryptos")
setwd("/home/rstudio/SFA_Cryptos/")

#Packages
# install.packages("kernlab")
# install.packages("fBasics")
# install.packages("stringr")
# install.packages("e1071")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("MASS")
# install.packages("tidyverse")
# install.packages("pracma")
# install.packages("reshape2")
# install.packages("scatterplot3d")
# install.packages("devtools")
# install.packages("qcc")
# install.packages("qicharts2")
# install.packages("factoextra")
# install.packages("caret")
# install.packages("rms")

#- Source R functions to be used -#
source('kmeans_opt.R')
source('factor_an_static.R')

#- Libraries -#
library(rms)
library(caret)
library(factoextra)
library(qicharts2)
library(qcc) 
library(devtools)
library(kernlab)
library(fBasics)
library(stringr)
library(reshape2)
library(ggplot2)
library(pracma)
library(tidyverse)
library(scatterplot3d) 
library(MASS)
library(e1071)



#################################################
#- Data to read -#
#################################################


# Read in our data with the 23 variables - static matrix
# Our dataset is a combination of cryptocurrencies and classical assets
# (commodities, exchange rates and stocks),
# covering the time period 01/02/2014 - 08/30/2019 (1426 trading days),
# for n = 679 assets.
#The variables used are parameters of the daily distribution of log-returns:
# Variance, Skewness, Kurtosis, Stable_alpha, Stable_gamma, Quantiles, 
# Conditional Tail Expectations,ARCH and GARCH parameters.
#   
data <- read.csv("23D.csv")

head(data)
stats=data[4:26]

############################################
#### Factor Model#####
#############################################

 #Factor Analysis

fa=factor_an_static(stats)
F=fa$F # final scores after the varimax rotation
eigval=fa$eigval #eigenvalues
eigvec=fa$eigvec #eigenvector
loadings=fa$loadings #loadings
f2=fa$f2 # standardized scoring coefficients

#Correlation heatmap
cormat <- round(cor(stats),2)
head(cormat)

melted_cormat <- melt(cormat)
head(melted_cormat)

# Heatmap

Var1=colnames(stats)
Var2=colnames(stats)

png("Correlation matrix.png")
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()+ggtitle("Correlation matrix")
dev.off()


#Scree plot

png("Scree plot.png")
# plot(
#   x = seq(1:10), y = eigval[1:10],
#   type = "o",
#   xlab = "Principle Component", ylab = "Eigenvalue", main="Scree plot")
y = eigval[1:6]
names(y)=c("1","2","3","4","5","6")

pareto.chart(y, xlab="Principle Component",
             ylab = "Eigenvalue",ylab2 = "Cumulative Variance",main="Pareto Chart")
dev.off()


#Loadings
df <- data.frame(loadings[,1], loadings[,2], loadings[,3])

names(df) <- c("1", "2", "3")
rownames(df) <-  c("Variance", "Skewness","Kurtosis",
                   "Stable_alpha","Stable_gamma",
                   "Q_{5%}","Q_{2.5%}","Q_{1%}","Q_{0.5%}",
                   "CTE_{5%}","CTE_{2.5%}","CTE_{1%}","CTE_{0.5%}",
                   "Q_{95%}","Q_{97.5%}","Q_{99%}","Q_{99.5%}",
                   "CTE_{95%}","CTE_{97.5%}","CTE_{99%}","CTE_{99.5%}"
                   ,"GARCH parameter","ARCH parameter")
df
df2=melt(t(df), id.vars = rownames(df))

names(df2)[1] <- "Factor"
names(df2)[2] <- "Variable"


# Reverse the order for ggplot
df2$Variable <- factor(df2$Variable, levels = rev(levels(df2$Variable)))

png("Loadings.png")
ggplot(df2,aes(x =Factor ,y=Variable,fill=value))+
         geom_tile()+
         scale_fill_gradient2(low = "blue", high ="red", mid="white",
                              midpoint = 0, limit = c(-1,1)) +
         theme_minimal()+ 
         theme(axis.text.x = element_text(angle = 0, vjust = 1, 
                                          size =8, hjust = 1))+
         ggtitle("Loadings of the three factors")
dev.off()
##########################################################
# Correlations between variables and the first three pc"s
#########################################################

# plot
#par(mfrow = c(2, 2))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))

png("Corr1.png")
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "Tail factor",
     ylab = "Memory factor", main="Correlations between variables and factors: 1 and 2"
     , cex.lab = 1, cex.axis = 1, cex.main = 1, lwd = 2)
abline(h = 0, v = 0)
label = c("Variance", "Skewness","Kurtosis",
          "Stable_alpha","Stable_gamma",
           "Q_{5%}","Q_{2.5%}","Q_{1%}","Q_{0.5%}",
          "CTE_{5%}","CTE_{2.5%}","CTE_{1%}","CTE_{0.5%}",
          "Q_{95%}","Q_{97.5%}","Q_{99%}","Q_{99.5%}",
          "CTE_{95%}","CTE_{97.5%}","CTE_{99%}","CTE_{99.5%}"
          ,"GARCH parameter","ARCH parameter")
points(loadings[,1],loadings[,2],col="red",pch = 16,type="p",cex = 1.5)
text(loadings[,1],loadings[,2],label,cex = 1)
dev.off()
# plot
#par(mfrow = c(2, 2))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))


png("Corr2.png")
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "Tail factor",
     ylab = "Memory factor", main="Correlations between variables and factors: 1 and 3"
     , cex.lab = 1, cex.axis = 1, cex.main = 1, lwd = 2)
abline(h = 0, v = 0)
label = c("Variance", "Skewness","Kurtosis",
          "Stable_alpha","Stable_gamma",
          "Q_{5%}","Q_{2.5%}","Q_{1%}","Q_{0.5%}",
          "CTE_{5%}","CTE_{2.5%}","CTE_{1%}","CTE_{0.5%}",
          "Q_{95%}","Q_{97.5%}","Q_{99%}","Q_{99.5%}",
          "CTE_{95%}","CTE_{97.5%}","CTE_{99%}","CTE_{99.5%}"
          ,"GARCH parameter","ARCH parameter")
points(loadings[,1],loadings[,3],col="red",pch = 16,type="p",cex = 1.5)
text(loadings[,1],loadings[,3],label,cex = 1)
dev.off()
# plot
#par(mfrow = c(2, 2))
ucircle = cbind(cos((0:360)/180 * pi), sin((0:360)/180 * pi))

png("Corr3.png")
plot(ucircle, type = "l", lty = "solid", col = "blue", xlab = "Moment factor",
     ylab = "Memory factor", main="Correlations between variables and factors: 2 and 3"
     , cex.lab = 1, cex.axis = 1, cex.main = 1, lwd = 2)
abline(h = 0, v = 0)
label = c("Variance", "Skewness","Kurtosis",
          "Stable_alpha","Stable_gamma",
          "Q_{5%}","Q_{2.5%}","Q_{1%}","Q_{0.5%}",
          "CTE_{5%}","CTE_{2.5%}","CTE_{1%}","CTE_{0.5%}",
          "Q_{95%}","Q_{97.5%}","Q_{99%}","Q_{99.5%}",
          "CTE_{95%}","CTE_{97.5%}","CTE_{99%}","CTE_{99.5%}"
          ,"GARCH parameter","ARCH parameter")
points(loadings[,2],loadings[,3],col="red",pch = 16,type="p",cex = 1.5)
text(loadings[,2],loadings[,3],label,cex = 1)
dev.off()
####################################################
#Intermediary calculations
##################################################

type_assets<-as.character(data$type_assets)
symb_assets<-as.character(data$symb_assets)
index_crypto=as.integer(str_detect(type_assets,"Crypto"))

Check <-c("BTC","ETH","XRP","BCH","LTC","USDT",	"BNB","EOS","XMR") 

index_show=match( Check,symb_assets)
colors=c("red","green","blue","black")[as.numeric(data$type_assets)]


DF=data.frame(F)
DF=cbind(DF, symb_assets,type_assets,index_crypto)
DF$label_show=as.numeric(apply(DF, 1, function(r) any(r %in% Check)))
plot( F[,1],  F[,2], 
      col=colors,
      xlab = "Tail Factor",
      ylab = "Moment factor", main="Tail factor vs. Moment factor",
      cex.lab = 1, cex.axis = 1, cex.main = 1,
      pch = 20, cex = 1.8)

points(F[index_show,1],  F[index_show,2],col="green",pch = 20,type="p",cex = 1.8)
text(F[index_show,1],  F[index_show,2], Check,cex = 1) 

unik <- !duplicated(type_assets)  ## logical vector of unique values
index_type_raw<-seq_along(type_assets)[unik]                      
n_assets<-length(type_assets)                    
index_type<-c(index_type_raw,n_assets)

n_types=length(index_type[1:(length(index_type)-1)])

####################################
# Factors and Kernel Density Contours
####################################
png("Factors1.png")
p1<-ggplot( DF, aes( X1, X2))+  geom_point(aes(color = type_assets),size = 2) + 
  xlim(-5,2)+
  ylim(-3,6)+
  scale_color_manual(values = c("red", "green", "blue","black"))+theme_classic()+
  geom_text(data = filter(DF, label_show==1),aes(label=symb_assets))+
labs(title = "Tail factor vs. Moment factor",x="Tail Factor", y="Moment factor")+ 
 geom_density_2d(aes(color = type_assets),contour=TRUE)
p1

dev.off()

png("Factors2.png")

p2<-ggplot( DF, aes( X1, X3))+  geom_point(aes(color = type_assets),size = 2) + 
  xlim(-4.5,2)+
  ylim(-2.5,2.5)+
  scale_color_manual(values = c("red", "green", "blue","black"))+theme_classic()+
  geom_text(data = filter(DF, label_show==1),aes(label=symb_assets))+
  labs(title = "Tail factor vs. Memory factor",x="Tail Factor", y="Memory factor")+ 
  geom_density_2d(aes(color = type_assets),contour=TRUE)
p2
dev.off()

png("Factors3.png")
p3<-ggplot( DF, aes( X2, X3))+  geom_point(aes(color = type_assets),size = 2) + 
  xlim(-3,6)+
  ylim(-2.5,3)+
  scale_color_manual(values = c("red", "green", "blue","black"))+theme_classic()+
  geom_text(data = filter(DF, label_show==1),aes(label=symb_assets))+
  labs(title = "Moment factor vs. Memory factor",x="Moment Factor", y="Memory factor")+ 
  geom_density_2d(aes(color = type_assets),contour=TRUE)
p3

dev.off()
###########################
#3D Scatter Plot
##########################

png("3D_Scatter.png")
scatterplot3d(DF[,1:3], pch = 16, color=colors,   main="3D Scatter Plot",
              xlab = "Tail Factor",
              ylab = "Moment Factor",
              zlab = "Memory Factor")
dev.off()

########################################
#Binary Logistic Regression
#######################################

# Encoding the target feature as factor 


DF$type_crypto=as.numeric(apply(DF, 1, function(r) any(r %in% "Crypto")))
DF$type_crypto = factor(DF$type_crypto, levels = c(0, 1)) 
colors_class=c("black","green")[as.numeric(DF$type_crypto)]

mod1 <- lrm(type_crypto ~ X1, data = DF)
print(mod1)

mod2 <- lrm(type_crypto ~ X2, data = DF)
print(mod2)

mod3 <- lrm(type_crypto ~ X3, data = DF)
print(mod3)
#####################################
#Linear Discriminant Analysis
####################################


mdl <- lda(type_crypto ~ X1+X2, data = DF)
# draw discrimination line
np <- 1000
nd.x <- seq(from = min(DF$X1), to = max(DF$X2), length.out = np)
nd.y <- seq(from = min(DF$X1), to = max(DF$X2), length.out = np)
nd <- expand.grid(X1 = nd.x, X2 = nd.y)

prd <- as.numeric(predict(mdl, newdata = nd)$class)

png("LDA.png")
plot( DF[,1:2], 
      col=colors_class,
      xlab = "Tail Factor",
      ylab = "Moment factor", main="Linear Discriminant Analysis",
      cex.lab = 1, cex.axis = 1, cex.main = 1,
      pch = 20, cex = 1.5)
points(F[index_show,1],  F[index_show,2],col="green",pch = 20,type="p",cex = 1.5)
text(F[index_show,1],  F[index_show,2], Check,cex = 1) 
#points(mdl$means, pch = "+", cex = 2, col = c("green", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE,col="red")
dev.off()
#####################################
#Quadratic Discriminant Analysis
####################################
mdl1 <- qda(type_crypto ~ X1+X2, data = DF)
# draw discrimination curve
np <- 1000
nd.x <- seq(from = min(DF$X1), to = max(DF$X2), length.out = np)
nd.y <- seq(from = min(DF$X1), to = max(DF$X2), length.out = np)
nd <- expand.grid(X1 = nd.x, X2 = nd.y)

prd1 <- as.numeric(predict(mdl1, newdata = nd)$class)

png("QDA.png")
plot( DF[,1:2], 
      col=colors_class,
      xlab = "Tail Factor",
      ylab = "Moment factor", main="Quadratic Discriminant Analysis",
      cex.lab = 1, cex.axis = 1, cex.main = 1,
      pch = 20, cex = 1.5)
points(F[index_show,1],  F[index_show,2],col="green",pch = 20,type="p",cex = 1.5)
text(F[index_show,1],  F[index_show,2], Check,cex = 1) 
#points(mdl$means, pch = "+", cex = 2, col = c("green", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd1, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE,col="red")

dev.off()


######################################################
#Support Vector Machines#
#####################################################



classifier = ksvm(type_crypto ~ X1+X2, data = DF,kernel="rbfdot",C=10) 
set =DF


grid_add=1.15;

X1=seq(from =min(set[,1])-grid_add, to = max(set[,1]+grid_add), by =0.05)
X2=seq(from =min(set[,2])-grid_add, to = max(set[,2]+grid_add), by =0.05)

grid_set = expand.grid(X1, X2) 
colnames(grid_set) = c('X1', 'X2') 
y_grid = predict(classifier, newdata = grid_set)

png("SVM.png")

plot(set[,1:2], col=colors_class ,
     xlim = range(X1), ylim = range(X2) ,pch = 20, cex = 1.5 ) 


contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), col="red",drawlabels = FALSE) 
points(set, pch = 20, col=colors_class,cex = 1.5)
text(set[index_show,1],  set[index_show,2], Check,cex = 1) 
#points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'white', 'white')) 
title(main = 'SVM', 
      xlab = 'Tail Factor', ylab = 'Moment Factor')
dev.off()

###################################
###K-means clustering
##################################
k_opt=kmeans_opt(DF[,1:3])
k_opt$K
k_opt$IDX
png("3D_Scatter_Clusters.png")

scatterplot3d(DF[,1:3], pch = 16, color=as.numeric(k_opt$IDX),   main="3D Scatter Plot",
              xlab = "Tail Factor",
              ylab = "Moment Factor",
              zlab = "Memory Factor")

dev.off()
clusters=cbind(k_opt$IDX,symb_assets,type_assets)
# 2-Way Frequency Table

mytable <- table(k_opt$IDX,type_assets) 
mytable

# # The optimal number of clusters, as determined by the Elbow method, is k = 10; however,
# # five clusters contain only cryptocurrencies, while the the other five clusters contain
# # stocks, commodities and exchange rates, plus the cryptocurrencies Bitcoin and Tether.
# # The quantlet Cluster_cryptos transformes these 10 clusters into 6 cluster, 5 for cryptos
# and the others in a single cluster.