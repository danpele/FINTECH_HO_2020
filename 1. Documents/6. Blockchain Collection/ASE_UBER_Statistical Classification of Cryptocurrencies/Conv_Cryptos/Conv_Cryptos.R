# # Name of Quantlet: Conv_cryptos
# # 
# # 
# 
# Description: 'This quantlet shows the convergence of cryptocurrencies over time, by computing Likelihood Ratio from binary logistic regression.'
# 
# Keywords: cryptocurrency, genus proximum, classiffication, clustering, classification, multivariate analysis, convergence
# 
# Author: Daniel Traian Pele
# 
# See also : SFA_cryptos
# 
# 
# 
# Datafiles : 'dynamic_dataset.csv'

#################################################################
#- Clear Environment -#
rm(list = ls())
graphics.off()

#setwd("D:\\PROIECTE\\HORIZON 2020\\Use Case DP\\Conv_Cryptos")
setwd("/home/rstudio/Conv_Cryptos/")


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
# install.packages("gridExtra")

#install.packages("anytime")
#- Source R functions to be used -#

source('factor_an_static.R')


#- Libraries -#
library(anytime)
library(gridExtra)
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

library(MASS)
library(e1071)
library(scales)

#Libraries

#################################################
#- Data to read -#
#################################################

# 
# In order to derive the dynamics of the assets' universe, we used an 
# expanding window approach, described below:
# -The 23-dimensional dataset is estimated for the time interval [1; t0] =
# [01=02=2014; 10=31=2016].
# -Time window is extended on a daily basis, up to T=08/30/2019 and for each step
# in time, the dataset is projected on the 2-dimensional space defined by the tail
# factor and the moment factor, estimated for the entire time period.
# #   

data_dynamic <- read.csv('dynamic_dataset.csv') #dynamic dataset
data <- read.csv("23D.csv") #static dataset
stats=data[4:26]
fa=factor_an_static(stats[,1:23])

eigval=fa$eigval #eigenvalues
eigvec=fa$eigvec #eigenvector
loadings=fa$loadings #loadings
f2=fa$f2 # standardized scoring coefficients

n_waves=740 # number of waves

LR <- data.frame(matrix(nrow = n_waves, ncol = 7))
Date<-unique(data_dynamic$Date)

Date<-anydate(Date)


for (i in (1:n_waves))
{
  stats_t=data_dynamic[ which(data_dynamic$wave==i) ,]
  stats_t[is.na(stats_t)] <- 0
  
  n1 = nrow(stats_t[,1:23]);
  m  = colMeans(stats_t[,1:23], na.rm = FALSE, dims = 1)
  # standardizing data
  
  
  Rho=cor(stats_t[,1:23])
  Rho[is.na(Rho)] <- 0
  
  std=repmat(sqrt((colVars(stats_t[,1:23]))),n1,1)
  
  zz = as.matrix((stats_t[,1:23]-repmat(m,n1,1))/std)
  zz[is.na(zz)] <- 0
  F=zz%*%f2
  
  type_assets<-as.character(stats_t$type_assets)
  symb_assets<-as.character(stats_t$symb_assets)
  index_crypto=as.integer(str_detect(type_assets,"Crypto"))
  
  Check <-c("BTC","ETH","XRP","BCH","LTC","USDT",	"BNB","EOS","XMR") 
  
  index_show=match( Check,symb_assets)
  colors=c("red","green","blue","black")[as.numeric(stats_t$type_assets)]
  DF=data.frame(F)
  DF=cbind(DF, symb_assets,type_assets,index_crypto)
  DF$type_crypto=as.numeric(apply(DF, 1, function(r) any(r %in% "Crypto")))
  DF$type_crypto = factor(DF$type_crypto, levels = c(0, 1)) 
  colors_class=c("black","green")[as.numeric(DF$type_crypto)]
  
  mod1 <- lrm(type_crypto ~ X1, data = DF)
  

  mod2 <-lrm(type_crypto ~ X2, data = DF)

  
  mod3 <- lrm(type_crypto ~ X3, data = DF)

  LR[i,1]<-i
  LR[i,2]<-mod1$deviance[2]
  LR[i,3]<-mod2$deviance[2]
  LR[i,4]<-mod3$deviance[2]
  LR[i,5]<-mod1$stats[5]
  LR[i,6]<-mod2$stats[5]
  LR[i,7]<-mod3$stats[5]
 
}

LR=cbind(LR,Date)
png("LR.png")
p1<-ggplot(data = LR, aes(x = Date, y = X2))+
  geom_line(color = "blue",size=1)+ scale_x_date(date_labels = "%d %b %y")+
  ylim(0,800)+
  labs(x = "Date", y = "Likelihood Ratio", 
       title = "Tail Factor")
p2<-ggplot(data = LR, aes(x = Date, y = X3))+ scale_x_date(date_labels = "%d %b %y")+
  geom_line(color = "blue",size=1)+
  labs(x = "Date", y = "Likelihood Ratio", 
       title = "Moment Factor")

p3<-ggplot(data = LR, aes(x = Date, y = X4))+ scale_x_date(date_labels = "%d %b %y")+
  geom_line(color = "blue",size=1)+
  labs(x = "Date", y = "Likelihood Ratio", 
       title = "Memory Factor")

grid.arrange(p1, p2, p3, ncol=1, nrow = 3)

dev.off()

png("p_values.png")
p1<-ggplot(data = LR, aes(x = Date, y = X5))+
  geom_line(color = "blue",size=1)+ scale_x_date(date_labels = "%d %b %y")+
  ylim(0,0.04)+
  labs(x = "Date", y = "P-value", 
       title = "Tail Factor")
p2<-ggplot(data = LR, aes(x = Date, y = X6))+ scale_x_date(date_labels = "%d %b %y")+
  geom_line(color = "blue",size=1)+
  ylim(0,0.04)+
  labs(x = "Date", y = "P-value", 
       title = "Moment Factor")

p3<-ggplot(data = LR, aes(x = Date, y = X7))+ scale_x_date(date_labels = "%d %b %y")+
  geom_line(color = "blue",size=1)+
  ylim(0,0.04)+
  labs(x = "Date", y = "P-value", 
       title = "Memory Factor")

grid.arrange(p1, p2, p3, ncol=1, nrow = 3)

dev.off()