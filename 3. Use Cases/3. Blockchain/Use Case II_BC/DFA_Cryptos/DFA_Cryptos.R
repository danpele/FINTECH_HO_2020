# # Name of Quantlet: DFA_cryptos
# # 
# 
# Description: 'Dynamic projection of a dataset of 23 variables, describing cryptos, 
# stocks, FX and commodities on a 3D space defined by the three factors
# extracted using Factor Analysis.'
# 
# Keywords: cryptocurrency, genus proximum, classiffication, multivariate analysis, factor models
# 
# Author : Daniel Traian Pele
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
options(warn=-1)
#setwd("D:\\PROIECTE\\HORIZON 2020\\Use Case DP\\rstudio-export\\DFA_Cryptos")
setwd("/home/rstudio/DFA_Cryptos/")

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

options(warn=-1)
data_dynamic <- read.csv('dynamic_dataset.csv') #dynamic dataset
data <- read.csv("23D.csv") #static dataset
stats=data[4:26]
fa=factor_an_static(stats[,1:23])

eigval=fa$eigval #eigenvalues
eigvec=fa$eigvec #eigenvector
loadings=fa$loadings #loadings
f2=fa$f2 # standardized scoring coefficients

n_waves=740 # number of waves

for (i in c(50,300,500,740))
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
  DF$label_show=as.numeric(apply(DF, 1, function(r) any(r %in% Check)))
  # plot( F[,1],  F[,2], 
  #       col=colors,
  #       xlab = "Tail Factor",
  #       ylab = "Moment factor", main=paste("Data: 01-Jan-2014 - ",
  #                                          format(as.Date(stats_t$Date[1]), "%d-%b-%Y")),
  #       cex.lab = 1, cex.axis = 1, cex.main = 1,
  #       pch = 20, cex = 1.8)
  # 
  # points(F[index_show,1],  F[index_show,2],col="green",pch = 20,type="p",cex = 1.8)
  # text(F[index_show,1],  F[index_show,2], Check,cex = 1) 
  progress=percent(i/n_waves)
  ttl=paste("Data: 1/1/2014 - ",stats_t$Date[1]," (",progress,")",sep="")
  
  
  p1<-ggplot( DF, aes( X1, X2))+  geom_point(aes(color = type_assets),size = 2) + 
    xlim(-1,8)+
    ylim(-3,14)+
    scale_color_manual(values = c("red", "green", "blue","black"))+theme_classic()+
    geom_text(data = filter(DF, label_show==1),aes(label=symb_assets))+
    labs(title = ttl,x="Tail Factor", y="Moment factor")+ 
    geom_density_2d(aes(color = type_assets),contour=TRUE)
  print(p1)
  
  
  
}

# See the created plots.

