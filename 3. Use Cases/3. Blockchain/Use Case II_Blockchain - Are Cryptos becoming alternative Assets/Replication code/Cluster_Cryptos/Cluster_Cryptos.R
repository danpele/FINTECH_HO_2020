# Name of Quantlet: Cluster_cryptos
# 
# Description: 'Clusterization of different cryptocurrencies as form
#                      of unsupervised machine learning in finance.'
# 
# Keywords: cryptocurrency, genus proximum, classification, clustering, unsupervised learning
# 
# Author: Daniel Traian Pele

#- Clear Environment -#
rm(list = ls())
graphics.off()

#setwd("D:\\PROIECTE\\HORIZON 2020\\Use Case DP\\rstudio-export\\Cluster_Cryptos")
setwd("/home/rstudio/Cluster_Cryptos/")

#Packages
# install.packages("e1071")
# install.packages("gridExtra")
# install.packages("scatterplot3d")

#- Libraries -#
library(gridExtra) 
library(scatterplot3d) 

library(e1071)

library(tidyverse)

#################################################
#- Data to read -#
#################################################


# read in our data with the clusters
clusters <- read.csv("clusters_assets.csv")

type_assets<-as.character(clusters$type_assets)
symb_assets<-as.character(clusters$symbol)
index_crypto=as.integer(str_detect(type_assets,"Crypto"))

Check <-c("BTC","USDT") 



index_show=match( Check,symb_assets)
png("Clusters cryptos.png")
s3d<-scatterplot3d(clusters[,4:6], pch = 16, color=as.numeric(clusters$cluster),  
                   main="3D Scatter Plot",
              xlab = "Tail Factor",
              ylab = "Moment Factor",
              zlab = "Memory Factor")

text(s3d$xyz.convert(clusters[index_show,4:6]), Check) 
dev.off()


svm1 <- svm(as.factor(cluster)~ F1+F2+F3, data=clusters,
            method="C-classification", kernal="radial", 
            gamma=0.1, cost=10)
prediction <- predict(svm1, clusters[,4:6])

xtab <- table(clusters$cluster, prediction)
xtab
grid.table(xtab)
#The validity of this classification is proven by Support Vector Machines (SVM)
Accuracy=sum(diag(xtab))/length(clusters$cluster)
print(Accuracy)

