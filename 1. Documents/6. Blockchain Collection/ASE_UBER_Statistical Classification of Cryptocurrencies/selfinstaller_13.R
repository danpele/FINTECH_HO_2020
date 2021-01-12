rm(list=ls())

print("start selfinstaller")

options(rgl.useNULL = TRUE)
options(repos = c(CRAN = "http://mran.revolutionanalytics.com/snapshot/2020-03-20"))

used_packages <- c("devtools","gridExtra","scatterplot3d","e1071","anytime","rms","caret","factoextra",
                   "qicharts2","qcc","kernlab","fBasics","stringr","reshape2","ggplot2","pracma","tidyverse","MASS","scales","matlab"
                   )

for(package in used_packages) {
  if(!package %in% installed.packages()){
    install.packages(package)
  } 
}


library(devtools)
library(gridExtra) 
library(scatterplot3d) 
library(e1071)
library(anytime)
library(rms)
library(caret)
library(factoextra)
library(qicharts2)
library(qcc) 
library(kernlab)
library(fBasics)
library(stringr)
library(reshape2)
library(ggplot2)
library(pracma)
library(tidyverse)
library(MASS)
library(scales)
library(matlab)



print(sessionInfo())

print("selfinstaller finished!")




