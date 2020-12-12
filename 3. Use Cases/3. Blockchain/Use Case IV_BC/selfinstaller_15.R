rm(list=ls())

print("start selfinstaller")

options(rgl.useNULL = TRUE)
options(repos = c(CRAN = "http://mran.revolutionanalytics.com/snapshot/2020-03-20"))

used_packages <- c("quantmod","quantreg","doParallel","foreach"
                   )

for(package in used_packages) {
  if(!package %in% installed.packages()){
    install.packages(package)
  } 
}


library("quantmod")
library("quantreg")
library("doParallel")
library("foreach")




print(sessionInfo())

print("selfinstaller finished!")




