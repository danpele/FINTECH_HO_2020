rm(list=ls())

print("start selfinstaller")

options(rgl.useNULL = TRUE)
options(repos = c(CRAN = "http://mran.revolutionanalytics.com/snapshot/2020-03-20"))

used_packages <- c(#"readr","PerformanceAnalytics","readxl","DescTools","NMOF",
                   "Rsolnp"
                   )

for(package in used_packages) {
  if(!package %in% installed.packages()){
    install.packages(package)
  } 
}


# library(readr)
# library(PerformanceAnalytics)
# library(readxl)
# library(DescTools)
# library(NMOF)
library(Rsolnp)





print(sessionInfo())

print("selfinstaller finished!")




