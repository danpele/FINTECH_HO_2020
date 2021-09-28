# Clean the environment 
graphics.off()
#options(warn=-1)
rm(list = ls(all = TRUE))

# Necessary libraries 
libraries = c("readr", "e1071", "MLmetrics", "stargazer", "dplyr", "purrr", "xtable", "base", "ggplot2", "DescTools","stylo", "igraph", "MASS", "ROCR", "rpart", "e1071", 
               "SDMTools", "caret","MLmetrics", "igraph", "clusterSim", "randomForest", "Hmisc", "networkD3", "emstreeR")

lapply(libraries, function(x) if (!(x %in% installed.packages())) {
  install.packages(x)
})

lapply(libraries, library, quietly = TRUE, character.only = TRUE)

