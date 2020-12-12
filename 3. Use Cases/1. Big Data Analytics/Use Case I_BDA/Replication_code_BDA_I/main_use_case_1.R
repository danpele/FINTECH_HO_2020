# Clean the environment 
graphics.off()
rm(list = ls(all = TRUE))

# set working directory to folder "Material"
setwd("/home/rstudio")
#setwd("C:/Users/JP/Documents/Paolo/platform_experiments/filemanagement/files/1- Supregtech_material/SUPREGTECH BDA Use Cases/Use Case I/Replication code BDA I")

# Pre-load the packages and model_perf function

libraries = c("readr", "e1071", "MLmetrics", "stargazer", "dplyr", "purrr", "xtable", "base", "ggplot2", "DescTools","stylo", "igraph", "MASS", "ROCR", "rpart", "e1071", 
              "SDMTools", "caret","MLmetrics", "igraph", "clusterSim", "randomForest", "Hmisc", "networkD3", "emstreeR")
lapply(libraries, library, quietly = TRUE, character.only = TRUE)

source("model_perf.R")

#============================================================================#
#============================================================================#
#=========================== GENERATE THE DATASETS ==========================#
#============================================================================#
#============================================================================#


# Import the dataset "final_dataset_smes.csv"
dataset = read_csv("smaller_dataset.csv")
dataset = dataset[,c(3:22)] # keep financial ratios plus the response variable "status" which takes value 0 if the company has not defaulted and 1 if it has.
dataset = dataset[complete.cases(dataset),]

# We define a metric201that provides the relative distance between 
# companies by applying the standardized Euclidean distance between each pair (xi,xj) 
# of institutions feature vectors
dist = as.matrix(dist(scale(dataset[-20])))
g = graph_from_adjacency_matrix(dist, mode = "undirected", weighted = TRUE) # we define the graph 

# We find the MST representation of the graph g
g_mst = mst(g)

# Plot the g_mst
V(g_mst)$status = dataset$status
V(g_mst)[status == 1]$color = "firebrick1" # color defaulted companies red
V(g_mst)[status == 0]$color = "lightgreen" # color active companies green
plot(g_mst, graph = "nsca",
     vertex.label=NA, 
     vertex.size = 3, 
     main = "MST representation of the borrowing companies networks")

# Generate the dataset with network and community information
dataset_net = dataset
dataset_net$degree = igraph::degree(g_mst) #degree centrality 
dataset_net$strenght = igraph::strength(g_mst) #strenght centrality 
community = cluster_louvain(g_mst) #community detection
com = membership(community) 
dataset_net$com = com 



#============================================================================#
#============================================================================#
#=========================== RUNNING OF MODELS  ==========================#
#============================================================================#
#============================================================================#


# Dataset without network parameters 
data = dataset
data$name = seq(1:4514)

# Check for NAs
apply(data, 2, function(x) any(is.na(x)))

# Decide the variables to be included in the model (step-wise)
glm_var_sel = glm(status~., data=data[-21],family=binomial(link= logit))
step = stepAIC(glm_var_sel, direction="both")
step$anova
formula = as.formula(step[["formula"]])

# Sub-sampling 
data$status = as.factor(data$status) # set the response as a factor
set.seed(1) 
splits = createFolds(data$status, returnTrain = TRUE) # define 10 training and 10 testing datasets 
in_train = list()
train = list()
test = list()

for (i in seq(along = splits)){
  in_train[[i]] = unique(splits[[i]])
  train[[i]] = data[(data$name %in% splits[[i]]),]
  train[[i]] = train[[i]][-21] # we remove the name column 
  test[[i]] = data[!(data$name %in% splits[[i]]),]
  test[[i]] = test[[i]][-21] # we remove the name column 
}


# Dataset with network parameters 
data_net = dataset_net
data_net$name = seq(1:4514)

# Check for NAs
apply(data_net, 2, function(x) any(is.na(x)))

# Define the formula - Final Step-wise model plus the centrality parameters 
y = "status"
x = c("ratio002", "ratio003", "ratio004", "ratio005", "ratio011",
      "ratio012", "ratio017", "ratio018", "ratio019", "ratio030", "DPO", 
      "DSO", "turnover","degree","strenght","com")
formula_net = as.formula(
  paste(y, 
        paste(x, collapse = " + "), 
        sep = " ~ "))

# Sub-sampling 
data_net$status = as.factor(data_net$status)
set.seed(1)
splits = createFolds(data_net$status, returnTrain = TRUE) # Define 10 training and 10 testing datasets 
in_train_net = list()
train_net = list()
test_net = list()

for (i in seq(along = splits)){
  in_train_net[[i]] = unique(splits[[i]])
  train_net[[i]] = data_net[(data_net$name %in% splits[[i]]),]
  train_net[[i]] = train_net[[i]][-24]
  test_net[[i]] = data_net[!(data_net$name %in% splits[[i]]),]
  test_net[[i]] = test_net[[i]][-24]
}


# LOGISTIC REGRESSION WITH AND WITHOUT NETWORK PARAMETERS 
glm_res = models_perf(formula, train, test, model = "glm") 
glm_res_net = models_perf(formula_net, train_net, test_net, model = "glm")


# LINEAR DISCRIMINANT ANALYSIS WITH AND WITHOUT NETWORK PARAMETERS
lda_res = models_perf(formula = formula, train, test, model = "lda")
lda_res_net = models_perf(formula = formula_net, train_net, test_net, model = "lda")

# CART WITH AND WITHOUT NETWORK PARAMETERS 
cart_res = models_perf(formula = formula, train, test, model = "rpart")
cart_res_net = models_perf(formula = formula_net, train_net, test_net, model = "rpart")

# SVM WITH AND WITHOUT NETWORK PARAMETERS 
svm_res = models_perf(formula = formula, train, test, model = "svm")
svm_res_net = models_perf(formula = formula_net, train_net, test_net, model = "svm")

# Make a summary table
summary = rbind(glm_res, glm_res_net, lda_res, lda_res_net, cart_res, cart_res_net, svm_res, svm_res_net)
summary1 = list()

for (i in 1:length(summary)){
  summary1[[i]] = strsplit(summary[[i]], " ")[[1]][c(3,5,8,11)]
}
table = do.call("rbind", summary1)
colnames(table) = c("AUC", "GINI", "KS", "ACC")
rownames(table) = c("glm_res", "glm_res_net", "lda_res", "lda_res_net", "cart_res", "cart_res_net", "svm_res", "svm_res_net")
table




