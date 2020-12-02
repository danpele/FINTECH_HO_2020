####   D I S C L A I M E R   #####

# This code supplements the paper https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3506274.
# It is only allowed to use it for evaluation puposes in the EU Horizon2020 project
# FIN-TECH (www.fintech-ho2020.eu) under grant agreement No 825215.
# 
# It is strictly prohibited to use the code outside the project or even distribute it.



# Clean the environment
graphics.off()
rm(list = ls(all = TRUE))

library("ggpubr")
library("caTools")
library("grDevices")
library("e1071")
library("tidyverse")
library("readxl")
library("caret")
library("xgboost")
library("ROCR")
library("mlr")
library("mlrMBO")
#library("devtools")
library("rgenoud")
library("emstreeR")
library("igraph")
library("graphlayouts")
library("grid")
library("autoxgboost") #devtools::install_github("ja-thomas/autoxgboost")
library("SHAPforxgboost")
library("e1071")

# setwd("/home/rstudio")
# setwd("/home/ruser/XAI_App")

# Import dataset and match header as matrix
data <- read_excel("final_dataset_smes.xlsx", sheet=1)
data <- data[,-1]

# Import the headers of the dataset
data_names <- read_excel("final_dataset_smes.xlsx", sheet=2)
as.matrix(data_names)

# Match the header names to the columns
# The readable Columnnamens have been added by hand to ensure easier readibility in the fututre and in case of display in ShinyApp or somewhere else
readableColNames = c("total assets divided by shareholder funds minus 1", "long term debt plus loans then divided by shareholder funds", 
                     "total Assets to total Liabilities", "Current Ratio", "current assets without stocks divided by current liabilities",
                     "Shareholders Funds plus Non current liabilities then divided by Fixed assets", "EBIT to interest paid", "profit before taxes plus interest paid then divided by total assets",
                     "profit or loss after tax/divided by shareholder funds", "operating revenue divided by total assets",
                     "sales divided by total assets", "interest paid divided by profit before tax plus interest paid",
                     "EBITDA to Interest Coverage Ratio", "EBITDA to operating revenue ratio", "EBITDA to sales",
                     "constraint EBIT", "constraint profit and losses before tax", "constraint financial profit and losses", 
                     "constraint profit and losses for period in th EUR", "trade payables divided by operating revenues",
                     "trade receivables divided by operating revenues", "inventories divided by operating revenues", "total revenue",
                     "Industry classification on NACE code, 4 digits precision")
data_names[,3] = readableColNames

colnames(data) <- c("status",as.matrix(data_names)[,3])
as_tibble(data)
sapply(data.frame(data), class)

# extract the variables which will not be used
out <- c(17,18,19,20,25)
data <- data[,-out]


status <- as.matrix(data$status)
table(status)

# convert data to dataframe
data_df <- data.frame(data)

# convert status to a factor
data_df$status <- as.factor(data_df$status)

set.seed(4711)
randomized_data_df <- data_df[sample(nrow(data_df)), ]

## Split dataset into train and test (80% train and 20% test)
set.seed(4711)
trainIndex = createDataPartition(data_df$status, p = 0.8, list=FALSE, times=1)
#plot(trainIndex)

train <-  randomized_data_df[trainIndex,]
test <-  randomized_data_df[-trainIndex,]

# Percentage of defaults in train and test dataset
## train
train$Default <- as.factor(train$status)

percentage <- function(x) {
  y <- as.numeric(levels(x))[x]
  prc <- sum(y)/length(y)*100
  prc
}

default_train <- percentage(train$Default)
default_train

## test
test$Default    <- as.factor(test$status)

default_test <- percentage(test$Default)
default_test

# Delete the default column
train <- train[, (colnames(train) != "Default")]
test <- test[, (colnames(test) != "Default")]



###########################################################################
################### Linear model implementation  ##########################
###########################################################################

# Logistic Regression model fitting
set.seed(4711)
lr_model <- glm(status ~., family = binomial(link = 'logit'), data = train)

# Analysis of deviance
# anova(lr_model)

# Measuring the predictive ability of the model
# If prob > 0.5 then 1, else 0. Threshold can be set for better results
fitted.results <- predict(lr_model, newdata = subset(test, type='response'))
fitted.results <- ifelse(fitted.results > 0.5, 1, 0)

misClasificError <- mean(fitted.results != test$status)
print(paste('Accuracy',1-misClasificError))

# Confusion matrix
cm_log <- caret::confusionMatrix(table(fitted.results, test$status))


# ROC curve and Area under the ROC curve (AUC)
# calculate the predictions
pred_log <- predict(lr_model, newdata = subset(test, type = "response"))
pr_log <- prediction(pred_log, test$status)

# ROC curve
prf_log <- ROCR::performance(pr_log, measure = "tpr", x.measure = "fpr")
# plot(prf_log, main = "Logistic Regression model")

# AUROC
auc_log <- ROCR::performance(pr_log, measure = "auc")
auc_log <- auc_log@y.values[[1]]




###########################################################################
################### XGBoost model implementation  #########################
###########################################################################
# set status as numaric and as factor
# train$status <- as.numeric(as.character(train$status)) 
# train$status <- as.factor(train$status)

set.seed(1234)
# Create a classification task
clas_task <- makeClassifTask(data = train, target = "status")

# Create a control object for MBO optimization
ctrl = makeMBOControl()
#Speed up Tuning by only doing 1 iteration
ctrl = setMBOControlTermination(ctrl, iters = 10)
 
# Fit and optimize a xgboost model by using autoxgboost package
res = autoxgboost(clas_task, control = ctrl, tune.threshold = FALSE)

#saveRDS(res,file="autoxgboost_res_t_budget_1200.rds")
#res <- readRDS(file="autoxgboost_res_t_budget_1200.rds")

# Get the current hyperparameter settings of a learner and make a list out of it
Param_chosen <- mlr::getHyperPars(res$final.learner)
#print(unlist(Param_chosen))
param_dart <- list(objective = Param_chosen$objective,
                   # booster = "dart",
                   nrounds = Param_chosen$nrounds,
                   eta = Param_chosen$eta,
                   max_depth = Param_chosen$max_depth,
                   gamma = Param_chosen$gamma,
                   subsample = Param_chosen$subsample,
                   colsample_bytree = Param_chosen$colsample_bytree)


# Rename trainset for the xgboost model
train_xgb <- train
y_var <-  "status"
train_xgb_y_excl <- train_xgb[,setdiff(colnames(train), y_var)]

train_xgb_y_only <- train_xgb[[y_var]]
# XGBoost model fitting
xgb_mod <- NA
set.seed(1234)
xgb_mod <- xgboost::xgboost(data = as.matrix(train_xgb_y_excl),
                            label = as.matrix(train_xgb[[y_var]]),
                            xgb_param = param_dart, nrounds = param_dart$nrounds,
                            verbose = TRUE, nthread = parallel::detectCores() - 2,
                            early_stopping_rounds = 8)


## Perform the predictions on the train set (for the MST)
pred_mod_train <- predict(xgb_mod, as.matrix(train_xgb_y_excl))

# change to predicted values (0 or 1)
pred_xgb_train <- rep(0,length(pred_mod_train))
pred_xgb_train[pred_mod_train>0.5] <- 1


# Rename testset for the xgboost model
test_xgb <- test
truelabel <- test_xgb$status
y_var <-  "status"
test_xgb_y_excl <- test_xgb[,setdiff(colnames(test), y_var)]

## Perform the predictions out of sample
pred_mod <- predict(xgb_mod, as.matrix(test_xgb_y_excl))


# change to predicted values (0 or 1)
pred_xgb_test <- rep(0,length(pred_mod))
pred_xgb_test[pred_mod>0.5] <- 1



## Confusion matrix
cm_xgb <- caret::confusionMatrix(as.factor(pred_xgb_test), as.factor(truelabel))


# ROC and AUC
pr_xgb <- prediction(pred_mod, truelabel) # TODO: Check
prf_xgb <- ROCR::performance(pr_xgb, measure = "tpr", x.measure = "fpr")
# plot(prf_xgb, main = "xgboost model")

auc_xgb <- ROCR::performance(pr_xgb, measure = "auc")
auc_xgb <- auc_xgb@y.values[[1]]



###########################################################################
############### Horse Race between lin. model and GBM  ####################
###########################################################################

# Plot the ROC curves
# variable pre-processing

# Convert to num
pred_log_num <- unname(pred_log)

# Define y
y = test$status

# Combine predictions of Logistic Regression model and XGBoost model
preds <- cbind(p1 = pred_mod, 
               p2 = pred_log)


## Plot the ROC curve and calculate AUROC
# TPR (True positive rate) = sensitivity, FPR False positive rate = 1-specificity
colors <- c('red', 'blue') 
# png(file="ROC.png",
#     width=1200, height=700)
for (i in 1:2) {
  ROCR::plot(ROCR::performance(prediction(preds[,i],y),"tpr","fpr"), 
       add=(i!=1),col=colors[i],lwd=2)
}
legend("bottomright", legend=c("Gradient Boosting model (xgboost)", "Logistic Regression model"),
       col=c("red", "blue"), lty=1, cex=0.8)

# dev.off()
# Calculate AUC
# XGBoost AUC
auc_xgb

# Logistic Regression AUC
auc_log

# Confusion matrices (CM)
# CM of the xgboost model
cm_xgb

# CM of the logit model
cm_log



###########################################################################
########### Minimum Spanning Tree based on Shapley Values  ################
###########################################################################

# Generate the SHAP Values based on the train set
# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = xgb_mod, X_train = train_xgb_y_excl)


# Generate the Sahp Values based on the test set
# To return the SHAP values and ranked features by mean|SHAP|
shap_values_test <- shap.values(xgb_model = xgb_mod, X_train = test_xgb_y_excl)


# Compute the Minimum spanning tree based on the shap values
g_mst_shap <- emstreeR::ComputeMST(shap_values$shap_score)

# create a matrix of the edges of the MST
edgematrix <- cbind(g_mst_shap$from,g_mst_shap$to)
# for some strange reason the link 1--1 is always included, is deleted here.
edgematrix <- edgematrix[-dim(edgematrix)[1],]  

# Create the graph based on this edge list matrix
g_mst0_shap <- graph_from_edgelist(edgematrix,directed = FALSE)

# start working with the MST based on the Shap values of the train dataset
# pre-processing on the graph object
pa <- g_mst0_shap

V(pa)$name <- c(1:length(V(pa)))
V(pa)$size <- rep(2,length(V(pa)))


# Plot MST with colored nodes, based on predicted defaults
# Color the nodes, which represent companies that are predicted defaults in red and the other nodes in grey
default_marker <- pred_xgb_train
defaultvector <- as.numeric(default_marker)
col <- rep("grey",length(defaultvector))
col[which(defaultvector==1)] <- "red"
V(pa)$color <- col

# Use package graphlayouts to stress the majorization for this larger graph based on a set of pivot nodes
set.seed(1234)
lay <- graphlayouts::layout_with_sparse_stress(pa, pivots = 60, iter = 300)
V(pa)$label <- NA


# Plot the MST based on the Shap values calculated from the train dataset 
# (defaults = red; non-defaults = grey)
plot(pa, layout=lay)

# Add a legend to the plot
legend("topleft", inset = .02, legend = c("Non-Defaults", "Defaults"), fill = c("grey", "red"), cex = 0.8)


#################################


# Plot MST with colored nodes, based on detected communities
# Pre-processing on the graph object
set.seed(1234)
tempnet = g_mst0_shap
V(tempnet)$name <- c(1:length(V(tempnet)))

# Finding communities and highlight them in the MST
imc = cluster_fast_greedy(tempnet)
cluster <- membership(imc)

# Define colors for the communities
colrainbow=rainbow(max(cluster))
V(tempnet)$color <- colrainbow[cluster]
V(tempnet)$size <- rep(2,length(V(tempnet)))
V(tempnet)$label <- NA

# MST with colored clusters
# Use package graphlayouts to stress the majorization for this larger graph based on a set of pivot nodes
templay <- graphlayouts::layout_with_sparse_stress(tempnet, pivots = 60, iter = 300)

# Plot the MST based on the Shap values calculated from the train dataset 
# (detected communities are colored)
set.seed(1234)
plot(tempnet, layout=templay)



###########################################################################
########### Shap values for four representative companies  ################
###########################################################################

# Plot shap values for 4 companies from the dataset (2 defaulted, 2 non-defaulted)
# Find in train$status 2 companies with status = 1 and 2 companies with status = 0 
# train[c(1:20),1]
# Chosen representative companies are in row 3,4,5 and 6

# Non-defaulted
# Company 1
# Extract the Shap values and set as dataframe
shap_comp_1 <- shap_values$shap_score[3,]
DF_shap_comp_1 <- data.frame(w = names(shap_comp_1), v = as.numeric(shap_comp_1))

# Plot the Shap values sorted by feature
shap_comp_1_plot <- ggplot(DF_shap_comp_1, aes(x = reorder(w), y = v,fill = v))+
  geom_bar(stat ="identity", position = "dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Company 1 - non-defaulted")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
shap_comp_1_plot


# Company 2
# Extract the Shap values and set as dataframe
shap_comp_2 <- shap_values$shap_score[4,]
DF_shap_comp_2 <- data.frame(w=names(shap_comp_2),v=as.numeric(shap_comp_2))

# Plot the Shap values sorted by feature
shap_comp_2_plot <- ggplot(DF_shap_comp_2, aes(x=reorder(w), y=v,fill=v))+    
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Company 2 - non-defaulted")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
shap_comp_2_plot


# Defaulted
# Company 3
# Extract the Shap values and set as dataframe
shap_comp_3 <- shap_values$shap_score[5,]
DF_shap_comp_3 <- data.frame(w=names(shap_comp_3),v=as.numeric(shap_comp_3))

# Plot the Shap values sorted by feature
shap_comp_3_plot <- ggplot(DF_shap_comp_3, aes(x=reorder(w), y=v,fill=v))+  
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Company 3 - defaulted")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
shap_comp_3_plot


# Company 4
# Extract the Shap values and set as dataframe
shap_comp_4 <- shap_values$shap_score[6,]
DF_shap_comp_4 <- data.frame(w=names(shap_comp_4),v=as.numeric(shap_comp_4))

# Plot the Shap values sorted by feature
shap_comp_4_plot <- ggplot(DF_shap_comp_4, aes(x=reorder(w), y=v,fill=v))+    
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+
  xlab("")+
  ggtitle("Company 4 - defaulted")+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
shap_comp_4_plot



multiplot <- function(..., plotlist=NULL, file, cols=2, layout=NULL) {
  # Multiple plot function
  #
  # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
  # - cols:   Number of columns in layout
  # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
  # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
  # then plot 1 will go in the upper left, 2 will go in the upper right, and
  # 3 will go all the way across the bottom.
  
    # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# Plot all 4 shap plots in one figure
multiplot(shap_comp_1_plot, shap_comp_2_plot, shap_comp_3_plot, shap_comp_4_plot, cols=2)
#legend("topleft", legend = c("High importance", "Low importance"), fill = c("blue", "red"), cex = 0.8)


overall_shap <- data.frame(shap_values$mean_shap_score)
overall_shap$ui <- rownames(overall_shap)

g<- ggplot(overall_shap[1:10,], 
       aes(x=reorder(ui, shap_values.mean_shap_score), y=shap_values.mean_shap_score, fill=shap_values.mean_shap_score))+ 
        geom_bar(stat ="identity",position="dodge") + coord_flip()+
  ylab("Global variable importance (top 10)")+xlab("")+#ggtitle()+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
# ggsave(filename = "globSHAP.png", plot=g, width = 8,height = 4)


g<- ggplot(overall_shap, 
           aes(x=reorder(ui, shap_values.mean_shap_score), y=shap_values.mean_shap_score, fill=shap_values.mean_shap_score))+ 
  geom_bar(stat ="identity",position="dodge") + coord_flip()+
  ylab("Global variable importance")+xlab("")+#ggtitle()+
  guides(fill=F)+
  scale_fill_gradient(low="red", high="blue")
# ggsave(filename = "globSHAP_total.png", plot=g, width = 8,height = 6)
