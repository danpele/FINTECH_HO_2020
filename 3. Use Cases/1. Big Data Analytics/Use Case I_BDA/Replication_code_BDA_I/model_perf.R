models_perf = function(formula, train, test, model){
  fit = list()
  fit_score = list()
  fit_score1 = list()
  fit_pred = list()
  fit_roc = list()
  fit_auc = list()
  fit_gini = list()
  fit_ks = list()
  if (model == "glm" || model == "lda" || model == "rpart"){
    for (i in seq(along = train)){
      set.seed(1000)
      fit[[i]] = train(formula, data = train[[i]], method = model)
      fit_score[[i]] = predict(fit[[i]], test[[i]], type = "prob")
      fit_pred[[i]] = prediction(fit_score[[i]][["1"]], test[[i]][["status"]])
      fit_roc[[i]] = performance(fit_pred[[i]], "tpr", "fpr")
      fit_auc[[i]] = round(performance(fit_pred[[i]], measure = "auc")@y.values[[1]]*100, 2)
      fit_gini[[i]] = ((fit_auc[[i]]/100)*2 - 1)*100
      fit_ks[[i]] = round(max(attr(fit_roc[[i]], 'y.value')[[1]] - attr(fit_roc[[i]], 'x.values')[[1]]), 2)*100
    }
    
    capture.output(cat("Av_AUC: ",mean(unlist(fit_auc)),"Av_Gini:", mean(unlist(fit_gini)), 
                       "Av_KS: ", mean(unlist(fit_ks)), 
                       "Av_ACC: ", mean(fit[[i]]$resample$Accuracy)*100))
  } else if (model == "svm"){
    for (i in seq(along = train)){
      set.seed(1000)
      fit[[i]] = svm(formula, data=train[[i]], probability = TRUE)
      fit_score[[i]] = predict(fit[[i]], test[[i]],  probability = TRUE)
      fit_score1[[i]] = attr(fit_score[[i]], "prob")
      fit_pred[[i]] = prediction(fit_score1[[i]][,1], test[[i]][["status"]])
      fit_roc[[i]] = performance(fit_pred[[i]], "tpr", "fpr")
      fit_auc[[i]] = round(performance(fit_pred[[i]], measure = "auc")@y.values[[1]]*100, 2)
      fit_gini[[i]] = ((fit_auc[[i]]/100)*2 - 1)*100
      fit_ks[[i]] = round(max(attr(fit_roc[[i]], 'y.value')[[1]] - attr(fit_roc[[i]], 'x.values')[[1]]), 2)*100
    } 
    capture.output(cat("Av_AUC: ",mean(unlist(fit_auc[[i]])),"Av_Gini:", mean(unlist(fit_gini[[i]])), 
                       "Av_KS: ", mean(unlist(fit_ks[[i]])), 
                       "Av_ACC: ", mean(Accuracy(fit_score[[i]], test_net[[i]][["status"]]))*100))
  } else {
    print("Error: model not specifed")
  }}
