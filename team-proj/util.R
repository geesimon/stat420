library(lmtest)

get_bp_decision = function(model, alpha) {
  decide = unname(bptest(model)$p.value < alpha)
  ifelse(decide, "Reject", "Fail to Reject")
}

get_sw_decision = function(model, alpha) {
  decide = unname(shapiro.test(resid(model))$p.value < alpha)
  ifelse(decide, "Reject", "Fail to Reject")
}

get_loocv_rmse = function(model) {
  sqrt(mean((resid(model) / (1 - hatvalues(model))) ^ 2))
}

## function to calculate rmse
rmse  = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2, na.rm=TRUE))
}

get_adj_r2 = function(model) {
  summary(model)$adj.r.squared
}

plot_fitted_resid = function(model, pointcol = "dodgerblue", linecol = "darkorange") {
  plot(fitted(model), resid(model), 
       col = pointcol, pch = 20, cex = 1.5,
       xlab = "Fitted", ylab = "Residuals")
  abline(h = 0, col = linecol, lwd = 2)
}

plot_qq = function(model, pointcol = "dodgerblue", linecol = "darkorange") {
  qqnorm(resid(model), col = pointcol, pch = 20, cex = 1.5)
  qqline(resid(model), col = linecol, lwd = 2)
}


#Calcuate log value of a data frame column and change its name
log_data = function(data, var_names) {
  for(name in var_names) {
    data[paste("Log_", name, sep = "")] = log(data[[name]])
  }
  data[, !colnames(data) %in% var_names]
}

#Build formular for lm from array of predictor names
build_predictors_str = function(predictor_names) {
  str_var = ""
  add_plus = FALSE
  
  for (predictor_name in predictor_names) {
    if(add_plus) str_var = paste(str_var, "+")
    else {
      str_var = paste(str_var, "(")
      add_plus = TRUE
    }
    
    str_var = paste(str_var, predictor_name)
  }
  paste(str_var, ")")
}

build_formula = function(response_name, predictor_names, interactive_names = NULL, interaction = 1) {
  str_formula = paste(response_name, "~")
  
  str_formula = paste(str_formula, build_predictors_str(predictor_names))
  if(!is.null(interactive_names)) {
    str_formula = paste(str_formula, "*",build_predictors_str(interactive_names))
  }

  if(interaction > 1) str_formula = paste(str_formula, "^ ", interaction)
  
  as.formula(str_formula)
}

#Get predictor original name from dummy variable names
get_predictor_name = function(all_names, dummy_names) {
  names = c()
  for(name in all_names) {
    nmatch = sum(name == sapply(dummy_names, function(n) {substr(n, 1, nchar(name))}))
    if(nmatch > 0) {
      names = c(names, name)
    }
  }
  names
}

## function to compare models using rmse 
compareModels = function(models, modelNames){
  train_error = rep(0, length(modelNames))
  test_error = rep(0, length(modelNames))
  for (i in 1:length(modelNames)){
    ## train errors
    train_error[i] = rmse(train_data$Log_SalePrice, predict(models[[ modelNames[i] ]], newdata=train_data))
    ## test errors
    test_error[i] = rmse(test_data$Log_SalePrice, predict(models[[ modelNames[i] ]], newdata=test_data))
  }
  df = data.frame("models"=modelNames, "trainErrors"=train_error, "testErrors"=test_error)
  return(df)
}

plot_errors = function(testData, model, model_name){
  ndf = data.frame("Log_SalePrice"=testData["Log_SalePrice"], "pred.Log_SalePrice" = predict(model, 
                                                                                      newdata=testData))
  plot(ndf$Log_SalePricee - ndf$pred.Log_SalePrice, col = "blue", ylab = "Pred Errors", main = model_name)
}
