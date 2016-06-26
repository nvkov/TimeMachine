# "Please make sure you have the training and validation set in a folder name data in the top level of this repo"
# "This function trains a logit model with the basic set of useable features"
# "As an argument please pass the feature Function (i.e. the variable without calling it with ()"
# "It only uses 10% of both the training and validation set for speed"
# "Also for performance reason the problem is looked at as a binary classification problem"
source("03_variable_creation/util/loadData.R")
library(data.table)

testPerformance <- function(feature_function){
  options(warn=-1)
  # Cutting down the data set
  training <- loadDataDTTraining()
  validation <- loadDataDTValidation()
  
  # These columns are used
  used_cols <- c("quantity","revenue","rrp","voucherAmount","priceIMP","Loyals","Newcommers","returningCustomer","new_feature")
  y <- training$returnBin
  yvalidation <- validation$returnBin
  
  # Add created feature
  training$new_feature <- feature_function(training)
  validation$new_feature <- feature_function(validation)
  
  # Filter datasets
  training <- training[,names(training) %in% used_cols,with=FALSE]
  validation <- validation[,names(validation) %in% used_cols,with=FALSE]
  
  fit <- glm(y~., data = training, family = binomial(link = "logit")) 
  yhat <- predict(fit, newdata = validation, type = "response")
  yhat <- round(yhat)
  
  total.error <- sum(abs(yhat - yvalidation))
  result <- paste("Baseline error is: 20515. Your error was: ",total.error)
  print(result)
  options(warn=0)
  
  return(fit)
}