# Parameters:
# d.train   -   training set
# d.valid   -   validation set
# baseline  -   character vector witha list of variable in the baseline model
# variable  -   new variable to be assessed against the baseline model


compare.models<-function(d.train, d.valid, baseline, variable){
  
  m.vars1<- baseline
  #m.vars2<- c(baseline, variable)
  
  # training forests
  m.forest1 <-randomForest(returnBin ~ ., data = d.train[1:2000,m.vars1], ntree = f.trees, mtry = f.mtry)
  print("Estimated data version 1")
  
  #m.forest2 <- randomForest(returnBin ~ ., data = d.train[1:num_obs,m.vars2], ntree = f.trees, mtry = f.mtry)
  print("Estimated data version 2")
  
  # extracting predictions
  f.pred1 <- predict(m.forest1, newdata = d.valid, type = "prob")[,"1"]
  #f.pred2 <- predict(m.forest2, newdata = d.valid, type = "prob")[,"1"]
  
  
  # preparing predictions
  f.pred1 <- prepare.prediction(f.pred1, test.data = d.valid, cutoff = 0.5)
  #f.pred2 <- prepare.prediction(f.pred2, test.data = d.valid, cutoff = 0.5)
  
  
  # saving error measures
  error1 <- prediction.error(f.pred1, test.data = d.valid)$total.error
  #error2 <- prediction.error(f.pred2, test.data = d.valid)$total.error
  
  # displaying values
  print(paste0("Data version 1: RF has error ", error1))
  #print(paste0("Data version 2: RF has error ", error2))
  
  #Save results to external file:
  sink("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/results/results_from_rf", append=T, split=FALSE)
  print(paste0("Data version 1: RF has error ", error1, "; Variables used: ", baseline))
  }
