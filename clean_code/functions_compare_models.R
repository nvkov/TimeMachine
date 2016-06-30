compare.models<-function(d.train, d.valid, baseline, variable){
  
  m.vars1<- baseline
  m.vars2<- c(baseline, variable)
  
  # m.vars1 <- c("quantity", "revenue", "rrp", "returnBin", "yearQuarter",
  #              "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
  #              "productGroup", "return_per_customerID", "return_per_productGroup","return_per_articleID", "return_per_size")
  # 
  #m.vars2 <- c("quantity", "revenue", "rrp", "returnBin", "yearQuarter", 
  #             "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
  #             "productGroup", "return_per_customerID", "return_per_articleID", "return_per_size",
  #              "regular_product")
  
  # training forests
  m.forest1 <- randomForest(returnBin ~ ., data = d.train[,m.vars1], ntree = f.trees, mtry = f.mtry)
  m.forest2 <- randomForest(returnBin ~ ., data = na.omit(d.train[,m.vars2]), ntree = f.trees, mtry = f.mtry)
  
  # extracting predictions
  f.pred1 <- predict(m.forest1, newdata = d.valid, type = "prob")[,"1"]
  f.pred2 <- predict(m.forest2, newdata = d.valid, type = "prob")[,"1"]
  
  
  # preparing predictions
  f.pred1 <- prepare.prediction(f.pred1, test.data = d.valid, cutoff = 0.5)
  f.pred2 <- prepare.prediction(f.pred2, test.data = d.valid, cutoff = 0.5)
  
  
  # saving error measures
  error1 <- prediction.error(f.pred1, test.data = d.valid)$total.error
  error2 <- prediction.error(f.pred2, test.data = d.valid)$total.error
  
  # displaying values
  print(paste0("Data version 1: RF has error ", error1))
  print(paste0("Data version 2: RF has error ", error2))
  
}
