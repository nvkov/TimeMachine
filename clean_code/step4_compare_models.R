rm(list=ls())

# libraries
library(beepr)
library(caret)
library(nnet)
library(randomForest)
library(xgboost)
library(LiblineaR)
library(data.table)
library(Matrix)
library(SparseM)
library(foreach)
library(doParallel) 
library(zoo)

load("C:/Users/Nk/Documents/Uni/APA/data_2_10.RDa")

# registering the cores
coreCount <- detectCores(logical = FALSE)
registerDoParallel(coreCount)

source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/pred_functions_updated.R")

df<- df[!is.na(df$returnQuantity)]


# listing variables which are not used in models
unused.vars <- c("unique_ID", "returnQuantity", "orderID", "orderDate", "articleID", "voucherID", 
                 "customerID", "colorCode", "cumsumReturn", "special_group")

# model parameters
f.trees <- 200
f.mtry  <- 3

# data parameters
data.ratio <- 0.6

# data manipulations:

#Random data partitioning:
trainingRows<-createDataPartition(df$returnQuantity, p=data.ratio, list=FALSE)
d.train <- df[c(trainingRows)]
d.valid <- df[c(-trainingRows)]

#Alternative data partitioning:
d.train<- df[df$orderDate< "2015-07-01",]
d.valid<- df[df$orderDate>="2015-07-01",]


# creating features and imputing values
data    <- add_returns(d.train, d.valid)
d.train <- data$train
d.valid <- data$test
d.train <- d.train[, !colnames(d.train) %in% "new"]
rm(data)

# sorting coloumns in datasets
d.train <- d.train[,   order(names(d.train))]
d.valid <- d.valid[,   order(names(d.valid))]

# remarking unused variables
#drops <- names(d.train) %in% unused.vars


#############################################
#                                           #
#                3. MODELING                #
#                                           #
#############################################

compare.models<-function(d.train, d.valid, baseline, variable){

m.vars1<- baseline
m.vars2<- c(baseline, variable)
  
#m.vars1 <- c("quantity", "revenue", "rrp", "returnBin", "yearQuarter", 
#             "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
#             "productGroup", "return_per_customerID", "return_per_productGroup","return_per_articleID", "return_per_size")

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


