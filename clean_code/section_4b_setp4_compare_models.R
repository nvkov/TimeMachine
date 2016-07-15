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
library("doSNOW")

# registering the cores
coreCount <- detectCores(logical = FALSE)
registerDoParallel(coreCount)


load("C:/Users/Nk/Documents/Uni/APA/data_2_10.RDa")

# registering the cores
#coreCount <- detectCores(logical = FALSE)
#registerDoParallel(coreCount)

source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_pred_functions_updated.R")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_compare_models.R")

df<- df[!is.na(df$returnQuantity)]

# listing variables which are not used in models
unused.vars <- c("unique_ID", "returnQuantity", "orderID", "orderDate", "articleID", "voucherID", 
                 "customerID", "colorCode", "cumSumReturn", "special_group", "part_v1",
                 "part_v2", "part3", "part_v4")

# model parameters
f.trees <- 200
f.mtry  <- 3
num_obs <- nrow(d.train) # number of observations used for rf

# data parameters
data.ratio <- 0.6

# data manipulations:

#Random data partitioning:
trainingRows<-createDataPartition(df$returnQuantity, p=data.ratio, list=FALSE)
d.train <- df[c(trainingRows)]
d.valid <- df[c(-trainingRows)]

rm(trainingRows)

#Alternative data partitioning:
#d.train<- df[df$orderDate< "2015-07-01",]
#d.valid<- df[df$orderDate>="2015-07-01",]

rm(df)

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

baseline<- c("rrp", "price", "returnBin", "yearQuarter",
             "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
             "productGroup", "return_per_customerID", "return_per_productGroup", "return_per_size", 
             "return_per_articleID")

variable<- c("total_number_of_articles_per_order")

#First round of selections:

relevantVars<- names(d.train)[!names(d.train) %in% baseline]
relevantVars<- relevantVars[!relevantVars %in% unused.vars]

for(i in relevantVars[57:60]){
variable<- i
#Compare models:
compare.models(d.train, d.valid, baseline, variable, 2000)
}

#########################################
#Second round of selections:

baseline2<- c("quantity", "revenue", "rrp", "returnBin", "yearQuarter",
             "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
             "productGroup", "return_per_customerID", "return_per_productGroup", "return_per_size", 
             "return_per_articleID", "return_per_paymentMethod")


relevantVars2<- c("customer_age", "cumArticleCount", "return_per_articleID_month", 
                  "C4", "C2", "return_per_articleID_week", 
                  "return_per_weekday", "total_number_of_articles_per_order",
                  "total_items_in_order")


for(i in relevantVars2[8:9]){
  variable<- i
  #Compare models:
  compare.models(d.train, d.valid, baseline2, variable, 20000)
}

#############################################
#Third round of selections round of selections:
baseline3<- c("quantity", "price", "rrp", "returnBin", "yearQuarter",
              "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
              "productGroup", "return_per_customerID", "return_per_productGroup", "return_per_size", 
              "return_per_articleID", "return_per_paymentMethod", "total_number_of_articles_per_order",
              "return_per_weekday")


relevantVars3<- c("customer_age", "cumArticleCount", "return_per_articleID_month", 
                  "C4", "C2", "return_per_articleID_week")

for(i in relevantVars3){
  variable<- i
  #Compare models:
  compare.models(d.train, d.valid, baseline3, variable, 200000, "rf_results_third_selection")
}
