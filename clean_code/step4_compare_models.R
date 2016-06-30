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

baseline<- c("quantity", "revenue", "rrp", "returnBin", "yearQuarter",
             "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
             "productGroup", "return_per_customerID", "return_per_productGroup","return_per_articleID", "return_per_size")

variable<- "regular_product"



