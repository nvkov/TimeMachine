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

# Setting number of cores in your machine. 
registerDoSNOW(makeCluster(2, type="SOCK"))


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

#Generate subsample: delete variables imputed with standard value:
#Variable in question: "return_per_articleID_week":

d.valid.subset<- d.valid[d.valid$return_per_articleID!=0.5,]



#############################################
#                                           #
#                3. MODELING                #
#                                           #
#############################################


#############################################
#Third round of selections round of selections:
baseline<- c("quantity", "revenue", "rrp", "returnBin", "yearQuarter",
              "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
              "productGroup", "return_per_customerID", "return_per_productGroup", "return_per_size", 
               "return_per_paymentMethod", "total_number_of_articles_per_order",
              "return_per_weekday")

variable<- "return_per_articleID"

filename<- variable

compare.models(d.train, d.valid.subset, baseline, variable, 2000, filename)




