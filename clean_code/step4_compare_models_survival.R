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
load("C:/Users/Nk/Documents/Uni/APA/survival_data.RDa")
# registering the cores
#coreCount <- detectCores(logical = FALSE)
#registerDoParallel(coreCount)

source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_pred_functions_updated.R")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_compare_models.R")

df<- df[!is.na(df$returnQuantity)]

sdf<- survival.data.final[,.(average_length_of_survival=max(average_length_of_survival)), by=.(customerID)]
rm(survival.data.final)

#Small data preparation for survival 
df<- df[,cum_article_count:=cumsum(quantity), by=customerID]
df<- df[,cum_article_count_order:= max(cum_article_count), by=orderID]
df<- merge(df, sdf, by="customerID")

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
#d.train <- d.train[, !colnames(d.train) %in% "new"]
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

baseline2<- c("quantity", "revenue", "rrp", "returnBin", "yearQuarter",
              "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
              "productGroup", "return_per_customerID", "return_per_productGroup", "return_per_size", 
              "return_per_articleID", "return_per_paymentMethod")


baseline3<- c("quantity", "price", "rrp", "returnBin", "yearQuarter",
              "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
              "productGroup", "return_per_customerID", "return_per_productGroup", "return_per_size", 
              "return_per_articleID", "return_per_paymentMethod", "total_number_of_articles_per_order",
              "return_per_weekday")


variable<- c("average_length_of_survival")
variable2<- c("cum_article_count")
variable3<- c("cum_article_count_order")



compare.models(d.train, d.valid, baseline3, variable2, 20000, "cumulative_counts")
compare.models(d.train, d.valid, baseline3, variable3, 20000, "cumulative_counts")

baseline4<- c("quantity", "price", "rrp", "returnBin", "yearQuarter",
              "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
              "productGroup", "return_per_customerID", "return_per_productGroup", "return_per_size", 
              "return_per_articleID", "return_per_paymentMethod", "total_number_of_articles_per_order",
              "return_per_weekday", "average_length_of_survival")

compare.models(d.train, d.valid, baseline4, variable2, 20000, "cumulative_counts")



#########################################
#Second round of selections:






