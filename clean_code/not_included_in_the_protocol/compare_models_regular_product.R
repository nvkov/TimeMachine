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

load("C:/Users/Nk/Documents/Uni/APA/data_2_9.RDa")
#load("C:/Users/Nk/Documents/Uni/APA/survival.RDa")

# registering the cores
coreCount <- detectCores(logical = FALSE)
registerDoParallel(coreCount)

source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/functions/pred_functions.R")

df<- df[!is.na(df$returnQuantity)]
df<- df[,productGroup_return_ratio:=sum(returnQuantity)/sum(quantity), by=.(productGroup)]

df$yearmon<- as.yearmon(df$orderDate)
df<- df[,customer_return_ratio_per_productGroup:=sum(returnQuantity)/sum(quantity), by=.(customerID, productGroup)]
df$regular_product_factor<- as.factor(ifelse(df$regular_product>3,1,0))
df$week<- week(df$orderDate)
df$year<- year(df$orderDate)
df$month<- month(df$orderDate)

#survival.df<- survival.df[,c("customerID", "average_time_between_returns"),with=F]
#survival.df<- survival.df[!duplicated(survival.df),]

#df<- merge(df, survival.df, by="customerID")

# listing variables which are not used in models
unused.vars <- c("unique_ID", "returnQuantity", "orderID", "orderDate", "articleID", "voucherID", 
                 "customerID", "colorCode", "cumsumReturn", "special_group")

# model parameters
f.trees <- 200
f.mtry  <- 6

# data parameters
data.ratio <- 0.6

# data manipulations

trainingRows<-createDataPartition(df$returnQuantity, p=data.ratio, list=FALSE)
d.train <- df[c(trainingRows)]
d.valid <- df[c(-trainingRows)]

# Subset only for regular products:

#d.train<- df[df$orderDate< "2015-07-01",]
#d.valid<- df[df$orderDate>="2015-07-01",]

#d.valid<- d.valid[d.valid$regular_product>3,]

# marking unused variables
#drops <- names(df) %in% unused.vars

# partitioning [level 1]
#d.valid <- labeled1[-data.part1,]
#d.train <- labeled1[ data.part1,]

# creating features and imputing values
data    <- add_returns(d.train, d.valid)
d.train <- data$train
d.valid <- data$test
d.train <- d.train[, !colnames(d.train) %in% "new"]
rm(data)

# sorting coloumns in datasets
#d.train <- d.train[,   order(names(d.train))]
#d.valid <- d.valid[,   order(names(d.valid))]

# remarking unused variables
#drops <- names(d.train) %in% unused.vars

#Subset for observations of interest:
#d.train<- d.train[d.train$return_per_articleID_week!=0.5 & d.train$return_per_articleID_week!=1 & d.train$return_per_articleID_week!=0 ,]
#d.valid<- d.valid[d.valid$return_per_articleID_week!=0.5 & d.valid$return_per_articleID_week!=1 & d.valid$return_per_articleID_week!=0, ]

#############################################
#                                           #
#                3. MODELING                #
#                                           #
#############################################


m.vars1 <- c("quantity", "revenue", "price","returnBin", "yearQuarter", 
             "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
             "return_per_customerID", "return_per_articleID", "return_per_size")

m.vars2 <- c("quantity", "revenue", "price","Sale", "returnBin", "yearQuarter", 
             "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
             "return_per_customerID", "return_per_size", "return_per_articleID", "return_per_articleID_month")

#Sample for random forest:
#sample_vec<- sample(d.train$unique_ID, size=20000)
#d.train<- d.train[sample_vec,]

# training forests
m.forest1 <- randomForest(returnBin ~ ., data = na.omit(d.train[1:20000,m.vars1]), ntree = f.trees, mtry = f.mtry)
m.forest2 <- randomForest(returnBin ~ ., data = na.omit(d.train[1:20000,m.vars2]), ntree = f.trees, mtry = f.mtry)

# extracting predictions
f.pred1 <- predict(m.forest1, newdata = d.valid, type = "prob")[,"1"]
f.pred2 <- predict(m.forest2, newdata = d.valid, type = "prob")[,"1"]


# preparing predictions
f.pred1 <- prepare.prediction(f.pred1, test.data = d.valid, cutoff = 0.50)
f.pred2 <- prepare.prediction(f.pred2, test.data = d.valid, cutoff = 0.50)


# saving error measures
error1 <- prediction.error(f.pred1, test.data = d.valid)$total.error
error2 <- prediction.error(f.pred2, test.data = d.valid)$total.error

# displaying values
print(paste0("Data version 1: RF has error ", error1))
print(paste0("Data version 2: RF has error ", error2))



