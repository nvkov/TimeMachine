#Compare data partitioning
#Prepare console:
rm(list=ls())

#Working directory:
setwd("C:/Users/Nk/Documents/Uni/APA/03_variable_creation/")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code_nk/pred_functions_updated.R")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code_nk/plot_partitioning.R")


# Load libraries:
library("data.table")
library("ggplot2")
library("lattice")
library("vcd")
library("gridExtra")
library("StatMatch")

#Load data:
load("C:/Users/Nk/Documents/Uni/APA/data_2_10.RDa")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code_nk/pred_functions_updated.R")

df$weekday<- weekdays(df$orderDate)

#Select relevant columns:
return_vars<- c("return_per_color", "return_per_size", "return_per_productGroup", 
                "return_per_voucherID", "return_per_paymentMethod", "return_per_customerID", 
                "return_per_articleID", "return_per_articleID_week", "return_per_articleID_month", 
                "return_per_weekday", "return_per_week", "return_per_month", 
                "return_per_weekday_year")


#Load correct return ratios:
data<- add_returns(df[df$part_v1=="tt.tr",], df[df$part_v1=="t.va",])
d.train.p1 <- data.table(data$train[,return_vars])
d.valid.p1 <- data.table(data$test[,return_vars])
#d.train.p1 <- d.train[, !colnames(d.train) %in% "new"]
rm(data)

data<- add_returns(df[df$part_v2=="tt.tr",], df[df$part_v2=="t.va",])
d.train.p2 <- data.table(data$train[,return_vars])
d.valid.p2 <- data.table(data$test[,return_vars])
#d.train.p2 <- d.train[, !colnames(d.train) %in% "new"]
rm(data)

data<- add_returns(df[df$part3=="tt.tr",], df[df$part3=="t.va",])
d.train.p3 <- data.table(data$train[,return_vars])
d.valid.p3 <- data.table(data$test[,return_vars])
#d.train.p3 <- d.train[, !colnames(d.train) %in% "new"]
rm(data)

data<- add_returns(df[df$part_v4=="train",], df[df$part_4=="class",])
d.train.p4 <- data.table(data$train[,return_vars])
d.valid.p4 <- data.table(data$test[,return_vars])
#d.train.p4 <- d.train[, !colnames(d.train) %in% "new"]
rm(data)


data<- add_returns(df[!is.na(df$returnQuantity),], df[is.na(df$returnQuantity),])
d.train <- data.table(data$train[,return_vars])
d.valid <- data.table(data$test[,return_vars])
#d.train <- d.train[, !colnames(d.train) %in% "new"]
rm(data)


#Plot different partitionings for comparison:
#Plot pre-partition generated vars:
#========================================================
#Apply functions:
#Find factor vars:
factor.vars<- names(Filter(is.factor, df))

#Find numerical vars:
numeric.vars<- names(Filter(is.numeric, df))
numeric.vars<- numeric.vars[-5]

#Plot univariate distributions
for(i in numeric.vars){
  plot.numerical.validation(i, df)
}

for(i in numeric.vars){
  plot.numerical.train(i, df)
}

####################################
#Apply on return variables:
for(i in return_vars){
  plot.returns.validation(i)
}

#---------------------
for(i in return_vars){
  plot.returns.train(i)
}


