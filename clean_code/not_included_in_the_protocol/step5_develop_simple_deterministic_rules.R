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

df<- df[!is.na(df$returnQuantity),]

#Customer specific preferred products:

products<- df[,.(preferred_product=.N, preferred_product_quantity=sum(quantity), 
                 product_return=sum(returnQuantity), 
                 product_return_bin= sum(as.numeric(as.character(returnBin)))),
              by=.(articleID, sizeCode, colorCode, customerID) ]
products<- products[products$preferred_product>1, ]

table(products$preferred_product, products$product_return_bin)
