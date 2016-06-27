#Data prep and feature creation pre partition:
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

load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/functions/pred_functions.R")

#Add possible time dimensions:
df$yearmon<- as.yearmon(df$orderDate)
df$week<- week(df$orderDate)
df$year<- year(df$orderDate)
df$month<- month(df$orderDate)

######################################################

#Add data partitioning:
load("C:/Users/Nk/Documents/Uni/APA/TimeMachine/data_part_vectors/part_full.RData")

#Merge with full dt:
part<- data.table(part)
setkey(df, "unique_ID")
setkey(part, "unique_ID")
df<- merge(df, part, by="unique_ID", all=T)

#Label partitioning correctly for 1 lvl:
df$part_v1[is.na(df$returnQuantity)]<- "class"
df$part_v2[is.na(df$returnQuantity)]<- "class"
df$part3[is.na(df$returnQuantity)]<- "class"

df$part_v1[df$part_v1=="tt.va"]<- "tt.tr"
df$part_v2[df$part_v2=="tt.va"]<- "tt.tr"
df$part3[df$part3=="tt.va"]<- "tt.tr"
df$part_v4<-ifelse(is.na(df$returnQuantity), "class", ifelse(df$orderDate<"2015-07-01", "train", "validation"))

###########################################################
#Generate additional variables:

#Look for commerce trends:
df<- df[, customers_per_date:=length(unique(customerID)), by=.(orderDate)]
df<- df[, orders_per_date:=length(unique(orderID)), by=.(orderDate)]


#------------------------------------------------------
#Order Data:
df<-df[order(df$returnBin),]
df<-df[order(df$orderID),]
df<-df[order(df$customerID),]

#------------------------------------------------------
#Customer's first date:
df<- df[, customer_first_order:=min(orderDate), by=.(customerID)]

#Customer's age:
df$customer_age<- difftime(df$orderDate, df$customer_first_order)

#Total number of articles by customerID:
df<- df[,total_number_of_articles:=.N, by=.(customerID)]

#Size variance per order and productGroup:
df<- df[, size_variance_per_order_and_product_group:=var(sizeCode), by=.(orderID, productGroup)]

#Total number of articles per order:
df<- df[, total_number_of_articles_per_order:=.N, keyby=list(customerID, orderID)][, total_number_of_orders:=.N, by=customerID]

#Number of products per customer
df<- df[,number_of_products_per_customer:=length(unique(productGroup)),by=.(customerID)]

#Number of products per order and customer
df<- df[,number_of_products_per_order_and_customer:=length(unique(productGroup)),by=.(customerID, orderID)]

#Number of unique articles per order and customer:
df<- df[,number_of_unique_products_per_order_and_customer:=length(unique(articleID)),by=.(customerID, orderID)]

#Number of products' colors per order and customer
df<- df[,number_of_unique_colors_per_order_and_customer:=length(unique(colorCode)),by=.(customerID, orderID)]

#Number of products' size per order and customer
df<- df[,number_of_unique_sizes_per_order_and_customer:=length(unique(sizeCode)),by=.(customerID, orderID)]

save(df, file="C:/Users/Nk/Documents/Uni/APA/data_2_10.RDa")


