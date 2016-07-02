#Compare data partitioning
#Prepare console:
rm(list=ls())

#Working directory:
setwd("C:/Users/Nk/Documents/Uni/APA/03_variable_creation/")


# Load libraries:
library("data.table")
library("ggplot2")
library("lattice")
library("vcd")
library("gridExtra")
library("StatMatch")
library("asbio")

#Load data:
load("C:/Users/Nk/Documents/Uni/APA/data_2_10.RDa")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_pred_functions_updated.R")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_plot_partitioning.R")

# Calculate Mahalanobis distance for partitions --------------------------

#Select relevant columns:
return_vars<- c("quantity", "revenue", "rrp", 
                "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
                "return_per_customerID", "return_per_productGroup", "return_per_size", 
                "return_per_paymentMethod", "total_number_of_articles_per_order",
                "return_per_weekday", "return_per_articleID")


#Load correct return ratios:
data<- add_returns(df[df$part_v1=="tt.tr",], df[df$part_v1=="t.va",])
d.train.p1 <- data.table(data$train[,return_vars])
d.valid.p1 <- data.table(data$test[,return_vars])
rm(data)

data<- add_returns(df[df$part_v2=="tt.tr",], df[df$part_v2=="t.va",])
d.train.p2 <- data.table(data$train[,return_vars])
d.valid.p2 <- data.table(data$test[,return_vars])
rm(data)

data<- add_returns(df[df$part3=="tt.tr",], df[df$part3=="t.va",])
d.train.p3 <- data.table(data$train[,return_vars])
d.valid.p3 <- data.table(data$test[,return_vars])
rm(data)

data<- add_returns(df[df$orderDate<="2015-02-17",], df[df$orderDate=="2015-02-17",])
d.train.p4 <- data.table(data$train[,return_vars])
d.valid.p4 <- data.table(data$test[,return_vars])
rm(data)


data<- add_returns(df[!is.na(df$returnQuantity),], df[is.na(df$returnQuantity),])
d.train <- data.table(data$train[,return_vars])
d.valid <- data.table(data$test[,return_vars])
rm(data)


#Calculate Mahalanobis distance for different partitionings: training data:

rm(df)

sink(file="C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/results/mahalanobis_distances.txt", append=T)
print("Compare training sets")

print(paste0("Partition 1: ", D.sq(d.train.p1, d.train)$D.sq))
#mahalanobis_part_v1$D.sq

print(paste0("Partition 2: ", D.sq(d.train.p2, d.train)$D.sq))
#mahalanobis.train_part_v2$D.sq

print(paste0("Partition 3: ", D.sq(d.train.p3, d.train)$D.sq))
#mahalanobis.train_part_v3$D.sq

print(paste0("Partition 4: ", D.sq(d.train.p4, d.train)$D.sq))
#mahalanobis.train_part_v4$D.sq

print("Compare validations sets")

#Calculate Mahalanobis distance for different partitionings: validation data:

print(paste0("Partition 1: ", D.sq(d.valid.p1, d.valid)$D.sq))
#mahalanobis.valid_part_v1$D.sq

print(paste0("Partition 2: ", D.sq(d.valid.p2, d.valid)$D.sq))
#mahalanobis.valid_part_v2$D.sq

print(paste0("Partition 3: ", D.sq(d.valid.p3, d.valid)$D.sq))
#mahalanobis.valid_part_v3$D.sq

print(paste0("Partition 4: ", D.sq(d.valid.p4, d.valid)$D.sq))
#mahalanobis.valid_part_v4$D.sq
sink()


# Pairwise Mahalanobis distance -------------------------------------------

#Calculate pairwise Mahalanobis distance only for observations present in both sets:

#Select relevant columns:
return_vars<- c("customerID", "quantity", "revenue", "rrp", 
                "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
                "return_per_customerID", "return_per_productGroup", "return_per_size", 
                "return_per_paymentMethod", "total_number_of_articles_per_order",
                "return_per_weekday", "return_per_articleID")

relevant_vars<- c("rrp", 
                "number_of_same_items_in_order", "relative_deviation_price_mean_byCustomerID",
                "return_per_customerID", "return_per_productGroup", "return_per_size", 
                "return_per_paymentMethod", "total_number_of_articles_per_order",
                "return_per_weekday", "return_per_articleID")


#Load correct return ratios:
data<- add_returns(df[df$part_v1=="tt.tr",], df[df$part_v1=="t.va",])
#d.train.p1 <- data.table(data$train[,return_vars])
d.valid.p1 <- data.table(data$test[,return_vars])
rm(data)

data<- add_returns(df[df$part_v2=="tt.tr",], df[df$part_v2=="t.va",])
#d.train.p2 <- data.table(data$train[,return_vars])
d.valid.p2 <- data.table(data$test[,return_vars])
rm(data)

data<- add_returns(df[df$part3=="tt.tr",], df[df$part3=="t.va",])
#d.train.p3 <- data.table(data$train[,return_vars])
d.valid.p3 <- data.table(data$test[,return_vars])
rm(data)

#data<- add_returns(df[df$part_v4=="train",], df[df$part_4=="class",])
#d.train.p4 <- data.table(data$train[,return_vars])
#d.valid.p4 <- data.table(data$test[,return_vars])
#rm(data)


data<- add_returns(df[!is.na(df$returnQuantity),], df[is.na(df$returnQuantity),])
#d.train <- data.table(data$train[,return_vars])
d.valid <- data.table(data$test[,return_vars])
rm(data)


#Calculate Mahalanobis distance for different partitionings: training data:

rm(df)

#Subset only for obs in both sets:
d.valid.p1<- d.valid.p1[d.valid.p1$customerID %in% d.valid$customerID,]
d.valid<- d.valid[d.valid$customerID %in% d.valid.p1$customerID,]

i<- "000001"

# M.dist<- matrix( , nrow = 22892, ncol = 2)
# for(i in unique(d.valid$customerID)){
#   for(j in 1:nrow(M.dist)){
#   M.dist[j,1]<- i
#   M.dist[j,2]<- D.sq(d.valid.p1[d.valid.p1$customerID==i,relevant_vars, with=F], d.valid[d.valid$customerID==i,relevant_vars, with=F])$D.sq
# 
#   }
# }


hi<- rbind(d.valid.p1[d.valid.p1$customerID==i,relevant_vars, with=F], d.valid[d.valid$customerID==i,relevant_vars, with=F])
Mahalanobis_Distance<-D2.dist(hi, cov(hi))
print(Mahalanobis_Distance)