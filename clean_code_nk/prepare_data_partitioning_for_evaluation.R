#Compare data partitioning

#Prepare console:
rm(list=ls())

#Working directory:
setwd("C:/Users/Nk/Documents/Uni/APA/03_variable_creation/")
source("pred_functions.R")
source("C:/Users/Nk/Documents/Uni/APA/clean_code_nk/plot_partitioning.R")


# Load libraries:
library("data.table")
library("ggplot2")
library("lattice")
library("vcd")
library("gridExtra")

#Load data:
load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")
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

#Clean working space:
rm(part)

#Load correct return ratios:
data<- add_returns(df[df$part_v1=="tt.tr",], df[df$part_v1=="t.va",])
d.train.p1 <- data$train[,55:60]
d.valid.p1 <- data.table(data$test[,55:60])
d.train.p1 <- d.train[, !colnames(d.train.p1) %in% "new"]
rm(data)

data<- add_returns(df[df$part_v2=="tt.tr",], df[df$part_v2=="t.va",])
d.train.p2 <- data.table(data$train[,55:60])
d.valid.p2 <- data.table(data$test[,55:60])
d.train.p2 <- d.train.p2[, !colnames(d.train.p2) %in% "new"]
rm(data)

data<- add_returns(df[df$part3=="tt.tr",], df[df$part3=="t.va",])
d.train.p3 <- data.table(data$train[,55:60])
d.valid.p3 <- data.table(data$test[,55:60])
d.train.p3 <- d.train.p3[, !colnames(d.train.p3) %in% "new"]
rm(data)

data<- add_returns(df[df$part_v4=="train",], df[df$part_v4=="class",])
d.train.p4 <- data.table(data$train[,55:60])
d.valid.p4 <- data.table(data$test[,55:60])
d.train.p4 <- d.train.p4[, !colnames(d.train.p4) %in% "new"]
rm(data)


#========================================================
#Apply functions:
#Find factor vars:
factor.vars<- names(Filter(is.factor, df))

#Find numerical vars:
numeric.vars<- names(Filter(is.numeric, df))
numeric.vars<- numeric.vars[-5]

#Plot univariate distributions
# For Validation set vars pre-partitioning:
for(i in numeric.vars){
  plot.numerical.validation(i, df)
}

#For training set vars pre partitioning:
for(i in numeric.vars){
  plot.numerical.train(i, df)
}

#For validation set vars post partitioning:
for(i in names(d.valid.p1)){
  plot.returns.validation(i)
}

#For training set vars post partitioning:
for(i in names(d.train.p1)){
  plot.returns.validation(i)
}

