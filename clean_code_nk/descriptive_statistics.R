# Desctripitve stats for the Data section of the paper

rm(list=ls())

# Find one time customers:
library("data.table")

load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

class<-df[is.na(df$returnQuantity)]
df<- df[!is.na(df$returnQuantity)]

##################################################

#Descriptive stats labeled data:
nrow(df)
length(unique(df$customerID))
length(unique(df$articleID))
summary(df$orderDate)
length(unique(df$customerID[df$totOrder==1])) 
length(unique(df$customerID[df$totOrder>1]))

#Descriptive stats unlabeled data:
nrow(class)
length(unique(class$customerID))
length(unique(class$articleID))
summary(class$orderDate)
length(unique(class$customerID[class$totOrder==1]))
length(unique(class$customerID[class$totOrder>1]))


#Look at intersection of labeled/unlabeled data:

#customer level:
sum(unique(class$customerID) %in% unique(df$customerID))
sum(unique(class$customerID[class$totOrder==1]) %in% unique(df$customerID))
sum(unique(class$customerID) %in% unique(df$customerID[df$totOrder==1]))

#product level:
sum(unique(class$articleID) %in% unique(df$articleID))
sum(!unique(class$articleID) %in% unique(df$articleID))
sum(!unique(df$articleID) %in% unique(class$articleID))


