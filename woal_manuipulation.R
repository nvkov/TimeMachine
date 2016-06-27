#Look into the future:

rm(list=ls())

#Load libraries:
library("data.table")
library("zoo")

#Load data:
load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")
class<- df[is.na(returnQuantity)]
df<- df[!is.na(df$returnQuantity)]

#Relevant cols:
relevantCols<- c("customerID", "articleID", "sizeCode", "colorCode", "productGroup",
                 "woal", "price", "returnBin", "orderID", "orderDate", "quantity", "returnQuantity")

#df<- df[, relevantCols, with=F]

df<- df[,orderingStyle:=sum(as.numeric(as.character(woal))), by=.(customerID)]
df<- df[,orderingStyle_ratio:=sum(as.numeric(as.character(woal)))/.N, by=.(customerID)]
df<- df[,return_ratio_per_CustomerID:=sum(returnQuantity)/sum(quantity), by=.(customerID)]
df<- df[,regular_product:=.N,by=.(customerID, articleID, colorCode, sizeCode)]
#df$regular_product<- ifelse(df$regular_product>1, 1, 0)

# Look when product was ordered again, but not returned:
#View(df[df$woal==1 & df$returnBin==0 &df$quantity>0,])
#View(df[df$customerID=="000032",])

table(df$returnQuantity, df$regular_product)

#Check if this happens in classinfication:
class<-class[,regular_product:=.N,by=.(customerID, articleID, colorCode, sizeCode)]

#impute from labelled dataset:
impute.df<- df[,c("customerID", "articleID", "sizeCode", 
                  "colorCode", "regular_product"), with=F]
impute.df<- impute.df[!duplicated(impute.df),]

class<-merge(class, impute.df, by=c("customerID", "articleID", "sizeCode", "colorCode"), all.x=T)

class$regular_product.y[is.na(class$regular_product.y)]<- 0
class$regular_product<- class$regular_product.x + class$regular_product.y
class<- class[!duplicated(class),] #336884

save(df, file="C:/Users/Nk/Documents/Uni/APA/data_2_9.RDa")

View(df[df$customerID=="107459" & df$articleID=="3278",])


