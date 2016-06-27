rm(list=ls())

# Find one time customers:
library("data.table")

load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

df<- df[!is.na(df$returnQuantity)]

relevantCols<- c("unique_ID", "customerID", "articleID", "orderID","orderDate", "returnBin", 
                 "returnQuantity", "quantity", "sizeCode", "colorCode", "rrp", "productGroup")
df<- df[ ,relevantCols, with=F]
#------------------------------------------------------
#Functions:
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

#------------------------------------------------------

df<-df[order(df$returnBin),]
df<-df[order(df$orderDate),]

df<-df[order(df$customerID),]
#------------------------------------------------------
df<- df[,return_quantity_per_order:=sum(returnQuantity), by=.(customerID, orderID)]
df<- df[,quantity_per_order:=sum(quantity), by=.(customerID, orderID)]

#small<- df[df$customerID=="000001"]

#small$returnRatio<-small$return_quantity_per_order/small$quantity_per_order
#plot(small$orderDate, small$returnRation, type="l")

#------------------------------------------------------
df<- df[, customer_first_order:=min(orderDate), 
        by=.(customerID)]

df$customer_age<- difftime(df$orderDate, df$customer_first_order)


df$survival<- ifelse(df$returnQuantity>0, 1, 0)

#======Find consecutives for survival analysis:

#Prepare data for the analysis:
df1<-df[df$returnQuantity>0,]
df1<- df1[,.(last_return_date=max(orderDate)), by=.(customerID)]
setkey(df1, "customerID") 
setkey(df, "customerID") 

df<- df[df1]
df<- df[df$orderDate<= df$last_return_date,]

number_of_orders_until_return<-diff(c(0, as.integer(gregexpr("1", paste0(df$survival, collapse = ""))[[1]]) ) )   
consec_obs<- rep(number_of_orders_until_return, times=number_of_orders_until_return)
consec_obs_idx<- rep(1:length(number_of_orders_until_return), times=number_of_orders_until_return)
df<- cbind(df, consec_obs, consec_obs_idx)

df<- df[,total_number_of_articles:=.N, by=.(customerID)]

df<- df[,first_date:= min(orderDate), by=.(consec_obs_idx)]

df<- df[,last_date:= max(orderDate), by=.(consec_obs_idx)]

df<- df[, number_of_articles_purchased_until_return:=sum(quantity), by=.(consec_obs_idx)]

df<- df[, number_of_articles_returned_at_return:=sum(returnQuantity), by=.(consec_obs_idx)]
df<- df[,number_of_articles_kept:=number_of_articles_purchased_until_return-number_of_articles_returned_at_return,]
df<- df[,total_number_of_returns:=sum(returnQuantity), by=.(customerID)]
df<- df[, size_variance_per_order_and_product_group:=var(sizeCode), by=.(orderID, productGroup)]

df1<- df[df$returnQuantity==0,]
df1<- df1[,mode_size_kept=Mode(sizeCode), by=.(customerID, productGroup)]
