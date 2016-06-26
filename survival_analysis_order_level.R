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
df<-df[order(df$orderID),]
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
#df<- df[, size_variance_per_order_and_product_group:=var(sizeCode), by=.(orderID, productGroup)]
df<- df[, total_number_of_articles_per_order:=.N, keyby=list(customerID, orderID)][, total_number_of_orders:=.N, by=customerID]

#Subset only for customers with longer buying behavior:
df<- df[df$total_number_of_orders>5]

#Number of products per customer
df<- df[,number_of_products_per_customer:=length(unique(productGroup)),by=.(customerID)]

#Number of products per order and customer
df<- df[,number_of_products_per_order_and_customer:=length(unique(productGroup)),by=.(customerID, orderID)]

#Number of unique products per
df<- df[,number_of_unique_products_per_order_and_customer:=length(unique(articleID)),by=.(customerID, orderID)]

#Number of products' colors per order and customer
df<- df[,number_of_unique_colors_per_order_and_customer:=length(unique(colorCode)),by=.(customerID, orderID)]

#Number of products' size per order and customer
df<- df[,number_of_unique_sizes_per_order_and_customer:=length(unique(sizeCode)),by=.(customerID, orderID)]


#How many long time customers left:
length(unique(df$customerID))

order.df<- df[, .(return_order_level=max(as.numeric(as.character(returnBin))),
                  total_number_of_orders=sum(quantity),
                  total_number_of_returns=sum(returnQuantity), 
                  total_revenue=sum(rrp), 
                  total_money_saved=sum(rrp*returnQuantity)), 
              by=.(customerID, orderID, orderDate)]


order.df<-order.df[order(order.df$orderID),]
order.df<-order.df[order(order.df$customerID),]

#Remove censored data:
df1<-order.df[order.df$return_order_level>0,]
df1<- df1[,.(last_return_date=max(orderDate)), by=.(customerID)]
setkey(df1, "customerID") 
setkey(order.df, "customerID") 

order.df<- order.df[df1]
order.df<- order.df[order.df$orderDate<= order.df$last_return_date,]

#Calculate survival time:
number_of_orders_until_return<-diff(c(0, as.integer(gregexpr("1", paste0(order.df$return_order_level, collapse = ""))[[1]]) ) )   
survival_time<- rep(number_of_orders_until_return, times=number_of_orders_until_return)
consec_obs_idx<- rep(1:length(number_of_orders_until_return), times=number_of_orders_until_return)
order.df<- cbind(order.df, survival_time, consec_obs_idx)

survival.df<- order.df[,.(total_revenue=sum(total_revenue),
                          first_date= min(orderDate),
                          last_date=max(orderDate),
                          total_money_saved=sum(total_money_saved),
                          total_number_of_returns=sum(total_number_of_returns),
                          total_number_of_orders= sum(total_number_of_orders)),
                       
                       by=.(customerID, consec_obs_idx, survival_time)]
survival.df$elapsed_time<- difftime(survival.df$last_date, survival.df$first_date, units="days")
#Survival analysis:
library("survival")
mini.surv <- survfit(Surv(survival.df$survival_time)~ 1, conf.type="none")
summary(mini.surv)

plot(mini.surv, xlab="Time", ylab="Survival Probability")

custID<- as.character(unique(survival.df$customerID)[234])
coxph.model <- coxph( Surv(survival.df$survival_time[survival.df$customerID==custID])~survival.df$elapsed_time[survival.df$customerID==custID]+ 
                                                                                        survival.df$total_revenue[survival.df$customerID==custID]+
                                                                                        survival.df$total_money_saved[survival.df$customerID==custID]+
                                                                                        survival.df$total_number_of_returns[survival.df$customerID==custID]+
                                                                                        survival.df$total_number_of_orders[survival.df$customerID==custID],
                                                                                        method="breslow")
summary(coxph.model)

coxph.model <- coxph( Surv(survival.df$survival_time)~survival.df$elapsed_time+ 
                        survival.df$total_revenue+
                        survival.df$total_money_saved+
                        survival.df$total_number_of_returns+
                        survival.df$total_number_of_orders,
                      method="breslow")
summary(coxph.model)

plot(survfit(coxph.model), xlab='Weeks', 
     ylab='Proportion Not Rearrested')
lines(mini.surv, col="red")

#########################################
#Average number of orders before return
survival.df<- survival.df[,total_number_of_orders:=sum(survival_time),by=.(customerID)]
survival.df<- survival.df[,total_survival_events:= .N, by=.(customerID)]
survival.df$average_time_between_returns<- survival.df$total_number_of_orders/survival.df$total_survival_events



#Subset for one time returners:
survival.df<- subset(survival.df, survival.df$total_number_of_orders>3)


#Plot average time between returns to categorize customers:
hist(survival.df$average_time_between_returns, xlim=c(1, 5), 
     xlab="Average time between returns", main="Customer return behavior", breaks=2000)


#Prepare data to run survival functions on a group:
survival.df$strata<- ifelse(survival.df$average_time_between_returns==1,"constant returners", 
                            ifelse(survival.df$average_time_between_returns<=1.5, "undecided",
                                   ifelse(survival.df$average_time_between_returns<=1.2, "sporadic", 
                                          "loyal")))



coxph.model <- coxph( Surv(survival.df$survival_time)~survival.df$elapsed_time+ 
                        survival.df$total_revenue+
                        survival.df$total_money_saved+
                        survival.df$total_number_of_returns+
                        survival.df$total_number_of_orders +
                        strata(survival.df$strata),
                      method="efron")
summary(coxph.model)

plot(survfit(coxph.model), xlab='Consecutive orders', 
     ylab='Proportion Not Rearrested')
lines(mini.surv, col=c("red", "blue"))

####################################################################
#Save relevant variables for merge:

save(survival.df, file="C:/Users/Nk/Documents/Uni/APA/survival.RDa")

