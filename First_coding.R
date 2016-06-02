# Find one time customers:
library("data.table")

load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

df<- df[, rank := frank(orderID, ties.method="dense"), by=customerID]

df<- df[, maxRank := max(rank), by=customerID]

df_one_time<- df[df$maxRank==1,]
df<- df[df$maxRank>1,]

#df$maxRank<- NULL

df<- df[!is.na(df$returnQuantity)]

df<- df[,number_of_articles_in_the_order:=sum(quantity), by=.(customerID, orderID)]
df$returnRatio<- df$returnQuantity/df$number_of_articles_in_the_order

#==========================================
#Define variables
df$weekday<- as.factor(weekdays(df$orderDate))
df$month<- as.factor(month(df$orderDate))

tapply(df$returnBin, df$month, summary)

df<- df[ , first_date_for_customer:=min(orderDate), by=.(customerID) ]

#See if customers shop at different times:
small<- df[df$maxRank==401,]

tapply(df$returnQuantity, df$weekday, sum)
tapply(df$quantity, df$weekday, sum)

#Preferred day of the week:
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

df<- df[ ,preferred_days:=Mode(weekday), by=.(customerID)]

#Preferred day of return:

df.subset<- df[df$returnQuantity>0,]
df.subset<- df.subset[,.(preferred_return_weekday=Mode(weekday)),by=.(customerID)]

setkey(df, "customerID")
setkey(df.subset, "customerID")

df<- df[df.subset]

df$returnRatio<- df$returnQuantity/df$quantity
df.returns<- df[,.(return_quantity_per_date= sum(df$returnQuantity),quantity_per_date=sum(df$quantity)), by=.(orderDate)]

plot(df$orderDate, df$return_ratio_per_date) 
tapply(df$returnRatio, df$weekday, plot, df$order$Date)