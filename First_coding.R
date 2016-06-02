# Find one time customers:
library("data.table")
df<- df[, rank := frank(orderID, ties.method="dense"), by=customerID]

df<- df[, maxRank := max(rank), by=customerID]

df_one_time<- df[df$maxRank==1,]
df<- df[df$maxRank>1,]

df$maxRank<- NULL

df<- df[!is.na(df$returnQuantity)]

df<- df[,number_of_articles_in_the_order:=sum(quantity), by=.(customerID, orderID)]
df$returnRatio<- df$returnQuantity/df$number_of_articles_in_the_order

#==========================================
df$weekday<- weekdays(df$orderDate)
df$month<- month(df$orderDate)
df$weeks<- 

tapply(df$returnBin, df$month, summary)

