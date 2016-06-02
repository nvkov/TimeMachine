rm(list=ls())

# Find one time customers:
library("data.table")

load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

df<- df[!is.na(df$returnQuantity)]

#relevantCols<- c("unique_ID", "customerID", "articleID", "orderID","orderDate", "returnBin", 
#                 "returnQuantity", "quantity", "sizeCode", "colorCode", "rrp", "productGroup")

#df<- df[ ,relevantCols, with=F]

#Look for commerce trends:
df<- df[, customers_per_date:=length(unique(customerID)), by=.(orderDate)]

#Look at number of returns for different dynamics: 
df<- df[, returns_per_date:=sum(returnQuantity), by=.(orderDate)]
df<- df[, orders_per_date:=sum(quantity), by=.(orderDate)]
df$return_ratio_per_day<- df$returns_per_date/df$orders_per_date
df$day<- weekdays(df$orderDate)
df<- df[,productGroup_quantity:=sum(quantity), by=.(orderDate, productGroup)]
df<- df[,productGroup_return_quantity:=sum(returnQuantity), by=.(orderDate, productGroup)]
df<- df[,article_return_quantity:=sum(returnQuantity)/sum(quantity), by=.(orderDate, articleID)]


df$productGroup_return_ratio<- df$productGroup_return_quantity/df$productGroup_quantity


df<- df[,sizeCode_quantity:=sum(quantity), by=.(orderDate, sizeCode)]


#Plot for intensity days:
#par(mfrow=c(2,1))
#plot(df$orderDate, df$customers_per_date)
#plot(df$orderDate, df$return_ratio_per_day)

plot(df$customers_per_date, df$return_ratio_per_day)


#plot(df$orderDate, df$customers_per_date)

#Find the dates with highest orders:
sort(unique(df$orderDate[df$customers_per_date>quantile(df$customers_per_date)[4]]))

sort(unique(df$orderDate[df$customers_per_date<quantile(df$customers_per_date)[2]]))

#See whats the difference between days with the same number of customers
# But different return ratios
Mode(df$customers_per_date)

plot(df$orderDate[df$customers_per_date==1183], df$return_ratio_per_day[df$customers_per_date==1183])
text(df$orderDate[df$customers_per_date==1183], df$return_ratio_per_day[df$customers_per_date==1183], df$day[df$customers_per_date==1183])

#Look for seasonality in products:
plot(df$orderDate, df$productGroup)

library("ggplot2")

ggplot(data=df,
       aes(x=orderDate, y=productGroup_return_ratio, colour=factor(productGroup))) +
  geom_line()


plot(df$orderDate[df$productGroup==3], df$productGroup_return_ratio[df$productGroup==3])
plot(df$orderDate[df$productGroup==13], df$productGroup_return_ratio[df$productGroup==13])

View(df[df$productGroup==13,])


ggplot(df, aes(x=orderDate, 
               y=productGroup_return_ratio), group=productGroup, color=productGroup) +
  geom_point()

for(i in unique(df$productGroup)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/return_ratio_for_productGroup_", i, ".jpg"))  
  
temp<- plot(df$orderDate[df$productGroup==i], df$productGroup_return_ratio[df$productGroup==i],
     xlab=paste0("productGroup = ", i), ylab="Return ratio")
  dev.off()
}

for(i in unique(df$articleID)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/return_ratio_for_articleID_", i, ".jpg"))  
  
  temp<- plot(df$orderDate[df$articleID==i], df$productGroup_return_ratio[df$articleID==i],
              xlab=paste0("productGroup = ", i), ylab="Return ratio")
  dev.off()
}
