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

#returns per day
df<- df[, returns_per_date:=sum(returnQuantity), by=.(orderDate)]
df<- df[, orders_per_date:=sum(quantity), by=.(orderDate)]
df$return_ratio_per_day<- df$returns_per_date/df$orders_per_date

df$day<- weekdays(df$orderDate)
df<- df[,productGroup_quantity:=sum(quantity), by=.(orderDate, productGroup)]
df<- df[,productGroup_return_quantity:=sum(returnQuantity), by=.(orderDate, productGroup)]
df<- df[,article_return_quantity:=sum(returnQuantity)/sum(quantity), by=.(orderDate, articleID)]

df$productGroup_return_ratio<- df$productGroup_return_quantity/df$productGroup_quantity
df<- df[,sizeCode_quantity:=sum(quantity), by=.(orderDate, sizeCode)]

#returns per month:
df$yearmon<-as.yearmon(df$orderDate)
df<- df[,article_return_ratio_per_month:=sum(returnQuantity)/sum(quantity), by=.(yearmon, articleID)]
df<- df[,productGroup_return_ratio_per_month:=sum(returnQuantity)/sum(quantity), by=.(yearmon, productGroup)]

#returns per week:
df$week<-week(df$orderDate)
df<- df[,article_return_ratio_per_week:=sum(returnQuantity)/sum(quantity), by=.(week, articleID)]
df<- df[,productGroup_return_ratio_per_week:=sum(returnQuantity)/sum(quantity), by=.(week, productGroup)]


#Save data for test:
save(df, file="C:/Users/Nk/Documents/Uni/APA/data_2_8.RDa")

df<- merge(df, survival.df, ny="customerID")



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

#library("ggplot2")
#ggplot(data=df,
#       aes(x=orderDate, y=productGroup_return_ratio, colour=factor(productGroup))) +
#  geom_line()

#Plot returns per day:
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

####################################################
#Returns per month:
for(i in unique(df$productGroup)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/return_ratio_for_productGroup_per_month", i, ".jpg"))  
  
  temp<- plot(df$yearmon[df$productGroup==i], df$productGroup_return_ratio_per_month[df$productGroup==i],
              xlab=paste0("productGroup = ", i), ylab="Return ratio")
  dev.off()
}

####################################################
#Returns per week:
for(i in unique(df$productGroup)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/return_ratio_for_productGroup_per_week", i, ".jpg"))  
  
  temp<- plot(df$week[df$productGroup==i], df$productGroup_return_ratio_per_week[df$productGroup==i],
              xlab=paste0("productGroup = ", i), ylab="Return ratio")
  dev.off()
}

#--------------------------------------------------

for(i in unique(df$articleID)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/RR_week/return_ratio_for_articleID_per_week", i, ".jpg"))  
  
  temp<- plot(df$wee[df$articleID==i], df$productGroup_return_ratio_per_week[df$articleID==i],
              xlab=paste0("articleID = ", i), ylab="Return ratio")
  dev.off()
}

####################################################
#Take a look at complementary products:

View(df[df$articleID==1887|df$articleID==1790])

####################################################
