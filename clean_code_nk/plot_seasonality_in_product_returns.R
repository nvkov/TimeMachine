rm(list=ls())

#Load libraries:
library("data.table")
library("zoo")

#Load data:
load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

load("C:/Users/Nk/Documents/Uni/APA/data_2_8.RDa")
load("C:/Users/Nk/Documents/Uni/APA/common_products.RDa")

#Subset for common products:
df<-df[df$articleID %in% common_products,]

#Plot for intensity days:
plot(df$customers_per_date, df$return_ratio_per_day)

#Find the dates with highest orders:
sort(unique(df$orderDate[df$customers_per_date>quantile(df$customers_per_date)[4]]))
sort(unique(df$orderDate[df$customers_per_date<quantile(df$customers_per_date)[2]]))

#See whats the difference between days with the same number of customers
# But different return ratios
#Mode(df$customers_per_date)

plot(df$orderDate[df$customers_per_date==1183], df$return_ratio_per_day[df$customers_per_date==1183])
text(df$orderDate[df$customers_per_date==1183], df$return_ratio_per_day[df$customers_per_date==1183], df$day[df$customers_per_date==1183])

#Look for seasonality in products:
plot(df$orderDate, df$productGroup)


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
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/RR_month/return_ratio_for_productGroup_per_month", i, ".jpg"))  
  
  temp<- plot(df$yearmon[df$productGroup==i], df$productGroup_return_ratio_per_month[df$productGroup==i],
              xlab=paste0("productGroup = ", i), ylab="Return ratio")
  dev.off()
}

####################################################
#Returns per week:
for(i in unique(df$productGroup)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/RR_week/return_ratio_for_productGroup_per_week", i, ".jpg"))  
  
  temp<- plot(df$week[df$productGroup==i], df$productGroup_return_ratio_per_week[df$productGroup==i],
              xlab=paste0("productGroup = ", i), ylab="Return ratio")
  
  fit <- loess(df$productGroup_return_ratio_per_week[df$productGroup==i] ~ df$week[df$productGroup==i], span=0.6)
  my.count <- seq(from=1, to=length(unique(df$week)), by=1)
  pred6 <- predict(fit, my.count, se=TRUE)
  
  dev.off()
}

#--------------------------------------------------

for(i in unique(df$articleID)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/RR_week/return_ratio_for_articleID_per_week", i, ".jpg"))  
  
  temp<- plot(df$week[df$articleID==i], df$productGroup_return_ratio_per_week[df$articleID==i],
              xlab=paste0("articleID = ", i), ylab="Return ratio")
  
    dev.off()
}

####################################################
#Take a look at complementary products:

View(df[df$articleID==1887|df$articleID==1790])

####################################################
