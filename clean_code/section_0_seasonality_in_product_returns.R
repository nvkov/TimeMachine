rm(list=ls())

#Load libraries:
library("data.table")
library("zoo")

#Load data:
load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

df<- df[!is.na(df$returnQuantity)]

#Look for commerce trends:
df<- df[, customers_per_date:=length(unique(customerID)), by=.(orderDate)]

#Look at number of returns for different dynamics: 

#returns per day
df$day<- weekdays(df$orderDate)
df<- df[, returns_ratio_per_day:=sum(returnQuantity)/sum(quantity), by=.(orderDate)]
df<- df[,productGroup_return_ratio_per_day:=sum(returnQuantity), by=.(orderDate, productGroup)]
df<- df[,article_return_ratio_per_day:=sum(returnQuantity)/sum(quantity), by=.(orderDate, articleID)]
df<- df[,sizeCode_return_ratio_per_day:=sum(returnQuantity)/sum(quantity), by=.(orderDate, sizeCode)]

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

#############################################
#Keep only return ratios for seasonality check:
seasonality_vars_productGroup<- c("productGroup", "productGroup_return_ratio_per_week",
                                  "week")

seasonality_check_productGroup<- df[,seasonality_vars_productGroup, with=F]
seasonality_check_productGroup<- seasonality_check_productGroup[!duplicated(seasonality_check_productGroup),]

#############################################
#Returns per month:
i<- 17
for(i in unique(df$productGroup)){
  jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/", i, ".jpg"))  
  
  plot(seasonality_check_productGroup$week[seasonality_check_productGroup$productGroup==i], 
              seasonality_check_productGroup$productGroup_return_ratio_per_week[seasonality_check_productGroup$productGroup==i],
              xlab=paste0("productGroup = ", i), ylab="Return ratio")
  my.count=seq(from=1, to=52, by=1)
  fit6 <- loess(seasonality_check_productGroup$productGroup_return_ratio_per_week[seasonality_check_productGroup$productGroup==i] ~ seasonality_check_productGroup$week[seasonality_check_productGroup$productGroup==i], span=0.6)
  lines(seasonality_check_productGroup$productGroup_return_ratio_per_week[seasonality_check_productGroup$productGroup==i], predict(fit6), col = "blue")
  dev.off()
}

jpeg(paste0("C:/Users/Nk/Documents/Uni/APA/", i, ".jpg"))  
hi<-acf(seasonality_check_productGroup$productGroup_return_ratio_per_week[seasonality_check_productGroup$productGroup==17])
dev.off()

seasonality_check_productGroup<- seasonality_check_productGroup[order(seasonality_check_productGroup$week)]
hi<- acf(seasonality_check_productGroup$productGroup_return_ratio_per_week[seasonality_check_productGroup$productGroup==17])


####################################
library(forecast)
# Automated forecasting using an exponential model
fit <- ets(seasonality_check_productGroup[order(seasonality_check_productGroup$week)])

# Automated forecasting using an ARIMA model
fit <- auto.arima(seasonality_check_productGroup$productGroup_return_ratio_per_week[seasonality_check_productGroup$productGroup==17])


