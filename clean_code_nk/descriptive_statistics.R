# Desctripitve stats for the Data section of the paper

rm(list=ls())

# Find one time customers:
library("data.table")

load("C:/Users/Nk/Documents/Uni/APA/data_2_10.RDa")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code_nk/pred_functions_updated.R")

df$weekday<- weekdays(df$orderDate)
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

######################################################
#Add return ratios:
# creating features and imputing values
data    <- add_returns(df, class)
df <- data$train
class <- data$test
df <- df[, !colnames(df) %in% "new"]
rm(data)


#################################################################
#Plot to check for shift due to growth - week level:
relevantCols<- c("return_per_week", "week", "year")
temp<-df[, relevantCols]
temp<- temp[!duplicated(temp),]
plot(temp$week, temp$return_per_week, col=ifelse(temp$year==2015, 1, 2))

#T-test for difference:
t.test(temp$return_per_week[temp$year==2015], temp$return_per_week[temp$year==2014])

#Plot to check for shift due to growth - month level:
relevantCols<- c("return_per_month", "month", "year")
temp<-df[, relevantCols]
temp<- temp[!duplicated(temp),]
plot(temp$month, temp$return_per_month, col=ifelse(temp$year==2015, 1, 2))

#T-test for difference:
t.test(temp$return_per_month[temp$year==2015], temp$return_per_month[temp$year==2014])

#Do t.test for the difference between yearly returns and 0!

#########################################################
#Plot customer number for customer growth
relevantCols<- c("customerID", "orderDate", "year")
temp<-df[, relevantCols]
temp<- as.data.table(temp)
temp<- temp[!duplicated(temp),]
temp<- temp[, .(customers=.N),by=.(orderDate)]
plot(temp$orderDate, temp$customers)

#########################################################
#Plot for weekly seasonality:
relevantCols<- c("return_per_weekday", "weekday")
temp<-df[, relevantCols]
temp<- temp[!duplicated(temp),]
temp<-temp[order(temp$return_per_weekday),]
plot(as.factor(temp$weekday), temp$return_per_weekday)

########################################################
#Plot for weekly seasonality (yearly comparison:
relevantCols<- c("return_per_weekday_year", "weekday", "year")
temp<-df[, relevantCols]
temp<- temp[!duplicated(temp),]
temp<-temp[order(temp$return_per_weekday_year),]
plot(as.factor(temp$weekday), temp$return_per_weekday_year, col=ifelse(temp$year==2014, 1, 2))

########################################################

# See which products are present for labeled and unlabeled data for the same period:
# Look for common products:
relevantCols<- c("articleID", "week")
temp<- df[unique(class$articleID) %in% unique(df$articleID),relevantCols]
length(unique(temp$articleID))

temp<-temp[temp$week>=40,]

temp<-data.table(temp[!duplicated(temp),])
table(temp$week)

#Check which products where present for the whole period
temp<-temp[,.(number_of_weeks_present=.N), by=.(articleID)]
table(temp$number_of_weeks_present)


# Look for common customers:
relevantCols<- c("customerID", "week")
temp<- df[unique(class$customerID) %in% unique(df$customerID),relevantCols]
length(unique(temp$customerID))

temp<-temp[temp$week>=40,]

temp<-data.table(temp[!duplicated(temp),])
table(temp$week)

#Check which products where present for the whole period
temp<-temp[,.(number_of_weeks_present=.N), by=.(articleID)]
table(temp$number_of_weeks_present)


#############################################################
