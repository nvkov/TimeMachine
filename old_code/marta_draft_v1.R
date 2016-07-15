rm(list=ls())
set.seed(1)

#### Meta-parameters ####################################
project_directory <-"C:/Users/Marta/Desktop/HU offline/Applied Predictive Analytics/"
#C:\Users\Marta\Desktop\HU offline\Applied Predictive Analytics

setwd(project_directory)

load(paste(project_directory,"data_2_7_full.RDa", sep=""))

ls()


#### Load libraries ######################################
library(dplyr)
library(ggplot2)
library(vcd)
library(iterators)
library(data.table)

#install.packages("data.table")




head(df)

df$day <- weekdays(as.Date(df$orderDate))
df$month <- as.numeric(format(df$orderDate, "%m"))
df$mday <- as.numeric(format(df$orderDate, "%d"))



head(df$day)

table(df$mday)

df <- df %>% group_by(orderID) %>% 
  arrange(orderID) %>% 
  mutate(sum_quant = sum(quantity), 
         sum_ret_quant = sum(returnQuantity))

df <- df %>% group_by(orderID) %>% 
  arrange(orderID) %>% 
  mutate(ret_ratio_customer = sum_ret_quant/sum_quant)






tapply(label$orderID, label$day, summary)

df$wday <- as.POSIXlt(df$orderDate)$wday

head(df$wday)

label <- df[!is.na(returnQuantity),]

group_day <- group_by(label, wday)
suma_day <- summarise(group_day,
                  sum_return=sum(returnQuantity), 
                  sum_quant=sum(quantity),
                  cnt_orders = length(unique(orderID))
                  )

plot(sum_quant~wday,data = suma_day)

plot(cnt_orders~wday,data = suma_day)

                  
tapply(label$ret_ratio_customer, label$wday, summary)


boxplot(ret_ratio_customer ~ wday, data = label, col = "lightgray")
means <- tapply(label$ret_ratio_customer,label$wday,mean)
points(means,col="red",pch=18)



df <- df %>% group_by(customerID)  %>% arrange(orderID)   %>%  mutate(cumsumOrder = dense_rank(orderID))
df <- df %>% group_by(customerID) %>% mutate(totOrder = max(cumsumOrder))

df <- df %>% group_by(customerID)  %>% arrange(orderID) %>%  mutate(cumsumOrderRet = dense_rank(ifelse(returnQuantity>0,orderID,NA))
df <- df %>% group_by(customerID) %>% mutate(totOrderRet = max(dense_rank(ifelse(returnQuantity>0,orderID,NA)))

                                             
head(df$cumsumOrder)
