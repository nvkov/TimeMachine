rm(list=ls())
set.seed(1)
#load("apa_paper_1.RData")
#### Meta-parameters ####################################
#project_directory <- "D:/HU/Applied Predictive Analytics/"
project_directory <- "C:/Users/Marta/Desktop/HU offline/Applied Predictive Analytics/"


setwd(project_directory)

load(paste(project_directory,"data_2_7_full.RDa", sep=""))

ls()

#install.packages("data.table")
#### Load libraries ######################################
library(dplyr)
library(ggplot2)
library(iterators)
library(data.table)
library(vcd)


####################################################################################
df$day <- weekdays(as.Date(df$orderDate))
df$month <- as.numeric(format(df$orderDate, "%m"))
df$mday <- as.numeric(format(df$orderDate, "%d"))

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
                                             

#head(df$cumsumOrder)
####################################################################################

df <- df[df$special_group=="",]
df <- df[!is.na(returnQuantity)]
  
df<- df[,day:=weekdays(as.Date(orderDate)),]
df<- df[,wday:=as.POSIXlt(orderDate)$wday,]
# (0-6 starting on Sunday)

df<- df[,weekday:=ifelse(wday==0||wday==6,0,1),]
df<- df[,weekend:=ifelse(wday==0||wday==6,1,0),]
df<- df[,sunday:=ifelse(wday==0,1,0),]


df<- df[,month:=as.numeric(format(orderDate, "%m")),]
df<- df[,mday:=as.numeric(format(orderDate, "%d")),]

df<- df[,sum_quant:=sum(quantity),by=.(orderID)]
df<- df[,sum_ret_quant:=sum(returnQuantity),by=.(orderID)]

df<- df[,ret_ratio_order:=sum_ret_quant/sum_quant,]

df$orderIDret <- ifelse(df$sum_ret_quant>0,df$orderID,NA)
df<- df[,cumsumOrderRet:=dense_rank(orderIDret),by=.(customerID)]

df<- df[,max_cumsumOrderRet:=max(cumsumOrderRet),by=.(customerID)]
df<- df[,max_cumsumOrder:=max(cumsumOrder),by=.(customerID)]



df<- df[,revenue_final:= revenue - price*returnQuantity,]
df<- df[,revenue_return:= price*returnQuantity,]


df <- df %>% arrange(customerID, orderID)
df<- df[,cumsum_rev_fin_lag:=cumsum(c(0, head(revenue_final, -1))),by=.(customerID)]
df<- df[,cumsum_revenue_lag:=cumsum(c(0, head(revenue, -1))),by=.(customerID)]
df<- df[,cumsum_revenue:=cumsum(revenue),by=.(customerID)]
df<- df[,cumsum_rev_fin:=cumsum(revenue_final),by=.(customerID)]

df<- df[,cumsum_revenue_return:=cumsum(revenue_return),by=.(customerID)]
df<- df[,cumsum_revenue_return_lag:=cumsum(c(0, head(revenue_return, -1))),by=.(customerID)]



#label<- label[,max_lab_cumsumOrderRet:=max(cumsumOrderRet),by=.(customerID)]
#label<- label[,max_lab_cumsumOrder:=max(cumsumOrder),by=.(customerID)]

#df_all <- df
#df <- label

group_order <- group_by(df, orderID)
 
order_level <- summarise(group_order,
		customerID=max(customerID), 
		orderDate=max(orderDate), 
		day=max(day),
		wday=max(wday),
		month=max(month),
		mday=max(mday),
		weekday=max(weekday),
		weekend=max(weekend),
		sunday=max(sunday),
		cumsumOrder=max(cumsumOrder),
		cumsumOrderRet=max(cumsumOrderRet),
		sum_quant=max(sum_quant),
		sum_ret_quant=max(sum_ret_quant),
		ret_ratio_order=max(ret_ratio_order),
		revenue=sum(revenue),
		revenue_final=sum(revenue_final)
		)

order_level2 <- order_level

order_level2$cumsumOrder_lag <- ifelse(order_level2$cumsumOrder >1,  order_level2$cumsumOrder-1, NA) 
order_level2$cumsumOrder_org <- order_level2$cumsumOrder
order_level2$cumsumOrder   <- order_level2$cumsumOrder_lag

order_level_joined <-left_join(order_level, order_level2, by = c("cumsumOrder" = "cumsumOrder", "customerID" = "customerID"))


order_level_joined_inner <- order_level_joined[!is.na(order_level_joined$cumsumOrder_org),]


order_level_joined_inner$diff_forward <- order_level_joined_inner$orderDate.y - order_level_joined_inner$orderDate.x


#head(order_level_joined)
#head(order_level_joined_inner)
#head(order_level_joined_inner$diff_forward)
#typeof(order_level_joined_inner$diff_forward)

#table(order_level_joined_inner$diff_forward, ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0))

#plot(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0) ~floor(order_level_joined_inner$diff_forward/5) )

#order_level_joined_inner$diff_forward[order_level_joined_inner$diff_forward<0]



#Delete 3 orders with not chronological IDs
order_level_joined_inner <- order_level_joined_inner[!(order_level_joined_inner$diff_forward<0),]

order_level_joined_inner$days_between_orders_10 <- ifelse(order_level_joined_inner$diff_forward>10, "0_or_>10_days", ifelse(order_level_joined_inner$diff_forward==0,"0_or_>10_days","1_to_10_days"))

order_level_joined_inner$days_between_orders_7 <- ifelse(order_level_joined_inner$diff_forward>7, "0_or_>7_days", ifelse(order_level_joined_inner$diff_forward==0,"0_or_>7_days","1_to_7_days"))

order_level_joined_inner$return_indicator_forward <- ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)

order_level_joined_inner$return_indicator_backwards <- ifelse( order_level_joined_inner$sum_ret_quant.x>0,1,0)


#Mosaicplot forward - 30 days
mosaic(
 ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward>=30, 30, ceiling(order_level_joined_inner$diff_forward/1)*1 ))

 
#Mosaicplot forward - 10 days cutoff binarz
mosaic(main = "Mosaicplot forward -  10 days cutoff",
data = order_level_joined_inner,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
return_indicator_forward
~days_between_orders_10
)


#Mosaicplot backwards 20 days cutoff
mosaic(main = "Mosaicplot forward - 10  10 days cutoff",
data = order_level_joined_inner,
return_indicator_backwards~
ifelse(diff_forward>=20, 20, ceiling(diff_forward/1)*1 ))


#Mosaicplot backwards - 10 days cutoff binarz
mosaic(main = "Mosaicplot backwards -  10 days cutoff",
data = order_level_joined_inner,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
return_indicator_backwards
~days_between_orders_10
)


#Mosaicplot backwards - 7 days cutoff binarz
mosaic(main = "Mosaicplot backwards -  7 days cutoff",
data = order_level_joined_inner,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
return_indicator_backwards
~days_between_orders_7
)




with(order_level_joined_inner, {	 
plot(diff_forward,return_indicator_backwards)
lines(lowess(diff_forward,return_indicator_backwards))
})




mosaic(
 ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ceiling(order_level_joined_inner$revenue.x/20))
 

 mosaic(
 ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~(ceiling(order_level_joined_inner$revenue_final.x/20)))
 
 mosaic(
 ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~(order_level_joined_inner$sum_quant.x))

 
 

nrow(order_level_joined_inner)
nrow(df)


#############################

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0.5,1,0)~ifelse(order_level_joined_inner$diff_forward>60, 13*5, ceiling(order_level_joined_inner$diff_forward/5)*5 ))

#library("vcd")
mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~floor(order_level_joined_inner$diff_forward/10) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward<30, 1, 2) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~floor(order_level_joined_inner$diff_forward/10) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward<30, 1, 2) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward>60, 13*5, ceiling(order_level_joined_inner$diff_forward/5)*5 ))

mosaic(ifelse( order_level_joined_inner$ret_ratio_order.y>0.5,1,0)~ifelse(order_level_joined_inner$diff_forward>60, 13*5, ceiling(order_level_joined_inner$diff_forward/5)*5 ))

mosaic(ifelse( order_level_joined_inner$ret_ratio_order.y>0.25,1,0)~ifelse(order_level_joined_inner$diff_forward>30, 31, order_level_joined_inner$diff_forward ))

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward>30, 31, order_level_joined_inner$diff_forward ))


mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~order_level_joined_inner$day.x)

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~order_level_joined_inner$day.y)

#head(order_level)
#############################

#order_level<- order_level[,wday:=as.POSIXlt(orderDate)$wday,]
# (0-6 starting on Sunday)
mosaic(ifelse( order_level$sum_ret_quant>0,1,0)~order_level$wday)

table(ifelse( order_level$sum_ret_quant>0,1,0),order_level$wday)

barplot(table(ifelse( order_level$sum_ret_quant>0,1,0),order_level$wday))


mosaic(ifelse( order_level$sum_ret_quant>0,1,0)~order_level$mday)

mosaic(ifelse( order_level$sum_ret_quant>0,1,0)~order_level$month)

table(ifelse( order_level$sum_ret_quant>0,1,0),order_level$month)

barplot(table(ifelse( order_level$sum_ret_quant>0,1,0),order_level$mday))


dev.off()

#head(order_level_joined_inner)

#Chi square 

group_customer_inner <- group_by(order_level_joined_inner, customerID)
 
customer_level_inner <- summarise(group_customer_inner,
		customerID, 
		orderDateMin=min(orderDate.y),
		orderDateMax=max(orderDate.y),
		diff_forward_min=min(diff_forward),
		diff_forward_max=max(diff_forward),
		diff_forward_avg= mean(diff_forward),
		diff_forward_median= median(diff_forward),
		sum_ret_quant = sum(sum_ret_quant.y),
		sum_quant = sum(sum_quant.y),
		ret_ratio_customer= sum(sum_ret_quant.y)/sum(sum_quant.y),
		cnt_order = sum(1),
		cnt_weekdays = sum(weekday.y),
		cnt_weekends =  sum(weekend.y),
		cnt_sunday =  sum(sunday.y)
		)

#customer_level_inner <-customer_level
hist(as.numeric(customer_level_inner$diff_forward_avg))



mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.25,1,0)~ceiling(customer_level_inner$diff_forward_avg/20) )


# diff_avg  - return_ratio>0.25
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.25,1,0)~ifelse(customer_level_inner$diff_forward_avg>=180, 180, ceiling(customer_level_inner$diff_forward_avg/5)*5 ) )

# diff_avg  - return_ratio>0.5
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.5,1,0)~ifelse(customer_level_inner$diff_forward_avg>=180, 180, ceiling(customer_level_inner$diff_forward_avg/5)*5 ) )

# diff_avg  - return_ratio>0.1
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.1,1,0)~ifelse(customer_level_inner$diff_forward_avg>=180, 180, ceiling(customer_level_inner$diff_forward_avg/5)*5 ) )

# diff_avg  - any return 
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0,1,0)~ifelse(customer_level_inner$diff_forward_avg>=180, 180, ceiling(customer_level_inner$diff_forward_avg/5)*5 ) )




# diff_min  - any return
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0,1,0)~ifelse(customer_level_inner$diff_forward_min>=180, 180, ceiling(customer_level_inner$diff_forward_min/5)*5 ) )

# diff_min  - return_ratio>0.25
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.25,1,0)~ifelse(customer_level_inner$diff_forward_min>=180, 180, ceiling(customer_level_inner$diff_forward_min/5)*5 ) )

# diff_min  - return_ratio>0.5
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.5,1,0)~ifelse(customer_level_inner$diff_forward_min>=180, 180, ceiling(customer_level_inner$diff_forward_min/5)*5 ) )





# diff_max  - any return
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0,1,0)~ifelse(customer_level_inner$diff_forward_max>=180, 180, ceiling(customer_level_inner$diff_forward_max/5)*5 ) )

# diff_max  - return_ratio>0.25
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.25,1,0)~ifelse(customer_level_inner$diff_forward_max>=180, 180, ceiling(customer_level_inner$diff_forward_max/5)*5 ) )

# diff_max  - return_ratio>0.5
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.5,1,0)~ifelse(customer_level_inner$diff_forward_max>=180, 180, ceiling(customer_level_inner$diff_forward_max/5)*5 ) )






# diff_median  - any return
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0,1,0)~ifelse(customer_level_inner$diff_forward_median>=180, 180, ceiling(customer_level_inner$diff_forward_median/5)*5 ) )

# diff_median  - return_ratio>0.25
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.25,1,0)~ifelse(customer_level_inner$diff_forward_median>=180, 180, ceiling(customer_level_inner$diff_forward_median/5)*5 ) )

# diff_median  - return_ratio>0.5
mosaic(ifelse( customer_level_inner$ret_ratio_customer>0.5,1,0)~ifelse(customer_level_inner$diff_forward_median>=180, 180, ceiling(customer_level_inner$diff_forward_median/5)*5 ) )






df$cumsum_rev_fin_lag_2500 <- ifelse(df$cumsum_rev_fin_lag>=2500, 2500,(ceiling(df$cumsum_rev_fin_lag/100)*100))

df$cumsum_rev_fin_lag_2500_25 <- ifelse(df$cumsum_rev_fin_lag>=1000, 1000,(ceiling(df$cumsum_rev_fin_lag/50)*50))




plot(factor(df$returnBin)~factor(df$cumsum_rev_fin_lag_2500_25))


plot(factor(df$returnBin)~factor(ifelse(df$cumsum_revenue_return_lag>=500, 500,(ceiling(df$cumsum_revenue_return_lag/20)*20))))



tbl <- table((ceiling(df$cumsum_rev_fin_lag/100)*100),df$returnBin)

tbl1 <- table(df$returnBin,
ifelse(df$cumsum_rev_fin_lag>=2500, 2500,
(ceiling(df$cumsum_rev_fin_lag/500)*500))
)



mosaic(ifelse(df$returnQuantity>0,1,0)~df$cumsum_rev_fin_lag_2500)

barplot(table(ifelse(df$returnQuantity>0,1,0),df$cumsum_rev_fin_lag_2500))
tbl <- table(df$cumsum_rev_fin_lag_2500,ifelse(df$returnQuantity>0,1,0))
 
tbl <- table(ifelse(df$returnQuantity>0,1,0),df$cumsum_rev_fin_lag_2500)

tbl_25 <- table(ifelse(df$returnQuantity>0,1,0),df$cumsum_rev_fin_lag_2500_25)

update.packages("ggplot2")
library(ggplot2)
library("plotly")
plot_ly(df, x=cumsum_rev_fin_lag_2500_25, y=returnBin)

ggplot(data=df, aes(x=cumsum_rev_fin_lag_2500_25, y=returnBin))+geom_point(size=2, alpha=0.4)+
  stat_smooth(method="loess", colour="blue", size=1.5)+
  xlab("Frequency")+
  ylab("Probability of Detection")+
  theme_bw()

  
  # Daily return/quatity ration
with(suma_date, {
  plot(orderDate, sum_return/sum_quant, type="l",xaxt = "n")
  axis(side = 1, at = orderDate, labels = orderDate)
  lines(ksmooth(orderDate, sum_return/sum_quant, "normal", bandwidth = 50), col = 3)
})
  
names(tbl)
barplot(tbl)

barplot(tbl_25)
 
barplot(prop.table(tbl,2))

barplot(prop.table(tbl_25,2))

cumsum_revenue_return


prop.table(tbl,2)
assocplot(tbl)


typeof(head(df$cumsum_rev_fin_lag_2500))
is.na(df$cumsum_rev_fin_lag_2500)
#head(df)

#rm("order_level_joined", "label", "df_all", "order_level2", "cumsumOrderdf")

#ls()