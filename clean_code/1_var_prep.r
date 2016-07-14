###############
#####CLEAR CODE
###############
rm(list=ls())
set.seed(1)

# install.packages("vcd")
library(dplyr)
library(ggplot2)
library(iterators)
library(data.table)
library(vcd)
library("colorspace")



#project_directory <- "D:/HU/Applied Predictive Analytics/APA-report/"
project_directory <- "D:/Dropbox/Dropbox/HU STUDIES/Applied Predictive Analytics/APA - report/CODE/"
setwd(project_directory)



### Load df dataframe (CLASS and LABEL)
load("data_2_7_full.RDa")
#ls()



#NEW VARIABLES 

# DELETE CLASS DATASET  (df.class)
df.class <- df[is.na(returnQuantity)]
df <- df[!is.na(returnQuantity),]
save(df.class, file="df_class_v1.RDa")
rm("df.class")

# DELETE SPECIAL GROUP OBS.  (df.special)
df.special <- df[df$special_group!="",]
df <- df[df$special_group=="",]
save(df.special, file="df_special_v1.RDa")
rm("df.special")

# day: Day of the week as text - ORDER LVL
df<- df[,day:=weekdays(as.Date(orderDate)),]
 
# wday: Day of the week as number (0-6 starting on Sunday)  - ORDER LVL
df<- df[,wday:=as.POSIXlt(orderDate)$wday,]

# weekday: Weekday indicator  - ORDER LVL
df<- df[,weekday:=ifelse(wday==0|wday==6,0,1),]
# weekend: Weekend indicator  - ORDER LVL
df<- df[,weekend:=ifelse(wday==0|wday==6,1,0),]
# sunday: Sunday indicator  - ORDER LVL
df<- df[,sunday:=ifelse(wday==0,1,0),]


# month: numer of month - ORDER LVL
df<- df[,month:=as.numeric(format(orderDate, "%m")),]
df<- df[,mday:=as.numeric(format(orderDate, "%d")),]

# sum_quant: Sum of Quantity - ORDER LVL
df<- df[,sum_quant:=sum(quantity),by=.(orderID)]
# sum_ret_quant: Sum of Quantity - ORDER LVL
df<- df[,sum_ret_quant:=sum(returnQuantity),by=.(orderID)]
# ret_ratio_order: Sum of Quantity - ORDER LVL
df<- df[,ret_ratio_order:=sum_ret_quant/sum_quant,]

# orderIDret: nonempty orderID if there was any return in the order - ORDER LVL
df$orderIDret <- ifelse(df$sum_ret_quant>0,df$orderID,NA)
# cumsumOrderRet: rank od returned order - ORDER LVL
df<- df[,cumsumOrderRet:=dense_rank(orderIDret),by=.(customerID)]#

df<- df[,cumsumOrderRet:=ifelse(is.na(cumsumOrderRet), 0, cumsumOrderRet),]

# cumsumOrderRetAll: rank od returned order for all orders - ORDER LVL
df<- df[,cumsumOrderRetAll:=cummax(cumsumOrderRet),by=.(customerID)]

# max_cumsumOrderRet: maximum rank of returned order - CUSTOMER LVL
df<- df[,max_cumsumOrderRet:=max(cumsumOrderRet),by=.(customerID)]


# max_cumsumOrder: maximum rank of order - CUSTOMER LVL
df<- df[,max_cumsumOrder:=max(cumsumOrder),by=.(customerID)]

# revenue_final: revenue for non-returner items - ARTICLE LVL
df<- df[,revenue_final:= revenue - price*returnQuantity,]
# revenue_return: revenue for non-returner items - ARTICLE LVL
df<- df[,revenue_return:= price*returnQuantity,]


# revenue_order: revenue for all ordered items - ORDER LVL
df<- df[,revenue_order:=sum(revenue),by=.(orderID)]

# revenue_final_order: revenue for non-returner items - ORDER LVL
df<- df[,revenue_final_order:=sum(revenue_final),by=.(orderID)]

# revenue_return_order: revenue for returner items - ORDER LVL
df<- df[,revenue_return_order:=sum(revenue_return),by=.(orderID)]


# cust_revenue_final: revenue for non-returner items - CUSTOMER LVL
df<- df[,cust_revenue_final:=sum(revenue_final),by=.(customerID)]

# cust_revenue_final: revenue for non-returner items - CUSTOMER LVL
df<- df[,trans_ind:=1,]

########################################################################
############ ORDER_LEVEL: DATA.TABLE ON ORDER_LVL
order_level<- df[,.(

	sum_quant=max(sum_quant),
	sum_ret_quant=max(sum_ret_quant),
	ret_ratio_order=max(ret_ratio_order),
	returnBin_order=max(as.numeric(ifelse(sum_ret_quant>0,1,0))),
	day= max(day),
	wday=max(wday),
	weekday=max(weekday),
	weekend=max(weekend),
	sunday=max(sunday),
	mday=max(mday),
	month=max(month),
	
	revenue_order=max(revenue_order),	
	revenue_final_order=max(revenue_final_order),
	revenue_return_order=max(revenue_return_order),
	
	cumsumOrder=max(cumsumOrder),
	cumsumOrderRet=max(cumsumOrderRet),
	cumsumOrderRetAll=max(cumsumOrderRetAll),
	order_ind = 1,
	order_trans=sum(trans_ind)
	),
	by=.(customerID, orderID, orderDate)]

# ORDER_LEVEL - sort ascending by orderID
#order_level <- order_level %>% arrange(customerID, orderID)
order_level<-order_level[order(customerID, orderID),]


# cumsum_revenue: Cumulated sum of revenue_order value - ORDER LVL
order_level<- order_level[,cumsum_revenue:=cumsum(revenue_order),by=.(customerID)]

# cumsum_rev_fin: Cumulated sum of revenue_final_order value - ORDER LVL
order_level<- order_level[,cumsum_rev_fin:=cumsum(revenue_final_order),by=.(customerID)]

# cumsum_revenue_return: Cumulated sum of revenue_return_order value - ORDER LVL
order_level<- order_level[,cumsum_revenue_return:=cumsum(revenue_return_order),by=.(customerID)]

# cumsum_revenue_lag: Lag(1) cumulated sum of revenue_order value - ORDER LVL
order_level<- order_level[,cumsum_revenue_lag:=cumsum(c(0, head(revenue_order, -1))),by=.(customerID)]

# cumsum_rev_fin_lag: Lag(1) period cumulated sum of revenue_final_order value - ORDER LVL
order_level<- order_level[,cumsum_rev_fin_lag:=cumsum(c(0, head(revenue_final_order, -1))),by=.(customerID)]

# cumsum_revenue_return: Cumulated sum of revenue_return_order value - ORDER LVL
order_level<- order_level[,cumsum_revenue_return_lag:=cumsum(c(0, head(revenue_return_order, -1))),by=.(customerID)]




# cumsum_quant: Cumulated quantity - ORDER LVL
order_level<- order_level[,cumsum_quant:=cumsum(sum_quant),by=.(customerID)]

# cumsum_quant_lag: Lag(1) Cumulated quantity - ORDER LVL
order_level<- order_level[,cumsum_quant_lag:=cumsum(c(0, head(sum_quant, -1))),by=.(customerID)]


# cumsum_ret_quant: Cumulated return quantity - ORDER LVL
order_level<- order_level[,cumsum_ret_quant:=cumsum(sum_ret_quant),by=.(customerID)]

# cumsum_ret_quant_lag: Lag(1) Cumulated quantity - ORDER LVL
order_level<- order_level[,cumsum_ret_quant_lag:=cumsum(c(0, head(sum_ret_quant, -1))),by=.(customerID)]

#############
# cum_ret_ratio
order_level<- order_level[,cum_ret_ratio:=cumsum_ret_quant/cumsum_quant,by=.(customerID)]

# cum_ret_ratio_lag
order_level<- order_level[,cum_ret_ratio_lag:=cumsum_ret_quant_lag/cumsum_quant_lag,by=.(customerID)]




########################################################################
############  Calculate invterval between orders

order_level2 <- order_level

#delete first orders per customer
order_level2<- order_level2[order_level2$cumsumOrder>1]

# cumsumOrder_lag: Lagged(1) rank of Order
order_level2<- order_level2[,cumsumOrder_lag:=(cumsumOrder-1),by=.(customerID)]

# cumsumOrder_org: original rank of Order
order_level2$cumsumOrder_org <- order_level2$cumsumOrder

# cumsumOrder: lagged rank of Order
order_level2$cumsumOrder   <- order_level2$cumsumOrder_lag

#INNER JOIN orders to the next order (drop single orders and last orders)
order_level_joined <-left_join(order_level, order_level2, by = c("cumsumOrder" = "cumsumOrder", "customerID" = "customerID"))
order_level_joined <- as.data.table(order_level_joined)

order_level_joined_inner <- order_level_joined[!is.na(order_level_joined$cumsumOrder_org),]

rm("order_level2", "order_level_joined")

# order_level_joined_inner$diff_forward <- order_level_joined_inner$orderDate.y - order_level_joined_inner$orderDate.x

# diff_forward: number of days until the next order  - ORDER LVL
order_level_joined_inner<- order_level_joined_inner[,diff_forward:= as.numeric(orderDate.y - orderDate.x),]

#Delete 3 orders with not chronological IDs
order_level_joined_inner <- order_level_joined_inner[!(order_level_joined_inner$diff_forward<0),]



########################################################################
############  Customer level - for intervals - INNER JOINED
customer_level_inner<- order_level_joined_inner[,.(
		diff_min=min(diff_forward),
		diff_max=max(diff_forward),
		diff_avg= mean(diff_forward),
		diff_median= median(diff_forward)
	),
	by=.(customerID)]

	
	
############  Customer level - all customers
customer_level<- order_level[,.(	
		sum_quant_cust=sum(sum_quant),
		sum_ret_quant_cust=sum(sum_ret_quant),
		ret_ratio_cust=sum(sum_ret_quant)/sum(sum_quant),
		ret_ratio_order_cust=sum(returnBin_order)/sum(order_ind),
		ret_cust=sum(returnBin_order),
		cnt_orders=sum(order_ind),
		cnt_trans=sum(order_trans),
		
		cnt_weekday=sum(weekday),
		cnt_weekend=sum(weekend),
		cnt_sunday=sum(sunday),
		
		revenue_cust=sum(revenue_order),
		revenue_final_cust=sum(revenue_final_order),
		revenue_return_cust=sum(revenue_return_order),
		
		minOrderDate=min(orderDate),
		maxOrderDate=max(orderDate),
		duration=as.numeric(max(orderDate)-min(orderDate))
	),
	by=.(customerID)]


# order_freq: average how many orders within 30 days - CUSTOMER LVL
customer_level<- customer_level[,order_freq:=(cnt_orders*30/(duration)),]

# order_freq: average how much revenue within 30 days - CUSTOMER LVL
customer_level<- customer_level[,rev_freq:=(revenue_cust*30/(duration)),]

# article_freq: average how much quantity within 30 days - CUSTOMER LVL
customer_level<- customer_level[,article_freq:=(sum_quant_cust*30/(duration)),]

# trans_freq: average how many transactions (distinct product/sizes/colours) within 30 days - CUSTOMER LVL
customer_level<- customer_level[,trans_freq:=(cnt_trans*30/(duration)),]


########################################################################	
############  1. Join together the Customer Level Variables

customer_level <-  merge(x = customer_level, y = customer_level_inner, by = "customerID", all.x = TRUE)

save(customer_level, file="customer_level_v1.RDa")
rm("customer_level_inner")

# head(customer_level)
########################################################################	
############  2. Join the CUSTOMER LVL variables to DF

df <-  merge(x = df, y = customer_level, by = "customerID", all.x = TRUE)

########################################################################	
############  3. Join together the ORDER LVL Variables
############  3.1. Join the variables about NEXT order

order_level_joined_inner$orderID  <- order_level_joined_inner$orderID.x

myvars <- c("orderID","diff_forward","orderDate.y","wday.y","month.y","mday.y","weekday.y","weekend.y","sunday.y","sum_quant.y","sum_ret_quant.y","ret_ratio_order.y","revenue_order.y","revenue_final_order.y", "revenue_return_order.y")

ord_lev <- order_level_joined_inner[,myvars,with=F]
order_level <-  merge(x = order_level, y = ord_lev, by = "orderID", all.x = TRUE)


############  3.2. Join the variables about PREVIOUS order

order_level_joined_inner$orderID  <- order_level_joined_inner$orderID.y

myvars1 <- c("orderID","diff_forward","orderDate.x","wday.x","month.x","mday.x","weekday.x","weekend.x","sunday.x","sum_quant.x","sum_ret_quant.x","ret_ratio_order.x","revenue_order.x","revenue_final_order.x", "revenue_return_order.x")

ord_lev1 <- order_level_joined_inner[,myvars1,with=F]
order_level <-  merge(x = order_level, y = ord_lev1, by = "orderID", all.x = TRUE)

rm("ord_lev", "myvars")
rm("ord_lev1", "myvars1")
rm("order_level_joined_inner")

names(order_level)[names(order_level)=="diff_forward.x"] <- "diff_forward"
names(order_level)[names(order_level)=="diff_forward.y"] <- "diff_backwards"

save(df, file="order_level_v1.RDa")

########################################################################	
############  4. Join the ORDER LVL Variables to DF


myvars2 <- c("orderID", "order_trans", "cumsum_revenue", "cumsum_rev_fin", "cumsum_revenue_return", "cumsum_revenue_lag", "cumsum_rev_fin_lag", "cumsum_revenue_return_lag",  "cumsum_quant",  "cumsum_quant_lag", "cumsum_ret_quant",  "cumsum_ret_quant_lag", "cum_ret_ratio", "cum_ret_ratio_lag", "diff_forward", "orderDate.y", "wday.y", "month.y", "mday.y", "weekday.y", "weekend.y", "sunday.y", "sum_quant.y", "sum_ret_quant.y", "ret_ratio_order.y", "revenue_order.y", "revenue_final_order.y", "revenue_return_order.y", "diff_backwards", "orderDate.x", "wday.x", "month.x", "mday.x", "weekday.x", "weekend.x", "sunday.x", "sum_quant.x", "sum_ret_quant.x", "ret_ratio_order.x", "revenue_order.x", "revenue_final_order.x", "revenue_return_order.x")

ord_lev2  <- order_level[,myvars2,with=F]

df <-  merge(x = df, y = ord_lev2, by = "orderID", all.x = TRUE)
rm("ord_lev2", "myvars2")

df<-df[order(customerID, orderID, unique_ID),]


########################################################################


# customer_age:  time from the first order until the particular order - ORDER LVL
df<- df[,customer_age:=as.numeric(orderDate - minOrderDate),]


save(df, file="df_v1.RDa")

#load("df_v1.RDa")



#####################
#######Product Cumulative Return Ratio

#Check whether the Articles are unique per OrderID. NO, they are not! - therefore make CumulativeLag(1) Article Ret Ratio on Order Level.
# df$ind <-1
# df<- df[,repeting_articles:=cumsum(ind),by=.(articleID, orderID)]
# table(df$repeting_articles)


#######
order_art <- df[,.(
	quantity=sum(quantity),
	orderDate=max(orderDate),
	returnQuantity=sum(returnQuantity)	
	),
	by=.(articleID, orderID)]

# Sort table asc by article and date (optional:orderID)
order_art<-order_art[order(articleID, orderDate, orderID),]



#cumQuantArt - Cumulative sum of quantity of an Article - Article LVL
order_art<- order_art[,cumQuantArt:=cumsum(quantity),by=.(articleID)]
#cumQuantArtLag - Cumulative Lagged sum of quantity of an Article - Article LVL
order_art<- order_art[,cumQuantArtLag:=cumsum(c(0, head(quantity, -1))),by=.(articleID)]

#cumRetQuantArt - Cumulative sum of returnQuantity of an Article - Article LVL
order_art<- order_art[,cumRetQuantArt:=cumsum(returnQuantity),by=.(articleID)]
#cumQuantArtLag - Cumulative Lagged sum of quantity of an Article - Article LVL
order_art<- order_art[,cumRetQuantArtLag:=cumsum(c(0, head(returnQuantity, -1))),by=.(articleID)]


#cumArtRetRatio - Cumulative Return Ratio per Article
order_art<- order_art[,cumArtRetRatio:=cumRetQuantArt/cumQuantArt,by=.(articleID)]
#cumArtRetRatioLag - Cumulative Return Ratio Lag(1) per Article
order_art<- order_art[,cumArtRetRatioLag:=cumRetQuantArtLag/cumQuantArtLag,by=.(articleID)]


# ArticleMaturity
order_art<- order_art[,ArticleMinDate:=min(orderDate),by=.(articleID)]
order_art<- order_art[,ArticleMaturity:=as.numeric(orderDate - ArticleMinDate),by=.(articleID)]



############ 5. Join the order_art variables to DF


col1 <- c("orderID", "articleID", "cumQuantArt", "cumQuantArtLag", "cumRetQuantArt", "cumRetQuantArtLag", "cumArtRetRatio", "cumArtRetRatioLag", "ArticleMaturity", "ArticleMinDate" )

order_art  <- order_art[,col1,with=F]

df <-  merge(x = df, y = order_art, by =c("orderID", "articleID"), all.x = TRUE)

rm("order_art", "col1")

save(df, file="df_v1.RDa")

####################################

head(df[, c("orderID", "orderIDret", "cumsumOrderRetAll", "cumsumOrderRetAllSurv"), with = FALSE])

head(df[, c("orderID", "orderIDret", "cumsumOrderRet", "cumsumOrderRetT", "cumsumOrderRetAll", "cumsumOrderRetAllSurv"), with = FALSE])



#Max is 360, so use 1000 instead of NA
df<- df[,cumsumOrderRetT:=ifelse(cumsumOrderRet==0, 1000, cumsumOrderRet),]


df<-df[order(customerID, -orderID),]

df <- df[,cumsumOrderRetAllSurv:=cummin(cumsumOrderRetT),by=.(customerID)]
df[,cumsumOrderRetT:=NULL]

save(df, file="df_v1.RDa")




####################################




##General Var For Survival

#number_of_articles_kept: Cumulative, Lag(1) - ORDER LVL
df<- df[,number_of_articles_kept:=cumsum_quant_lag-cumsum_ret_quant_lag,]

#total_number_of_returns - CUSTOMER LVL
df<- df[,total_number_of_returns:=sum(returnQuantity), by=.(customerID)]

#Number of product groups per customer - CUSTOMER LVL
df<- df[,number_of_products_per_customer:=length(unique(productGroup)),by=.(customerID)]

#Number of products groups per order and customer - ORDER LVL
df<- df[,number_of_products_per_order:=length(unique(productGroup)),by=.(customerID, orderID)]

	# Number of unique products per order - ORDER LVL
	# df<- df[,number_of_unique_products_per_order:=length(unique(articleID)),by=.(customerID, orderID)]


# Number of products' colors per order - ORDER LVL
df<- df[,number_of_unique_colors_per_order:=length(unique(colorCode)),by=.(customerID, orderID)]

# Number of products' size per order - ORDER LVL
df<- df[,number_of_unique_sizes_per_order:=length(unique(sizeCode)),by=.(customerID, orderID)]


save(df, file="df_v2.RDa")




####################################
## Deal with NA values in new variables

sapply(df, function(x) {sum(is.na(x))})

lapply(dfs, class)



df<- df[,diff_min:=ifelse(is.na(diff_min), -10, diff_min),]
df<- df[,diff_max :=ifelse(is.na(diff_max ), -10, diff_max ),]
df<- df[,diff_avg :=ifelse(is.na(diff_avg ), -10, diff_avg ),]
df<- df[,diff_median:=ifelse(is.na(diff_median), -10, diff_median),]
df<- df[,cum_ret_ratio_lag:=ifelse(is.na(cum_ret_ratio_lag), -10, cum_ret_ratio_lag),]
df<- df[,diff_forward:=ifelse(is.na(diff_forward), -10, diff_forward),]
df<- df[,orderDate.y:=ifelse(is.na(orderDate.y), -10, orderDate.y),]
df<- df[,wday.y:=ifelse(is.na(wday.y), -10, wday.y),]
df<- df[,month.y:=ifelse(is.na(month.y), -10, month.y),]
df<- df[,mday.y:=ifelse(is.na(mday.y), -10, mday.y),]
df<- df[,weekday.y:=ifelse(is.na(weekday.y), -10, weekday.y),]
df<- df[,weekend.y:=ifelse(is.na(weekend.y), -10, weekend.y),]
df<- df[,sunday.y:=ifelse(is.na(sunday.y), -10, sunday.y),]
df<- df[,sum_quant.y:=ifelse(is.na(sum_quant.y), -10, sum_quant.y),]
df<- df[,sum_ret_quant.y:=ifelse(is.na(sum_ret_quant.y), -10, sum_ret_quant.y),]
df<- df[,ret_ratio_order.y:=ifelse(is.na(ret_ratio_order.y), -10, ret_ratio_order.y),]
df<- df[,revenue_order.y:=ifelse(is.na(revenue_order.y), -10, revenue_order.y),]
df<- df[,revenue_final_order.y:=ifelse(is.na(revenue_final_order.y), -10, revenue_final_order.y),]
df<- df[,revenue_return_order.y:=ifelse(is.na(revenue_return_order.y), -10, revenue_return_order.y),]
df<- df[,diff_backwards:=ifelse(is.na(diff_backwards), -10, diff_backwards),]
df<- df[,orderDate.x:=ifelse(is.na(orderDate.x), -10, orderDate.x),]
df<- df[,wday.x:=ifelse(is.na(wday.x), -10, wday.x),]
df<- df[,month.x:=ifelse(is.na(month.x), -10, month.x),]
df<- df[,mday.x:=ifelse(is.na(mday.x), -10, mday.x),]
df<- df[,weekday.x:=ifelse(is.na(weekday.x), -10, weekday.x),]
df<- df[,weekend.x:=ifelse(is.na(weekend.x), -10, weekend.x),]
df<- df[,sunday.x:=ifelse(is.na(sunday.x), -10, sunday.x),]
df<- df[,sum_quant.x:=ifelse(is.na(sum_quant.x), -10, sum_quant.x),]
df<- df[,sum_ret_quant.x:=ifelse(is.na(sum_ret_quant.x), -10, sum_ret_quant.x),]
df<- df[,ret_ratio_order.x:=ifelse(is.na(ret_ratio_order.x), -10, ret_ratio_order.x),]
df<- df[,revenue_order.x:=ifelse(is.na(revenue_order.x), -10, revenue_order.x),]
df<- df[,revenue_final_order.x:=ifelse(is.na(revenue_final_order.x), -10, revenue_final_order.x),]
df<- df[,revenue_return_order.x:=ifelse(is.na(revenue_return_order.x), -10, revenue_return_order.x),]
df<- df[,cumArtRetRatioLag:=ifelse(is.na(cumArtRetRatioLag), -10, cumArtRetRatioLag),]

save(df, file="df_v3.RDa")



####################################
### Time Until Survival Modififcation of Variable - ADJUST RESPECTIVELY the first row below

df$orderIDret <- ifelse(df$ret_ratio_order>0.5,df$orderID,NA)



# cumsumOrderRet: rank od returned order - ORDER LVL
df<- df[,cumsumOrderRet:=dense_rank(orderIDret),by=.(customerID)]#

df<- df[,cumsumOrderRet:=ifelse(is.na(cumsumOrderRet), 0, cumsumOrderRet),]

# cumsumOrderRetAll: rank od returned order for all orders - ORDER LVL
df<- df[,cumsumOrderRetAll:=cummax(cumsumOrderRet),by=.(customerID)]

# max_cumsumOrderRet: maximum rank od returned order - CUSTOMER LVL
df<- df[,max_cumsumOrderRet:=max(cumsumOrderRet),by=.(customerID)]



#Max is 360, so use 1000 instead of NA
df<- df[,cumsumOrderRetT:=ifelse(cumsumOrderRet==0, 1000, cumsumOrderRet),]


df<-df[order(customerID, -orderID),]

df <- df[,cumsumOrderRetAllSurv:=cummin(cumsumOrderRetT),by=.(customerID)]

df[,cumsumOrderRetT:=NULL]
df[,number_of_unique_products_per_order:=NULL]

save(df, file="df_v3.RDa")
