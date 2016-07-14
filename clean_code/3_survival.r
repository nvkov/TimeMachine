########################################################################
##### Mosaicplots: CUSTOMER_LEVEL

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


########## DATA

load("df_v3.RDa")


#Subset only for customers with longer buying behavior:
dfs<- df[df$max_cumsumOrder>5]
# is.data.frame(dfs)
rm(list= ls()[!(ls() %in% c('dfs'))])

head(dfs)


dfs<- dfs[,survID:=paste0(customerID,cumsumOrderRetAllSurv, sep="_"),]
dfs<- dfs[,survID:=as.factor(survID),]
dfs<- dfs[,survival:=ifelse(ret_ratio_order>0.5,1,0),]


############  Customer level - all customers
order_survival <- dfs[,.(	
		#voucherID=max(as.integer(voucherID)),
		#voucherAmount=max(voucherAmount),
		#deviceID=max(as.integer(deviceID)),
		#paymentMethod=max(paymentMethod),
		#yearQuarter=max(as.numeric(yearQuarter)),
		orders_per_customer=max(orders_per_customer),
		number_of_same_items_in_order=max(number_of_same_items_in_order),
		number_of_items_from_same_category=max(number_of_items_from_same_category),
		total_items_in_order=max(total_items_in_order),
		#luxury_item_in_product_group=max(luxury_item_in_product_group),
		#cheap_item_in_product_group=max(cheap_item_in_product_group),
		voucher_ratio=max(voucher_ratio),
		relative_deviation_price_mean_byCustomerID=max(relative_deviation_price_mean_byCustomerID),
		relative_deviation_price_mean_byOrderID=max(relative_deviation_price_mean_byOrderID),
		gratis_per_customer=max(gratis_per_customer),
		gratis_per_order=max(gratis_per_order),
		giftcard_per_customer=max(giftcard_per_customer),
		not_delivered_order=max(not_delivered_order),
		not_delivered_customer=max(not_delivered_customer),	
		free_delivery_order=max(free_delivery_order),
		free_delivery_customer=max(free_delivery_customer),
		day=max(day),
		wday=max(wday),
		day=max(day),
		weekday=max(weekday),
		weekend=max(weekend),
		sunday=max(sunday),
		month=max(month),
		mday=max(mday),
		sum_quant=max(sum_quant),
		sum_ret_quant=max(sum_ret_quant),
		ret_ratio_order=max(ret_ratio_order),
		orderIDret=max(orderIDret),
		cumsumOrderRet=max(cumsumOrderRet),
		max_cumsumOrderRet=max(max_cumsumOrderRet),
		max_cumsumOrder=max(max_cumsumOrder),
		revenue_order=max(revenue_order),
		revenue_final_order=max(revenue_final_order),
		revenue_return_order=max(revenue_return_order),
		cust_revenue_final=max(cust_revenue_final),
		sum_quant_cust=max(sum_quant_cust),
		sum_ret_quant_cust=max(sum_ret_quant_cust),
		ret_ratio_cust=max(ret_ratio_cust),
		ret_ratio_order_cust=max(ret_ratio_order_cust),
		ret_cust=max(ret_cust),
		cnt_orders=max(cnt_orders),
		cnt_trans=max(cnt_trans),
		cnt_weekday=max(cnt_weekday),
		cnt_weekend=max(cnt_weekend),
		cnt_sunday=max(cnt_sunday),
		revenue_cust=max(revenue_cust),
		revenue_final_cust=max(revenue_final_cust),
		revenue_return_cust=max(revenue_return_cust),
		minOrderDate=max(minOrderDate),
		maxOrderDate=max(maxOrderDate),
		duration=max(duration),
		order_freq=max(order_freq),
		rev_freq=max(rev_freq),
		article_freq=max(article_freq),
		trans_freq=max(trans_freq),
		diff_min=max(diff_min),
		diff_max=max(diff_max),
		diff_avg=max(diff_avg),
		diff_median=max(diff_median),
		order_trans=max(order_trans),
		cumsum_revenue=max(cumsum_revenue),
		cumsum_rev_fin=max(cumsum_rev_fin),
		cumsum_revenue_return=max(cumsum_revenue_return),
		cumsum_revenue_lag=max(cumsum_revenue_lag),
		cumsum_rev_fin_lag=max(cumsum_rev_fin_lag),
		cumsum_revenue_return_lag=max(cumsum_revenue_return_lag),
		cumsum_quant=max(cumsum_quant),
		cumsum_ret_quant=max(cumsum_ret_quant),
		cumsum_ret_quant_lag=max(cumsum_ret_quant_lag),
		cum_ret_ratio=max(cum_ret_ratio),
		cum_ret_ratio_lag=max(cum_ret_ratio_lag),
		diff_forward=max(diff_forward),
		orderDate.y=max(orderDate.y),
		wday.y=max(wday.y),
		month.y=max(month.y),
		mday.y=max(mday.y),
		weekday.y=max(weekday.y),
		weekend.y=max(weekend.y),
		sunday.y=max(sunday.y),
		sum_quant.y=max(sum_quant.y),
		sum_ret_quant.y=max(sum_ret_quant.y),
		ret_ratio_order.y=max(ret_ratio_order.y),
		revenue_order.y=max(revenue_order.y),
		revenue_final_order.y=max(revenue_final_order.y),
		revenue_return_order.y=max(revenue_return_order.y),
		diff_backwards=max(diff_backwards),
		orderDate.x=max(orderDate.x),
		wday.x=max(wday.x),
		month.x=max(month.x),
		mday.x=max(mday.x),
		weekday.x=max(weekday.x),
		weekend.x=max(weekend.x),
		sunday.x=max(sunday.x),
		sum_quant.x=max(sum_quant.x),
		sum_ret_quant.x=max(sum_ret_quant.x),
		ret_ratio_order.x=max(ret_ratio_order.x),
		revenue_order.x=max(revenue_order.x),
		revenue_final_order.x=max(revenue_final_order.x),
		revenue_return_order.x=max(revenue_return_order.x),
		cumsumOrderRetAll=max(cumsumOrderRetAll),
		customer_age=max(customer_age),
		cumsumOrderRetAllSurv=max(cumsumOrderRetAllSurv),
		number_of_articles_kept=max(number_of_articles_kept),
		total_number_of_returns=max(total_number_of_returns),
		number_of_products_per_customer=max(number_of_products_per_customer),
		number_of_products_per_order_and_customer=max(number_of_products_per_order_and_customer),
		number_of_unique_colors_per_order=max(number_of_unique_colors_per_order),
		number_of_unique_sizes_per_order=max(number_of_unique_sizes_per_order),
		survival=max(survival),
		cumsumOrder=max(cumsumOrder),
		survID=max(survID)
	),
	by=.(customerID, orderID, orderDate)]
	

	
############  Customer level - all customers
return_survival <- dfs[,.(	
		#voucherID=max(as.integer(voucherID)),
		#voucherAmount=max(voucherAmount),
		#deviceID=max(as.integer(deviceID)),
		#paymentMethod=max(paymentMethod),
		#yearQuarter=max(as.numeric(yearQuarter)),
		orders_per_customer=max(orders_per_customer),
		number_of_same_items_in_order=max(number_of_same_items_in_order),
		number_of_items_from_same_category=max(number_of_items_from_same_category),
		total_items_in_order=sum(total_items_in_order),
		#luxury_item_in_product_group=max(luxury_item_in_product_group),
		#cheap_item_in_product_group=max(cheap_item_in_product_group),
		voucher_ratio=max(voucher_ratio),
		relative_deviation_price_mean_byCustomerID=mean(relative_deviation_price_mean_byCustomerID),
		relative_deviation_price_mean_byOrderID=mean(relative_deviation_price_mean_byOrderID),
		gratis_per_customer=max(gratis_per_customer),
		gratis_per_order=max(gratis_per_order),
		giftcard_per_customer=max(giftcard_per_customer),
		not_delivered_order=max(not_delivered_order),
		not_delivered_customer=max(not_delivered_customer),	
		free_delivery_order=max(free_delivery_order),
		free_delivery_customer=max(free_delivery_customer),
		# day=max(day),
		# wday=max(wday),
		# day=max(day),
		# weekday=max(weekday),
		# weekend=max(weekend),
		# sunday=max(sunday),
		# month=max(month),
		# mday=max(mday),
		sum_quant=sum(sum_quant),
		sum_ret_quant=sum(sum_ret_quant),
		ret_ratio_order=mean(ret_ratio_order),
		# orderIDret=max(orderIDret),
		cumsumOrderRet=max(cumsumOrderRet),
		max_cumsumOrderRet=max(max_cumsumOrderRet),
		max_cumsumOrder=max(max_cumsumOrder),
		revenue_order=sum(revenue_order),
		revenue_final_order=sum(revenue_final_order),
		revenue_return_order=sum(revenue_return_order),
		cust_revenue_final=max(cust_revenue_final),
		sum_quant_cust=max(sum_quant_cust),
		sum_ret_quant_cust=max(sum_ret_quant_cust),
		ret_ratio_cust=max(ret_ratio_cust),
		ret_ratio_order_cust=max(ret_ratio_order_cust),
		ret_cust=max(ret_cust),
		cnt_orders=max(cnt_orders),
		cnt_trans=max(cnt_trans),
		cnt_weekday=max(cnt_weekday),
		cnt_weekend=max(cnt_weekend),
		cnt_sunday=max(cnt_sunday),
		revenue_cust=max(revenue_cust),
		revenue_final_cust=max(revenue_final_cust),
		revenue_return_cust=max(revenue_return_cust),
		minOrderDate=max(minOrderDate),
		maxOrderDate=max(maxOrderDate),
		duration=max(duration),
		order_freq=max(order_freq),
		rev_freq=max(rev_freq),
		article_freq=max(article_freq),
		trans_freq=max(trans_freq),
		diff_min=max(diff_min),
		diff_max=max(diff_max),
		diff_avg=max(diff_avg),
		diff_median=max(diff_median),
		order_trans=max(order_trans),
		cumsum_revenue=max(cumsum_revenue),
		cumsum_rev_fin=max(cumsum_rev_fin),
		cumsum_revenue_return=max(cumsum_revenue_return),
		cumsum_revenue_lag=max(cumsum_revenue_lag),
		cumsum_rev_fin_lag=max(cumsum_rev_fin_lag),
		cumsum_revenue_return_lag=max(cumsum_revenue_return_lag),
		cumsum_quant=max(cumsum_quant),
		cumsum_ret_quant=max(cumsum_ret_quant),
		cumsum_ret_quant_lag=max(cumsum_ret_quant_lag),
		cum_ret_ratio=max(cum_ret_ratio),
		cum_ret_ratio_lag=max(cum_ret_ratio_lag),
		# diff_forward=max(diff_forward),
		# orderDate.y=max(orderDate.y),
		# wday.y=max(wday.y),
		# month.y=max(month.y),
		# mday.y=max(mday.y),
		# weekday.y=max(weekday.y),
		# weekend.y=max(weekend.y),
		# sunday.y=max(sunday.y),
		# sum_quant.y=max(sum_quant.y),
		# sum_ret_quant.y=max(sum_ret_quant.y),
		# ret_ratio_order.y=max(ret_ratio_order.y),
		# revenue_order.y=max(revenue_order.y),
		# revenue_final_order.y=max(revenue_final_order.y),
		# revenue_return_order.y=max(revenue_return_order.y),
		# diff_backwards=max(diff_backwards),
		# orderDate.x=max(orderDate.x),
		# wday.x=max(wday.x),
		# month.x=max(month.x),
		# mday.x=max(mday.x),
		# weekday.x=max(weekday.x),
		# weekend.x=max(weekend.x),
		# sunday.x=max(sunday.x),
		# sum_quant.x=max(sum_quant.x),
		# sum_ret_quant.x=max(sum_ret_quant.x),
		# ret_ratio_order.x=max(ret_ratio_order.x),
		# revenue_order.x=max(revenue_order.x),
		# revenue_final_order.x=max(revenue_final_order.x),
		# revenue_return_order.x=max(revenue_return_order.x),
		cumsumOrderRetAll=max(cumsumOrderRetAll),
		customer_age=max(customer_age),
		cumsumOrderRetAllSurv=max(cumsumOrderRetAllSurv),
		number_of_articles_kept=max(number_of_articles_kept),
		total_number_of_returns=max(total_number_of_returns),
		number_of_products_per_customer=max(number_of_products_per_customer),
		number_of_products_per_order_and_customer=max(number_of_products_per_order_and_customer),
		number_of_unique_colors_per_order=max(number_of_unique_colors_per_order),
		number_of_unique_sizes_per_order=max(number_of_unique_sizes_per_order),
		time0=min(cumsumOrder),
		time1=max(cumsumOrder),
		orderDate0=min(orderDate),
		orderDate1=max(orderDate),
		survival=max(survival),
		time_orders=max(cumsumOrder)-min(cumsumOrder)+1
	),
	by=.(customerID, survID)]	
	
	
	
library("survival")

model.1 <- coxph(Surv(time_orders, survival) ~
 sum_quant 
 + revenue_cust
 + number_of_unique_colors_per_order 
# + diff_avg 
+number_of_products_per_order_and_customer
 + article_freq
 +total_items_in_order
 +number_of_unique_sizes_per_order
 +number_of_unique_colors_per_order
 +customer_age
 +order_freq
 +voucher_ratio
 +relative_deviation_price_mean_byCustomerID
 +trans_freq
 +cumsum_quant
 +free_delivery_customer
 +rev_freq
 +gratis_per_customer
 +duration
 +cumsum_revenue_lag
 , data = return_survival)

summary(model.1)

mini.surv <- survfit(Surv(time_orders, survival)~ 1, conf.type="none"
, data = return_survival)

plot(mini.surv , xlab="Time", ylab="Survival Probability")
	
	dev.off()
	
	
order_survival<-order_survival[order(customerID, survID, orderID),]

order_survival <-order_survival[,ind:=1,]
order_survival <- order_survival[,time1:=cumsum(ind),by=.(survID)]
order_survival <- order_survival[,time0:=time1-1,by=.(survID)]
# order_survival[,t0:=NULL]	
# order_survival[,t1:=NULL]	




model.2 <- coxph(Surv(time0, time1, survival) ~ 
sum_quant 
 + revenue_order
 # + number_of_unique_colors_per_order 
 + diff_avg 
 + article_freq
 +order_freq
 +rev_freq
 +total_items_in_order
 +number_of_unique_sizes_per_order
 +number_of_unique_colors_per_order
 +customer_age
 +order_freq
 +voucher_ratio
 +relative_deviation_price_mean_byCustomerID
 +relative_deviation_price_mean_byOrderID
 +voucher_ratio
 +trans_freq
 +cumsum_quant
 # +free_delivery_customer
 +cumsum_revenue_lag
 +diff_forward
+ diff_backwards
+customer_age
+sum_quant.y
+sum_quant.x
+revenue_order.x
+revenue_order.y
# +sum_quant
+cnt_trans
+ weekend
+gratis_per_customer
+duration
+rev_freq
# +number_of_products_per_order_and_customer
, data = order_survival)

summary(model.2)

summary(model.1)


plot(survfit(model.2), xlab='Consecutive orders', 
     ylab='Proportion Not Rearrested')
lines(mini.surv, col=c("red", "blue"))




relevantCols<- c("unique_ID", "customerID", "articleID", "orderID","orderDate", "returnBin", "returnQuantity", "quantity", "sizeCode", "colorCode", "rrp", "productGroup")

dfs<- dfs[ ,relevantCols, with=F]





