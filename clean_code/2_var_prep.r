
########################################################################
##### MOSAICPLOTS - interval between orders  - order_level_joined_inner


#Mosaicplot diff forward - 30 days cutoff binary
mosaic(
main = "Mosaicplot forward -  30 days cutoff",
data = order_level_joined_inner,
return_indicator_forward
~ifelse(diff_forward>=30, 30, ceiling(diff_forward/1)*1 )
)

#Mosaicplot diff forward - 10 days cutoff binary
mosaic(
main = "Mosaicplot forward -  10 days cutoff",
data = order_level_joined_inner,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
return_indicator_forward
~days_between_orders_10
)

#Mosaicplot diff forward - 10 days cutoff binary
plot(
main = "Any Return (order) vs. No. of days since last order - 10 days cutoff",
data=order_level_joined_inner,
xlab  = "any return in the order - indicator",
ylab = "no. of days since last order",
factor(return_indicator_forward)
~factor(days_between_orders_10)
)




#Mosaicplot diff backwards 20 days cutoff binary
mosaic(
main = "Mosaicplot forward - 10  10 days cutoff",
data = order_level_joined_inner,
return_indicator_backwards~
ifelse(diff_forward>=20, 20, ceiling(diff_forward/1)*1 )
)

#Mosaicplot diff backwards - 10 days cutoff binary
mosaic(
main = "Mosaicplot backwards -  10 days cutoff",
data = order_level_joined_inner,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
return_indicator_backwards
~days_between_orders_10
)

#Mosaicplot diff backwards - 7 days cutoff binary
mosaic(
main = "Mosaicplot backwards -  7 days cutoff",
data = order_level_joined_inner,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
return_indicator_backwards
~days_between_orders_7
)

mosaic(
data = order_level_joined_inner,
ifelse(sum_ret_quant.y>0,1,0),
~ceiling(revenue_order.x/20)
)
 
mosaic(
data = order_level_joined_inner,
ifelse(sum_ret_quant.y>0,1,0),
~ceiling(revenue_final_order.x/20)
)
 
mosaic(
data = order_level_joined_inner,
ifelse(sum_ret_quant.y>0,1,0),
~(sum_quant.x)
)
 

mosaic(ifelse( order_level_joined_inner$ret_ratio_order.y>0.5,1,0)~ifelse(order_level_joined_inner$diff_forward>60, 13*5, ceiling(order_level_joined_inner$diff_forward/5)*5 ))

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~floor(order_level_joined_inner$diff_forward/10) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward<30, 1, 2) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~floor(order_level_joined_inner$diff_forward/10) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward<30, 1, 2) )

mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward>60, 13*5, ceiling(order_level_joined_inner$diff_forward/5)*5 ))

mosaic(ifelse( order_level_joined_inner$ret_ratio_order.y>0.5,1,0)~ifelse(order_level_joined_inner$diff_forward>60, 13*5, ceiling(order_level_joined_inner$diff_forward/5)*5 ))

mosaic(ifelse( order_level_joined_inner$ret_ratio_order.y>0.25,1,0)~ifelse(order_level_joined_inner$diff_forward>30, 31, order_level_joined_inner$diff_forward ))

mosaic(ifelse( order_level_joined_inner$ret_ratio_order.y>0,1,0)~ifelse(order_level_joined_inner$diff_forward>30, 31, order_level_joined_inner$diff_forward ))


# Mosaicplot: ReturnBin_order for the next order based on the weekday of the previous order
mosaic(ifelse( order_level_joined_inner$sum_ret_quant.y>0,1,0)~order_level_joined_inner$day.x)
# Mosaicplot: ReturnBin_order for the previous order based on the weekday of the next order
mosaic(ifelse( order_level_joined_inner$sum_ret_quant.x>0,1,0)~order_level_joined_inner$day.y)


########################################################################
##### Mosaicplots: ORDER_LEVEL

mosaic(ifelse( order_level$sum_ret_quant>0,1,0)~order_level$wday)
# barplot(table(ifelse( order_level$sum_ret_quant>0,1,0),order_level$wday))

mosaic(ifelse( order_level$sum_ret_quant>0,1,0)~order_level$mday)
# barplot(table(ifelse( order_level$sum_ret_quant>0,1,0),order_level$mday))

mosaic(ifelse( order_level$sum_ret_quant>0,1,0)~order_level$month)



########################################################################
##### Mosaicplots: ORDER_LEVEL

customer_level_inner

