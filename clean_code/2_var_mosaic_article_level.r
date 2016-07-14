########################################################################
##### Mosaicplots: ARTICLE_LEVEL

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

load("df_v1.RDa")


### Variable
df<- df[,cumsum_rev_fin_lag_2500:=ifelse(cumsum_rev_fin_lag>=2500, 2500,(ceiling(cumsum_rev_fin_lag/100)*100)),]

df<- df[,cumsum_rev_fin_lag_2500_25:=ifelse(cumsum_rev_fin_lag>=1000, 1000,(ceiling(cumsum_rev_fin_lag/50)*50)),]
####



plot(factor(df$returnBin)~factor(df$cumsum_rev_fin_lag_2500_25))


plot(factor(df$returnBin)~factor(ifelse(df$cumsum_revenue_return_lag>=500, 500,(ceiling(df$cumsum_revenue_return_lag/20)*20))))

plot(factor(df$returnBin)~df$cumsum_rev_fin_lag_2500)
# barplot(table(ifelse(df$returnQuantity>0,1,0),df$cumsum_rev_fin_lag_2500))



#Mosaicplot Cumulative Sum of Final Revenue Lag(1)"
plot(
main = "Return Binary vs. Cumulative Sum of Final Revenue Lag(1)",
data=df,
xlab  = "Return Binary",
ylab = "Cumulative Sum of Final Revenue Lag(1)",
factor(returnBin)
~(cumsum_rev_fin_lag_2500_25)
, col=diverge_hcl(4)
)



#########################################
############ARTICLE Level
#########################################

df$avg_interval_between_orders <- ifelse(df$diff_avg>=240, 240, ceiling(df$diff_avg/5)*5)

###### Order level forward/backwards

# diff_avg  - any return 
plot(
xlab = "Average number of days between orders",
ylab = "Return Binary ",
main = "Return Binary vs. Average interval between orders - Article Level",
data = df,
factor(returnBin)
~ifelse(is.na(avg_interval_between_orders),-1,avg_interval_between_orders)
, col=diverge_hcl(12)
)


plot(main = "Return binary vs. Number of days until next order - Article level",
data=df,
xlab = "no. of days until next order",
ylab = "Return Binary",
factor(returnBin)
~(ifelse(is.na(diff_forward), -1, ifelse(diff_forward>=60,60,ceiling(diff_forward/1)*1)))
)



df<- df[,min_interval_between_orders:=ifelse(diff_min>=30, 30, diff_min),]

# diff_min  - any return 
plot(
xlab = "minimum number of days between orders",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Min. interval between orders - Article Level",
data = df,
factor(returnBin)
~ifelse(is.na(min_interval_between_orders),-1,min_interval_between_orders)
, col=diverge_hcl(12)
)



df<- df[,min_interval_between_orders:=ifelse(diff_min>=60, 60, diff_min),]

# diff_max  - any return 
plot(
xlab = "minimum number of days between orders",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Min. interval between orders - Article Level",
data = df,
factor(returnBin)
~ifelse(is.na(min_interval_between_orders),-1,min_interval_between_orders)
, col=diverge_hcl(12)
)



df<- df[,max_interval_between_orders:=ifelse(diff_forward_max>=300, 300, ceiling(diff_forward_max/5)*5),]

# diff_max  - any return 
plot(
xlab = "maximum number of days between orders",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Max. interval between orders - Article Level",
data = df,
factor(returnBin)
~ifelse(is.na(max_interval_between_orders),-1,max_interval_between_orders)
, col=diverge_hcl(12)
)



df<- df[,max_interval_between_orders:=ifelse(diff_max>=300, 300, ceiling(diff_max/5)*5),]

# diff_max  - any return 
plot(
xlab = "maximum number of days between orders",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Max. interval between orders - Article Level",
data = df,
factor(returnBin)
~ifelse(is.na(max_interval_between_orders),-1,max_interval_between_orders)
, col=diverge_hcl(12)
)



df<- df[,revenue_order_25:=ifelse(revenue_order>=500,500,ceiling(revenue_order/25)*25),]

# revenue per order
plot(
xlab = "Revenue per order",
ylab = "Return Binary",
main = "Return Binary vs. Revenue per Order - Article Level",
data = df,
factor(returnBin)
~revenue_order_25
, col=rainbow_hcl(10)
)


# revenue per order
mosaic(
xlab = "revenue per order",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Revenue per Order - Article Level",
data = df,
(returnBin)
~revenue_order_25
, col=diverge_hcl(12)
)



df<- df[,cumsum_revenue_lag_25:=ifelse(cumsum_revenue_lag>=500,500,ceiling(cumsum_revenue_lag/25)*25),]
df<- df[,cumsum_revenue_lag_50:=ifelse(cumsum_revenue_lag>=500,500,ceiling(cumsum_revenue_lag/50)*50),]
 
 
 # revenue per order
plot(
xlab = "Cumulated Revenue Lag(1) per order",
ylab = "Return Binary",
main = "Return Binary vs. Cumulated Revenue Lag(1) Order - Article Level",
data = df,
factor(returnBin)
~cumsum_revenue_lag_25
, col=rainbow_hcl(10)
)
 
# revenue per order
mosaic(
xlab = "revenue per order",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Cum. Revenue Lag(1) Order - Art. Level",
data = df,
(returnBin)
~cumsum_revenue_lag_25
, col=diverge_hcl(12)
)

dev.off()


# diff_avg  - any return 
plot(
xlab = "Return Ratio of the previous order",
ylab = "Return Binary",
main = "Return Binary vs. Return Ratio of the previous order",
data = df,
factor(returnBin) ~ ifelse(is.na(ret_ratio_order.x),-1,(floor(ret_ratio_order.x/0.10)*0.10))
, col=terrain_hcl(3)
)


#"Return Ratio of the previous order"
mosaic(
xlab = "Return Ratio of the previous order",
ylab = "Return Binary",
main = "Return Binary vs. Return Ratio of the previous order",
data = df,
(returnBin)
~ifelse(is.na(ret_ratio_order.x),-1,(floor(ret_ratio_order.x/0.10)*0.10))
, col=diverge_hcl(12)
)



#"Return Ratio of the next order"
plot(
xlab = "Return Ratio of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Return Ratio of the next order",
data = df,
factor(returnBin) ~ ifelse(is.na(ret_ratio_order.y),-1,(floor(ret_ratio_order.y/0.10)*0.10))
, col=terrain_hcl(3)
)
#"Return Ratio of the next order"
mosaic(
xlab = "Return Ratio of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Return Ratio of the next order",
data = df,
(returnBin)
~ifelse(is.na(ret_ratio_order.y),-1,(floor(ret_ratio_order.y/0.10)*0.10))
, col=diverge_hcl(12)
)





df<- df[,quantity_of_previous_order:=ifelse(is.na(sum_quant.x), -1, ifelse(sum_quant.x>=10, 10, sum_quant.x)),]

# Quantity of the previous order 
plot(
xlab = "Quantity of the previous order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the previous order",
data = df,
factor(returnBin)
~quantity_of_previous_order
, col=heat_hcl(3)
)
dev.off()

# Quantity of the previous order
mosaic(
xlab = "revenue per order",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Quantity of the previous order",
data = df,
returnBin
~quantity_of_previous_order
, col=diverge_hcl(12)
)



df<- df[,quantity_of_next_order:=ifelse(is.na(sum_quant.y), -1, ifelse(sum_quant.y>=10, 10, sum_quant.y)),]

# Quantity of the next order
plot(
xlab = "Quantity of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the next order",
data = df,
factor(returnBin)
~quantity_of_next_order
, col=heat_hcl(3)
)


# Quantity of the next orde
mosaic(
xlab = "Quantity of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the next order",
data = df,
returnBin
~quantity_of_next_order
, col=diverge_hcl(12)
)








df<- df[,revenue_of_previous_order:=ifelse(is.na(revenue_order.x), -1, ifelse(revenue_order.x>=300, 300, revenue_order.x)),]

# Revenue of the previous order
plot(
xlab = "Revenue of the previous order",
ylab = "Return Binary",
main = "Return Binary vs. Revenue of the previous order",
data = df,
factor(returnBin)
~revenue_of_previous_order
, col=heat_hcl(3)
)




df<- df[,revenue_of_next_order:=ifelse(is.na(revenue_order.y), -1, ifelse(revenue_order.y>=300, 300, revenue_order.y)),]

# Revenue of the next order
plot(
xlab = "Revenue of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Revenue of the next order",
data = df,
factor(returnBin)
~revenue_of_next_order
, col=rainbow_hcl(3)
)

head(df)


df<- df[,revenue_final_of_previous_order:=ifelse(is.na(revenue_final_order.x), -1, ifelse(revenue_final_order.x>=300, 300, revenue_final_order.x)),]

# Revenue final of the previous order
plot(
xlab = "Revenue final of the previous order",
ylab = "Return Binary",
main = "Return Binary vs. Revenue final of the previous order",
data = df,
factor(returnBin)
~revenue_final_of_previous_order
, col=heat_hcl(3)
)



df<- df[,revenue_final_of_next_order:=ifelse(is.na(revenue_final_order.y), -1, ifelse(revenue_final_order.y>=300, 300, revenue_final_order.y)),]

# Revenue final of the next order
plot(
xlab = "Revenue final of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Revenue final of the next order",
data = df,
factor(returnBin)
~revenue_final_of_next_order
, col=rainbow_hcl(3)
)



# Quantity of the order
mosaic(
xlab = "quantity per order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the order",
data = df,
returnBin
~ifelse(sum_quant>15,15,sum_quant)
, col=diverge_hcl(12)
)

# Quantity of the order
plot(
xlab = "quantity per order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the order",
data = df,
factor(returnBin)
~ifelse(sum_quant>15,15,sum_quant)
, col=diverge_hcl(12)
)



df<- df[,quantity_of_previous_order_14:=ifelse(is.na(sum_quant.x), -1, ifelse(diff_backwards >14, -2, ifelse(sum_quant.x>=10, 10, sum_quant.x))),]

# Quantity of the previous order 14 days
plot(
xlab = "Quantity of the previous order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the previous order in 14 days",
data = df,
factor(returnBin)
~quantity_of_previous_order_14
, col=terrain_hcl(4)
)


# Quantity of the previous order 14 days
mosaic(
xlab = "revenue per order",
ylab = "any return for the customer - indicator",
main = "Return Binary vs. Quantity of the previous order",
data = df,
returnBin
~quantity_of_previous_order_14
, col=diverge_hcl(12)
)



dev.off()
df<- df[,quantity_of_next_order_14:=ifelse(is.na(sum_quant.y), -1, ifelse(diff_forward >14, -2, ifelse(sum_quant.y>=10, 10, sum_quant.y))),]


# Quantity of the next order 14 days
plot(
xlab = "Quantity of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the next order in 14 days",
data = df,
factor(returnBin)
~quantity_of_next_order_14
, col=terrain_hcl(4)
)


# Quantity of the next order 14 days
mosaic(
xlab = "Quantity of the next order",
ylab = "Return Binary",
main = "Return Binary vs. Quantity of the next order in 14 days",
data = df,
returnBin
~quantity_of_next_order_14
, col=diverge_hcl(12)
)





# Cumulative Return Ratio Lag(1)
plot(
xlab = "Cumulative Return Ratio Lag(1)",
ylab = "Return Binary",
main = "Return Binary vs. Cumulative Return Ratio Lag(1) - Article Level",
data = df,
factor(returnBin)
 ~cum_ret_ratio_lag
, col=diverge_hcl(4)
)

# dev.off()


head(df)






# Cumulative Return Ratio Lag(1)
plot(
xlab = "Cumulative Return Ratio Lag(1)",
ylab = "Return Binary",
main = "Return Binary vs. Cumulative Return Ratio Lag(1) - Article Level",
data = df[df$customerID=="000531"],
cum_ret_ratio
 ~orderDate 
, col=diverge_hcl(4),
type="l"
)







# Cumulative Return Ratio Lag(1)
plot(
xlab = "Cumulative Return Ratio Lag(1)",
ylab = "Return Binary",
main = "Return Binary vs. Cumulative Return Ratio Lag(1) - Article Level",
data = df[df$customerID=="000931"],
cum_ret_ratio
 ~cumsumOrder 
, col=diverge_hcl(4),
type="l",
ylim=c(0,1)
)
abline(h=0.4631086,lty=2,col="green", lwd=2)

#mean(order_level$ret_ratio_order)

