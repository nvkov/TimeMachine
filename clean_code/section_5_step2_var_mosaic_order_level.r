##############################################################
########## Mosaicplots: ORDER_LEVEL


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

load("order_level_v1.RDa")

##########
##########  Mosaicplot Variables
# days_between_orders_10: groupped interval until next order - ORDER LVL
order_level <- order_level[,days_between_orders_10:= ifelse(diff_forward>10, "0_or_>10_days", ifelse(diff_forward==0,"0_or_>10_days","1_to_10_days")),]

# days_between_orders_7: groupped interval until next order  - ORDER LVL
order_level <- order_level[,days_between_orders_7:=  ifelse(diff_forward>7, "0_or_>7_days", ifelse(diff_forward==0,"0_or_>7_days","1_to_7_days")),]

# days_between_orders_back_10: groupped interval since last order - ORDER LVL
order_level <- order_level[,days_between_orders_back_10:= ifelse(diff_backwards>10, "0_or_>10_days", ifelse(diff_backwards==0,"0_or_>10_days","1_to_10_days")),]

# days_between_orders_back_7: groupped interval since last order  - ORDER LVL
order_level <- order_level[,days_between_orders_back_7:=  ifelse(diff_backwards>7, "0_or_>7_days", ifelse(diff_backwards==0,"0_or_>7_days","1_to_7_days")),]


########################################################################
##### MOSAICPLOTS - interval between orders

#Mosaicplot diff forward - 30 days cutoff binary
mosaic(
main = "Mosaicplot forward -  30 days cutoff",
data = order_level,
returnBin_order
~ifelse(diff_forward>=30, 30, ceiling(diff_forward/1)*1 )
)

#Mosaicplot diff forward - 10 days cutoff binary
mosaic(
main = "Mosaicplot forward -  10 days cutoff",
data = order_level,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
returnBin_order
~days_between_orders_10
)

#Mosaicplot diff forward - 10 days cutoff binary
plot(
main = "Any Return (order) vs. No. of days since last order - 10 days cutoff",
data=order_level,
xlab  = "any return in the order - indicator",
ylab = "no. of days since last order",
factor(returnBin_order)
~factor(days_between_orders_10)
)

dev.off()


#Mosaicplot diff backwards 20 days cutoff binary
mosaic(
main = "Mosaicplot backwards -  20 days cutoff",
data = order_level,
returnBin_order~
ifelse(diff_backwards>=20, 20, ceiling(diff_backwards/1)*1 )
)

#Mosaicplot diff backwards - 10 days cutoff binary
mosaic(
main = "Mosaicplot backwards -  10 days cutoff",
data = order_level,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
returnBin_order
~days_between_orders_back_10
)

#Mosaicplot diff backwards - 7 days cutoff binary
mosaic(
main = "Mosaicplot backwards -  7 days cutoff",
data = order_level,
xlab  = NULL,#"return_indicator",
ylab = NULL,#"no._of_days_between_orders",
return_indicator_backwards
~days_between_orders_7
)

mosaic(
data = order_level,
returnBin_order
~ceiling(revenue_order.x/20)
)
 
mosaic(
data = order_level,
returnBin_order
~(ceiling(revenue_final_order.x/20))
)
 
mosaic(
data = order_level,
returnBin_order
~(sum_quant.x)
)
 

mosaic(ifelse( order_level$ret_ratio_order.y>0.5,1,0)~ifelse(order_level$diff_forward>60, 13*5, ceiling(order_level$diff_forward/5)*5 ))

mosaic(
data = order_level,
returnBin_order
~floor(diff_forward/10) )

mosaic(
data = order_level,
returnBin_order
~ifelse(diff_forward<30, 1, 2) )

mosaic(
data = order_level,
returnBin_order
~floor(diff_backwards/10) )

mosaic(
data = order_level,
returnBin_order
~ifelse(diff_backwards<30, 1, 2) )

mosaic(
data = order_level,
returnBin_order
~ifelse(diff_forward>60, 13*5, ceiling(diff_forward/5)*5 )
)

mosaic(
data = order_level,
ifelse(ret_ratio_order>0.5,1,0)
~ifelse(diff_backwards>60, 13*5, ceiling(diff_backwards/5)*5 )
)

mosaic(
data = order_level,
ifelse( ret_ratio_order>0.25,1,0)
~ifelse(diff_forward>30, 31, order_level$diff_forward ))

mosaic(
data = order_level,
returnBin_order
~ifelse(diff_backwards>30, 31, diff_backwards ))


# Mosaicplot: ReturnBin_order for the next order based on the weekday of the previous order
mosaic(
data = order_level,
returnBin_order
~wday.x)
# Mosaicplot: ReturnBin_order for the previous order based on the weekday of the next order
mosaic(
data = order_level,
returnBin_order
~wday.y)


mosaic(
data = order_level,
returnBin_order
~mday)

mosaic(
data = order_level,
returnBin_order
~month)


########################################################################
########################################################################

########################  
#####Number of days since last order

plot(main = "Any Return vs. Number of days since last order - Order level",
data=order_level,
xlab = "no. of days since last order",
ylab = "any return in the order - indicator",
factor(ifelse(sum_ret_quant>0,"RETURN","NO Returns"))
~factor(ifelse(is.na(diff_backwards), "no previous order", ifelse(diff_backwards>=10,">=10 or 0 days",ifelse(diff_backwards==0,">=10 or 0 days",ceiling(diff_backwards/1)*1))))
)

plot(main = "Any Return vs. Number of days since last order - Order level",
data=order_level_all,
xlab = "number of days since last order",
ylab = "any return in the order - indicator",
factor(ifelse(sum_ret_quant>0,"RETURN","NO Returns"))
~factor(ifelse(is.na(diff_backwards), "no previous order", ifelse(diff_backwards>=10,">=10 or 0 days",ifelse(diff_backwards==0,">=10 or 0 days","within 9 days"))))
)


#####Number of days until next order

plot(main = "Any Return vs. Number of days until next order - Order level",
data=order_level,
xlab = "number of days until next order",
ylab = "any return in the order - indicator",
factor(ifelse(sum_ret_quant>0,"RETURN","NO Returns"))
~factor(ifelse(is.na(diff_forward), "no next order", ifelse(diff_forward>7,">7 days","within 7 days")))
)


plot(main = "Any Return vs. Number of days until next order - Order level",
data=order_level,
xlab = "number of days until next order",
ylab = "any return in the order - indicator",
factor(ifelse(sum_ret_quant>0,"RETURN","NO Returns"))
~factor(ifelse(is.na(diff_forward), "no next order", ifelse(diff_forward>=10,">=10 days","within 9 days")))
)




plot(main = "Any Return vs. Number of days until next order - Order level",
data=order_level_all,
xlab = "number of days until next order",
ylab = "any return in the order - indicator",
factor(ifelse(sum_ret_quant>0,"RETURN","NO Returns"))
~factor(ifelse(is.na(diff_forward), "no next order", ifelse(diff_forward>=10,">=10 days",diff_forward)))
)
