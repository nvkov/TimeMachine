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

load("customer_level_v1.RDa")


hist(as.numeric(customer_level$diff_avg))


mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.25,1,0)~ceiling(diff_avg/20) )


# diff_avg  - return_ratio>0.25
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.25,1,0)~ifelse(diff_avg>=180, 180, ceiling(diff_avg/5)*5 ) )

# diff_avg  - return_ratio>0.5
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.5,1,0)~ifelse(diff_avg>=180, 180, ceiling(diff_avg/5)*5 ) )

# diff_avg  - return_ratio>0.1
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.1,1,0)~ifelse(diff_avg>=180, 180, ceiling(diff_avg/5)*5 ) )

# diff_avg  - any return 
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0,1,0)~ifelse(diff_avg>=180, 180, ceiling(diff_avg/5)*5 ) )



# diff_min  - any return
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0,1,0)~ifelse(diff_min>=180, 180, ceiling(diff_min/5)*5 ) )

# diff_min  - return_ratio>0.25
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.25,1,0)~ifelse(diff_min>=180, 180, ceiling(diff_min/5)*5 ) )

# diff_min  - return_ratio>0.5
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.5,1,0)~ifelse(diff_min>=180, 180, ceiling(diff_min/5)*5 ) )



# diff_max  - any return
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0,1,0)~ifelse(diff_max>=180, 180, ceiling(diff_max/5)*5 ) )

# diff_max  - return_ratio>0.25
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.25,1,0)~ifelse(diff_max>=180, 180, ceiling(diff_max/5)*5 ) )

# diff_max  - return_ratio>0.5
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.5,1,0)~ifelse(diff_max>=180, 180, ceiling(diff_max/5)*5 ) )



# diff_median  - any return
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0,1,0)~ifelse(diff_median>=180, 180, ceiling(diff_median/5)*5 ) )

# diff_median  - return_ratio>0.25
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.25,1,0)~ifelse(diff_median>=180, 180, ceiling(diff_median/5)*5 ) )

# diff_median  - return_ratio>0.5
mosaic(
data = customer_level,
ifelse( ret_ratio_cust>0.5,1,0)~ifelse(diff_median>=180, 180, ceiling(diff_median/5)*5 ) )




########################################################################
########################################################################

###### CUSTOMER level forward/backwards
customer_level$avg_interval_between_orders <- ifelse(customer_level$diff_avg>=240, 240, ceiling(customer_level$diff_avg/5)*5)

customer_level$any_return_indicator <- ifelse( customer_level$ret_ratio_cust>0,1,0)

# diff_avg  - any return 
mosaic(
main = "Any Return vs. Average interval between orders - Customer Level",
data = customer_level,
factor(any_return_indicator)~avg_interval_between_orders )


# diff_avg  - any return 
plot(
xlab = "Average number of days between orders",
ylab = "Return Ratio",
main = "Return Ratio vs. Average interval between orders - Customer Level",
data = customer_level,
factor(floor(ret_ratio_customer/0.10)*0.10)~avg_interval_between_orders 
, col=diverge_hcl(12)
)

# diff_avg  - any return 
plot(
xlab = "Average number of days between orders",
ylab = "Return Ratio",
main = "Return Ratio vs. Average interval between orders - Customer Level",
data = customer_level,
factor(floor(ret_ratio_customer/0.20)*0.20)~avg_interval_between_orders 
, col=diverge_hcl(6)
)



# diff_min  - any return
mosaic(ifelse( customer_level$ret_ratio_cust>0,1,0)~ifelse(customer_level$diff_min>=180, 180, ceiling(customer_level$diff_min/5)*5 ) )


# diff_max  - any return
mosaic(ifelse( customer_level$ret_ratio_cust>0,1,0)~ifelse(customer_level$diff_max>=180, 180, ceiling(customer_level$diff_max/5)*5 ) )


# diff_max  - return_ratio>0.25
mosaic(ifelse( customer_level$ret_ratio_cust>0.25,1,0)~ifelse(customer_level$diff_max>=180, 180, ceiling(customer_level$diff_max/5)*5 ) )






