rm(list=ls())

# Find one time customers:
library("data.table")

load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")
load("C:/Users/Nk/Documents/Uni/APA/survival.RDa")

df<- df[!is.na(df$returnQuantity)]

relevantCols<- c("unique_ID", "customerID", "articleID", "orderID","orderDate", "returnBin", 
                 "returnQuantity", "quantity", "sizeCode", "colorCode", "rrp", "productGroup")
df<- df[ ,relevantCols, with=F]


#######################
survival.df<- data.frame(survival.df)

coxph.model <- coxph( Surv(survival.df$survival_time)~survival.df$elapsed_time+ 
                        survival.df$total_revenue+
                        survival.df$total_money_saved+
                        survival.df$total_number_of_returns+
                        survival.df$total_number_of_orders +
                        strata(survival.df$strata),
                      method="efron")
summary(coxph.model)
