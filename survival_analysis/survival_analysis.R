#Compare data partitioning
#Prepare console:
rm(list=ls())

#Working directory:
setwd("C:/Users/Nk/Documents/Uni/APA/03_variable_creation/")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_pred_functions_updated.R")
source("C:/Users/Nk/Documents/Uni/APA/TimeMachine/clean_code/functions/functions_plot_partitioning.R")


# Load libraries:
library("data.table")
library("ggplot2")
library("lattice")
library("vcd")
library("gridExtra")
library("StatMatch")
library("stargazer")

#Load data:
load("C:/Users/Nk/Documents/Uni/APA/data_2_10.RDa")
df<- df[!is.na(df$returnQuantity),]

#Order data in correct order
df<- df[order(df$orderDate),]
df<- df[order(df$customerID),]

#Pre prep:
df$money_saved<- df$price*as.numeric(as.character(df$returnBin))*df$returnQuantity
df$money_saved_idx<- ifelse(df$money_saved>0, 1, 0)

#Condense dataset to order level:
order.df<- df[, .(returnBin=max(as.numeric(as.character(returnBin))), 
                  number_of_products_per_order=sum(quantity), 
                  money_spent= sum(price*quantity), 
                  number_of_returned_products=sum(returnQuantity) ,
                  return_percent=sum(returnQuantity)/sum(quantity), 
                  total_money_saved=sum(money_saved), 
                  money_saved_product_count=sum(money_saved_idx)) , 
              by=.(orderID, customerID, orderDate)]



order.df<- order.df[,number_of_orders:=.N , by=customerID]
order.df<- order.df[order.df$number_of_orders>=5,]

################################################################

#Create additional observation for censored data:
latest_date<- order.df[,.(orderDate=max(orderDate)+1), by=.(customerID)]

#Add additional failure time for censored data:
latest_date$returnBin<- rep(1, nrow(latest_date))
latest_date$censor<- rep(1, nrow(latest_date))

#Estimate survival time:
survival.df<- order.df[,c("customerID", "orderDate", "returnBin") , with=F]
survival.df$censor<- rep(0, nrow(survival.df))
survival.df<- rbind(survival.df, latest_date)

survival.df<- survival.df[order(survival.df$orderDate),]
survival.df<- survival.df[order(survival.df$customerID),]

#count survival time:
survival_time<- diff(c(0, as.integer(gregexpr("1", paste0(survival.df$returnBin, collapse = ""))[[1]]) ) )
survival<- rep(survival_time, survival_time)
survival_idx<- rep(1:length(survival_time), times=survival_time)
survival.df<- cbind(survival.df, survival, survival_idx)

#Find censored data and remove redundant observations:
#Step 1: remove redundant obs:
survival.df<-survival.df[survival.df$censor!= survival.df$survival]

#Step 2: find censored obs:
rows<- survival.df$survival_idx[which(survival.df$censor==1)]
survival.df$censor[survival.df$survival_idx %in% rows]<- 2
survival.df$survival[survival.df$survival_idx %in% rows]<- survival.df$survival[survival.df$survival_idx %in% rows]-1
survival.df$censor[survival.df$censor==0]<- 1
survival.df$censor[survival.df$censor==2]<- 0

#Step 3: remove the redundant obs for censored data:
survival.df<- survival.df[!(survival.df$censor==0 & survival.df$returnBin==1),]

#Merge order data with survival data:
order.df<- order.df[order(order.df$orderDate),]
order.df<- order.df[order(order.df$customerID),]

#Merge data for the survival dataset:
survival.data<- cbind(order.df, survival.df[,c("censor", "survival", "survival_idx" ), with=F])

#Create additional vars for the survival analysis:
survival.data<- survival.data[, ellapsed_time:=max(orderDate)-min(orderDate),
                              by=survival_idx]

############################################################
#Survival trees:
library(rpart)
library(survival)


survival.data.final<- survival.data[,.(total_money_spent=sum(money_spent), 
                                       return_percent=max(return_percent), 
                                       number_of_products=sum(number_of_products_per_order), 
                                       number_of_returned_products=sum(number_of_returned_products),
                                       total_money_saved=sum(total_money_saved)), 
                                    by=.(survival_idx, ellapsed_time, customerID, censor, survival)]

survival.data.final<- survival.data.final[,`:=`(average_length_of_survival= sum(survival)/.N, 
                                                min_length_of_survival= min(survival), 
                                                max_length_of_survival=max(survival), 
                                                num_survival_events=.N, 
                                                var_survival= var(survival),
                                                average_ellapsed_time=sum(ellapsed_time)/.N)
                                            ,by=customerID]

# Pass a survival object from Surv() to the function rpart() to perform the analysis.
fit <- rpart(Surv(survival, censor) ~ number_of_products + 
               total_money_spent + return_percent + ellapsed_time +
               average_length_of_survival + min_length_of_survival +
               max_length_of_survival + total_money_saved,
             data=survival.data.final)

# plot the resulting tree

plot(fit, uniform=T, branch=.4, compress=T)
text(fit, use.n=T)
# The print() function provides details of the tree not shown above
print(fit)


# CoxPH -------------------------------------------------------------------

m.coxph <- coxph(Surv(survival, censor) ~ number_of_products + 
                   total_money_spent + return_percent + ellapsed_time +
                   average_length_of_survival + min_length_of_survival +
                   max_length_of_survival + var_survival + total_money_saved +
                   average_ellapsed_time,
                    data=survival.data.final, method="breslow")
summary(m.coxph)

stargazer(m.coxph, title="Survival analysis: Cox proportional hazards")
  

# Survival for subgroup  --------------------------------------------------

survival.data.final.subset<- survival.data.final[survival.data.final$survival!=1,]

#################Models
fit <- rpart(Surv(survival, censor) ~ number_of_products + 
               money_spent + return_percent + ellapsed_time +
               average_length_of_survival + min_length_of_survival +
               max_length_of_survival + var_survival,
             data=survival.data.final.subset)

# plot the resulting tree

plot(fit, uniform=T, branch=.4, compress=T)
text(fit, use.n=T)
# The print() function provides details of the tree not shown above
print(fit)


# CoxPH -------------------------------------------------------------------

m.coxph <- coxph(Surv(survival, censor) ~ number_of_products + 
                   money_spent + return_percent + ellapsed_time +
                   average_length_of_survival + min_length_of_survival +
                   max_length_of_survival + var_survival,
                 data=survival.data.final.subset, method="breslow")
summary(m.coxph)


#Save survival data frame:
save(survival.data.final, file="C:/Users/Nk/Documents/Uni/APA/survival_data.RDa")