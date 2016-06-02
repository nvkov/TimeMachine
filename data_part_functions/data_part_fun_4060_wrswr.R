#Data partitioning:

rm(list=ls())
library(ggplot2)
library(dplyr)
library("data.table")
library("vcd")
library("gridExtra")

set.seed(1)
load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

#Prepare dataset for assesment:
tr<- df[!is.na(df$returnQuantity)]
#df<- df[!is.na(df$returnQuantity)]

### Splitting data in training (tr)  and classification (te) sets:
#Cut the last 3 months for pseudo-classification:
#te<- df[df$orderDate>="2015-07-01",]
#tr<- df[df$orderDate<="2015-07-01",]
#df$group<-ifelse(df$orderDate>="2015-07-01", "class", "train")


# Keep only training set for data partininig
rm(te, df)

#Transorm data.table to data.frame to use partitioning function with dplyr:
tr<- data.frame(tr)
#-----------------------------------------------------------------End Data Preprocessing

#### "Data partitioning strategy" task ###################
# Random sampling WR: 
#samp_size<- floor(0.08*nrow(tr))

# Data_partitioning

data_partitioning_v1<- function(tr, samp_perc){
  samp_size<- floor(samp_perc*nrow(tr))
  
  #Generate weights on quantiles:
  q25<- quantile(as.numeric(tr$orderDate), 0.25)
  q75<- quantile(as.numeric(tr$orderDate), 0.75)
  
  tr$ww<-rep(NA, nrow(tr))
  tr$ww[as.numeric(tr$orderDate)<=q25]<- 0.05
  tr$ww[as.numeric(tr$orderDate)>q25 & as.numeric(tr$orderDate)<=q75]<- 0.15
  tr$ww[as.numeric(tr$orderDate)>q75]<- 0.8
  
  data_part<- sample(nrow(tr), size=samp_size, replace=T, prob=tr$ww)
  
  # Weighted random partioning: test + validation
  mtest <- tr[data_part,] 
  mtrain <- tr[-data_part,] 
  
  rm(data_part)
  tr$ww<- NULL
  
  relevantCols<- c("unique_ID", "customerID", "orderDate")
  mtrain<- mtrain[,relevantCols]
  mtest<- mtest[, relevantCols]
  #Cut all inconsistent (future) observations from the mtrain set:
  
  ## Step 1: Find unique customerIDs and earliest date from the validation set:
  uniques<- mtest %>% group_by(customerID) %>% summarise(min(as.Date(as.character(orderDate))))
  colnames(uniques)[2]<- "minDate"
  
  ## Step 2: Extract obs. from the train set that where later than the observations in the validation (mtest) dataset:
  # Step 2.1: Find all customerIDs that come up in both the validation and train set:
  int_mtrain<- mtrain[mtrain$customerID %in% uniques$customerID,]
  
  # Step 2.2: See which observations in the train data occured before and 
  # after the obs in the validation set:
  joint_mtrain<- merge(int_mtrain, uniques, by="customerID")
  
  # Step 2.3: Subset all observations that occured earlier and keep for the training set
  xmtrain<- subset(joint_mtrain, as.Date(as.character(joint_mtrain$orderDate))<joint_mtrain$minDate)
  xmtrain$minDate<- NULL
  
  rm(int_mtrain)
  
  ## Step 3: Keep all observations not in the validation dataset for the training set:
  comp_mtrain<- mtrain[!mtrain$customerID %in% uniques$customerID,]
  rm(mtrain, uniques)
  
  #Check if observations are in the same order:
  #cbind(names(xmtrain), names(comp_mtrain))
  #change order of columns:
  comp_mtrain<-comp_mtrain %>% select(order(names(comp_mtrain)))
  xmtrain<-xmtrain %>% select(order(names(xmtrain)))
  #Double check - seems ok
  #cbind(names(xmtrain), names(comp_mtrain))
  
  ## Step 4: Merge mtrain files for the final version:
  final_mtrain<- rbind(xmtrain, comp_mtrain)
  rm(xmtrain, comp_mtrain, joint_mtrain)
  print(nrow(final_mtrain))
  return(final_mtrain)
}

t.train.samp_size<- 0.08
t.trainID<- data_partitioning_v1(tr, t.train.samp_size)

t.train<-tr[tr$unique_ID %in% t.trainID$unique_ID,] 
t.valid<- tr[!tr$unique_ID %in% t.trainID$unique_ID,]

ratio<- nrow(t.train)/nrow(tr)


#Second level partitioning:
t.t.samp_size<-0.12

t.t.trainID<- data_partitioning_v1(t.train, t.t.samp_size)
t.t.train<-t.train[t.train$unique_ID %in% t.t.trainID$unique_ID,]
t.t.valid<-t.train[!t.train$unique_ID %in% t.t.trainID$unique_ID,]  

t.train$part_v1<- rep("t.tr", nrow(t.train))
t.valid$part_v1<- rep("t.va", nrow(t.valid))
t.t.train$part_v1<- rep("tt.tr", nrow(t.t.train))
t.t.valid$part_v1<- rep("tt.va", nrow(t.t.valid))

part<- rbind(t.valid[, c("unique_ID", "part_v1")], t.t.train[, c("unique_ID", "part_v1")], t.t.valid[, c("unique_ID", "part_v1")]) 

t.t.ratio<- nrow(t.t.train)/nrow(t.train) 
save(part, file="C:/Users/Nk/Documents/Uni/APA/TimeMachine/data_part_vectors/part.Rdata")

#Check distributions:
length(unique(tr$customerID))/nrow(tr) 
length(unique(t.train$customerID))/nrow(t.train)
