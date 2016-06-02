rm(list=ls())
library(caret)
library(dplyr)
set.seed(345)
#### Meta-parameters ####################################

#### Load data ##########################################


load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

#Prepare dataset for assesment:
tr<- df[!is.na(df$returnQuantity)]

#df<- df[!is.na(df$returnQuantity)]

### Splitting data in training (tr)  and classification (te) sets:
#Cut the last 3 months for pseudo-classification:
#te<- df[df$orderDate>="2015-07-01",]
#tr<- df[df$orderDate<="2015-07-01",]
#df$group<-ifelse(df$orderDate>="2015-07-01", "class", "train")

dim(tr)
View(tr)

# Keep only training set for data partininig
rm(te, df)

#Transorm data.table to data.frame to use partitioning function with dplyr:
tr<- data.frame(tr)

##########  PARTITIONING IDEA with REturning Customers $ Chronological Order (by Marta)
data_partitioning_v2<- function(tr){
  
  tr <- tr %>% group_by(customerID) %>% mutate(totOrder_tn = max(cumsumOrder))
  
  #x <-as.data.frame(t(table(tr$totOrder_tn)))[,c(2,3)]
  #xp <-as.data.frame(t(round(prop.table(table(tr$totOrder_tn)),5)))[,c(2,3)]
  #xn <- table(tr$totOrder_tn)
  #options(scipen = 999)
  #barplot(xn, main="Returning Customers Distribution", xlab="Number of Orders")
  
  #table(ifelse(tr$totOrder_tn==1,"1", ">1"))
  #     >1       1 
  #  1791331  533834
  
  #table(ifelse(tr$totOrder_tn==1,"1", ifelse(tr$totOrder_tn>=2 & tr$totOrder_tn<=4 , "2-4", ">4")))
  #>4       1     2-4 
  #101.5732  53.3834  775.599
  
  #round(prop.table(table(ifelse(tr$totOrder_tn==1,"1", ">1"))),2)
  #>1    1 
  #0.77 0.23 
  
  #round(prop.table(table(ifelse(tr$totOrder_tn==1,"1", ifelse(tr$totOrder_tn>=2 & tr$totOrder_tn<=4 , "2-4", ">4")))),3)
  #>4     1   2-4 
  #0.437 0.230 0.334 
  
  
  #Step 1. Split randomly newcommers (customers with 1 order) - 23% of TRAIN set
  tn1 <- tr[tr$totOrder_tn==1,]
  
  trainIndex1 <- createDataPartition(tn1$returnBin, p = .6, list = FALSE, times = 1)
  train1 <- tn1[ trainIndex1,]
  test1  <- tn1[-trainIndex1,]
  
  
  #Step 2. For Returning Customers with >=2 orders - 77% of TRAIN set
  tn2 <- tr[tr$totOrder_tn>=2,]
  #tn2 <- tr[tr$totOrder_tn>=2 & tr$totOrder_tn<=4,]
  
  #Step 3. Choose randomly 35% of 77% Retuning Customers for which the whole Customer history will appear either in Train or Test (without Split)
  x2 <- sample(unique(tn2$customerID),floor(length(unique(tn2$customerID))*0.35))
  
  tn3 <- tn2[tn2$customerID %in% x2,] 
  
  # Create indicator if Customer has return ratio >= 50%
  tn3 <-tn3 %>% group_by(customerID)  %>% mutate(return_ratio = round(sum(returnQuantity)/sum(quantity)))
  #,sum_ret= sum(returnQuantity),
  #sum_quant = sum(quantity))
  #Dataframe on the customer level with calculated return_ratio
  tn3c <- summarise(tn3, return_ratio = max(return_ratio) )
  #Sanity check
  #tn3[tn3$customerID=="000002", c("return_ratio", "orderID", "quantity", "returnQuantity"#, "sum_ret", "sum_quant")]
  
  #tn3c[tn3c$customerID=="000002",]
  # Partition by CustomerID with respect to the return_ratio in order to have stable samples
  trainIndex3 <- createDataPartition(tn3c$return_ratio, p = .6, list = FALSE, times = 1)
  train3c <- tn3c[ trainIndex3,]
  #test3c  <- tn3c[-trainIndex3,]
  train3 <- tn3[tn3$customerID %in%train3c$customerID,]                                         
  test3 <- tn3[!(tn3$customerID %in%train3c$customerID),]  
  
  #Step 4. Split sequence of ORDERS for remaining 65% of Retuning Customers (>= 2 orders) in 60%/40% ratio
  # // LAter adjust to random split //
  tn4 <- tn2[!(tn2$customerID %in% x2),]
  
  #Sanity check
  #nrow(tn4) +nrow(tn3)
  #nrow(tn2) + nrow(tn1)
  #nrow(tr)
  
  tn4$split_num  <- floor(tn4$totOrder_tn*0.68)
  
  train4 <- tn4[tn4$cumsumOrder <= tn4$split_num,]
  test4 <- tn4[!(tn4$cumsumOrder <= tn4$split_num),]
  
  #Sanity check
  #nrow(train4)
  #nrow(test4)/(nrow(test4)+nrow(train4))  #with ratio 0.68 the test/train ratio is more less 40/60
  
  test <- bind_rows(test1, test3, test4)
  train <- bind_rows(train1, train3, train4)
  
  #names(test)
  test <- test[,!names(test) %in% c("random_split", "split_num", "sum_quant", "sum_ret", "return_ratio")]
  train <- train[,!names(train) %in% c("random_split", "split_num", "sum_quant", "sum_ret", "return_ratio")]
  
  #ls()
  rm("train3c", "tn3c", "test1", "test3", "test4", "tn1", "tn2", "tn3", "tn4", "train1", "train3", "train4", "trainIndex1", "trainIndex3")  
  
  test$totOrder_tn <- NULL
  train$totOrder_tn <- NULL
  
  #dim(test)
  #sanity check
  #nrow(test)/ (nrow(test)+nrow(train))  #~around 40%
  
  ######################################################
  ######################################################
  return(train)
}


t.trainID<- data_partitioning_v2(tr)

t.train<-tr[tr$unique_ID %in% t.trainID$unique_ID,] 
t.valid<- tr[!tr$unique_ID %in% t.trainID$unique_ID,]

#ratio<- nrow(t.train)/nrow(tr)


#Second level partitioning:

t.t.trainID<- data_partitioning_v2(t.train)
t.t.train<-t.train[t.train$unique_ID %in% t.t.trainID$unique_ID,]
t.t.valid<-t.train[!t.train$unique_ID %in% t.t.trainID$unique_ID,]  

t.train$part_v2<- rep("t.tr", nrow(t.train))
t.valid$part_v2<- rep("t.va", nrow(t.valid))
t.t.train$part_v2<- rep("tt.tr", nrow(t.t.train))
t.t.valid$part_v2<- rep("tt.va", nrow(t.t.valid))

load("C:/Users/Nk/Documents/Uni/APA/TimeMachine/data_part_vectors/part.Rdata")

part2<- rbind(t.valid[, c("unique_ID", "part_v2")], t.t.train[, c("unique_ID", "part_v2")], t.t.valid[, c("unique_ID", "part_v2")]) 

part <- merge(part,part2, by="unique_ID")
View(part)
#prop.table(table(part$part_v1))
#prop.table(table(part$part_v2))

#t.t.ratio<- nrow(t.t.train)/nrow(t.train) 
save(part, file=paste0("C:/Users/Nk/Documents/Uni/APA/TimeMachine/data_part_vectors/part_2.Rdata"))

#Check distributions:
#length(unique(tr$customerID))/nrow(tr) 
#length(unique(t.train$customerID))/nrow(t.train)
