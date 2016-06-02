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

data.part<- createDataPartition(tr$returnQuantity, p = 0.6, list = F)
t.train<- tr[data.part,]
t.valid<- tr[-data.part,]

data.part2<- createDataPartition(t.train$returnQuantity, p = 0.6, list = F)
t.t.train<-t.train[data.part2,]
t.t.valid<-t.train[-data.part2,]   

tr$part3<- ifelse(!tr$unique_ID %in% t.train$unique_ID, "t.va", 
                  ifelse(tr$unique %in% t.t.train,"tt.tr","tt.va"))

part_v3<- tr[,c("unique_ID", "part3")]

load("C:/Users/Nk/Documents/Uni/APA/TimeMachine/data_part_vectors/part_2.Rdata")

part <- merge(part,part_v3, by="unique_ID")
View(part)
#prop.table(table(part$part_v1))
#prop.table(table(part$part_v2))

#t.t.ratio<- nrow(t.t.train)/nrow(t.train) 
save(part, file=paste0("C:/Users/Nk/Documents/Uni/APA/TimeMachine/data_part_vectors/part_full.Rdata"))


