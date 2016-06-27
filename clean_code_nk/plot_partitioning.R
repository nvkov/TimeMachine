
#Plot different partitionings for comparison:

############################################################
#Plot pre-partition generated vars:

#-----------------------------------------------------------
#Plot validation set:

plot.numerical.validation<- function(variable, df){
  
  #Calculate K-S.-Distance and test statistic: 
  ks.stat_p1<- ks.test(unlist(df[part_v1=="class", variable, with=F]), unlist(df[part_v1=="t.va", variable, with=F]))
  ks.stat_p2<- ks.test(unlist(df[part_v2=="class", variable, with=F]), unlist(df[part_v2=="t.va", variable, with=F]))
  ks.stat_p3<- ks.test(unlist(df[part3=="class", variable, with=F]), unlist(df[part3=="t.va", variable, with=F]))
  
  #Save results:
  png(filename=paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/Partitioning/Valid/Pre/distributions_", variable, ".png"))
  
  #Plot distributions for all partitions and the unlabeled data:
  
  #Partition v1:
  d1 <- density(unlist(df[part_v1=="t.va", variable, with=F])) 
  plot(d1, col="blue", main = paste0("Distribution for ", variable))
  
  legend("topright", inset = .05, title = "partitioning",
         legend= c("v1", "v2", "v3", "unlabeled")
         ,fill = c("blue", "red", "green", "black"), horiz=F)
  
  legend("bottomrigh", inset = .05, title = "KS-Dist/p-value)",
         legend= c(paste0(round(ks.stat_p1$statistic,3), "/" , ks.stat_p1$p.value), 
                   paste0(round(ks.stat_p2$statistic,3), "/", ks.stat_p2$p.value),
                   paste0(round(ks.stat_p3$statistic,3), "/", ks.stat_p3$p.value)),
         fill = c("blue", "red", "green"), horiz=F)
  
  #Partition v2:      
  d2<- density(x=unlist(df[part_v2=="t.va", variable, with=F]))
  lines(d2, col="red") 
  
  #Partition v3:    
  d3<- density(x=unlist(df[part3=="t.va", variable, with=F]))
  lines(d3, col="green") 
  
  #Partition v4:  
  d4<- density(x=unlist(df[part_v4=="class", variable, with=F]))
  lines(d4, col="black")
  
  dev.off()
  #return(p1)
}

#########################################################

#Plot pre-partition train:

plot.numerical.train<- function(variable, df){
  
  #Calculate K-S.-Distance and test statistic: 
  ks.stat_p1<- ks.test(unlist(df[part_v1=="train", variable, with=F]), unlist(df[part_v1=="tt.tr", variable, with=F]))
  ks.stat_p2<- ks.test(unlist(df[part_v2=="train", variable, with=F]), unlist(df[part_v2=="tt.tr", variable, with=F]))
  ks.stat_p3<- ks.test(unlist(df[part3=="train", variable, with=F]), unlist(df[part3=="tt.tr", variable, with=F]))
  
  #Save results:
  png(filename=paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/Partitioning/Train/Pre/distributions_train_", variable, ".png"))
  
  #Plot distributions for all partitions and the unlabeled data:
  
  #Partition v1:
  d1 <- density(unlist(df[part_v1=="tt.tr", variable, with=F])) 
  plot(d1, col="blue", main = paste0("Distribution for ", variable)) 
  legend("topright", inset = .05, title = "partitioning",
         legend= c("v1", "v2", "v3", "labeled")
         ,fill = c("blue", "red", "green", "black"), horiz=F)
  
  legend("bottomrigh", inset = .05, title = "KS-Dist/p-value)",
         legend= c(paste0(round(ks.stat_p1$statistic,3), "/" , ks.stat_p1$p.value), 
                   paste0(round(ks.stat_p2$statistic,3), "/", ks.stat_p2$p.value),
                   paste0(round(ks.stat_p3$statistic,3), "/", ks.stat_p3$p.value)),
         fill = c("blue", "red", "green"), horiz=F)
  
  #Partition v2:      
  d2<- density(x=unlist(df[part_v2=="tt.tr", variable, with=F]))
  lines(d2, col="red") 
  
  #Partition v3:    
  d3<- density(x=unlist(df[part3=="tt.tr", variable, with=F]))
  lines(d3, col="green") 
  
  #Partition v4:  
  d4<- density(x=unlist(df[part_v4=="train", variable, with=F]))
  lines(d4, col="black")
  
  dev.off()
  #return(p1)
}
#####################################################################
#Plot post-partition generated vars validation:

plot.returns.validation<- function(variable){
  
  #Calculate K-S.-Distance and test statistic: 
  ks.stat_p1<- ks.test(unlist(d.valid.p4[, variable, with=F]), unlist(d.valid.p1[, variable, with=F]))
  ks.stat_p2<- ks.test(unlist(d.valid.p4[, variable, with=F]), unlist(d.valid.p2[, variable, with=F]))
  ks.stat_p3<- ks.test(unlist(d.valid.p4[, variable, with=F]), unlist(d.valid.p3[, variable, with=F]))
  
  #Save results:
  png(filename=paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/Partitioning/Valid/Post/distributions_", variable, ".png"))
  
  #Plot distributions for all partitions and the unlabeled data:
  
  #Partition v1:
  d1 <- density(unlist(d.valid.p1[,variable, with=F])) 
  plot(d1, col="blue", main = paste0("Distribution for ", variable))
  
  legend("topright", inset = .05, title = "partitioning",
         legend= c("v1", "v2", "v3", "unlabeled")
         ,fill = c("blue", "red", "green", "black"), horiz=F)
  
  legend("bottomrigh", inset = .05, title = "KS-Dist/p-value)",
         legend= c(paste0(round(ks.stat_p1$statistic,3), "/" , ks.stat_p1$p.value), 
                   paste0(round(ks.stat_p2$statistic,3), "/", ks.stat_p2$p.value),
                   paste0(round(ks.stat_p3$statistic,3), "/", ks.stat_p3$p.value)),
         fill = c("blue", "red", "green"), horiz=F)
  
  #Partition v2:      
  d2<- density(x=unlist(d.valid.p2[, variable, with=F]))
  lines(d2, col="red") 
  
  #Partition v3:    
  d3<- density(x=unlist(d.valid.p3[, variable, with=F]))
  lines(d3, col="green") 
  
  #Partition v4:  
  d4<- density(x=unlist(d.valid.p4[, variable, with=F]))
  lines(d4, col="black")
  
  dev.off()
  #return(p1)
}


#########################################################
#Plot post-partition generated vars train:

plot.returns.train<- function(variable){
  
  #Calculate K-S.-Distance and test statistic: 
  ks.stat_p1<- ks.test(unlist(d.train.p4[, variable, with=F]), unlist(d.train.p1[, variable, with=F]))
  ks.stat_p2<- ks.test(unlist(d.train.p4[, variable, with=F]), unlist(d.train.p2[, variable, with=F]))
  ks.stat_p3<- ks.test(unlist(d.train.p4[, variable, with=F]), unlist(d.train.p3[, variable, with=F]))
  
  #Save results:
  png(filename=paste0("C:/Users/Nk/Documents/Uni/APA/Graphs/Partitioning/Train/Post/distributions_", variable, ".png"))
  
  #Plot distributions for all partitions and the unlabeled data:
  
  #Partition v1:
  d1 <- density(unlist(d.train.p1[,variable, with=F])) 
  plot(d1, col="blue", main = paste0("Distribution for ", variable))
  
  legend("topright", inset = .05, title = "partitioning",
         legend= c("v1", "v2", "v3", "unlabeled")
         ,fill = c("blue", "red", "green", "black"), horiz=F)
  
  legend("bottomrigh", inset = .05, title = "KS-Dist/p-value)",
         legend= c(paste0(round(ks.stat_p1$statistic,3), "/" , ks.stat_p1$p.value), 
                   paste0(round(ks.stat_p2$statistic,3), "/", ks.stat_p2$p.value),
                   paste0(round(ks.stat_p3$statistic,3), "/", ks.stat_p3$p.value)),
         fill = c("blue", "red", "green"), horiz=F)
  
  #Partition v2:      
  d2<- density(x=unlist(d.train.p2[, variable, with=F]))
  lines(d2, col="red") 
  
  #Partition v3:    
  d3<- density(x=unlist(d.train.p3[, variable, with=F]))
  lines(d3, col="green") 
  
  #Partition v4:  
  d4<- density(x=unlist(d.train.p4[, variable, with=F]))
  lines(d4, col="black")
  
  dev.off()
  #return(p1)
}


#########################################################
#plot.numerical.train("quantity", df)
