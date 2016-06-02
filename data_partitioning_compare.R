#Data partitioning:
rm(list=ls())

# Find one time customers:
library("data.table")
library("ggplot2")
library("lattice")
library("vcd")
library("gridExtra")


load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")
load("C:/Users/Nk/Documents/Uni/APA/TimeMachine/data_part_vectors/part_full.RData")

part<- data.table(part)
setkey(df, "unique_ID")
setkey(part, "unique_ID")

df<- merge(df, part, by="unique_ID", all=T)

df$part_v1[is.na(df$returnQuantity)]<- "class"
df$part_v2[is.na(df$returnQuantity)]<- "class"
df$part3[is.na(df$returnQuantity)]<- "class"

#Modify for one level:
df$part_v1[df$part_v1=="tt.va"]<- "tt.tr"
df$part_v2[df$part_v2=="tt.va"]<- "tt.tr"
df$part3[df$part3=="tt.va"]<- "tt.tr"
df$part_v4<-ifelse(is.na(df$returnQuantity), "class", ifelse(df$orderDate<"2015-07-01", "train", "validation"))
rm(part)

variable<- "rrp"

plot.numerical<- function(df, variable){
  ks.stat_p1<- ks.test(unlist(df[part_v1=="class", variable, with=F]), unlist(df[part_v1=="t.va", variable, with=F]))
  ks.stat_p2<- ks.test(unlist(df[part_v2=="class", variable, with=F]), unlist(df[part_v2=="t.va", variable, with=F]))
  ks.stat_p3<- ks.test(unlist(df[part3=="class", variable, with=F]), unlist(df[part3=="t.va", variable, with=F]))
    p1<- ggplot() +
      geom_density(data=df[part_v1=="t.va"],
                   aes(x=unlist(df[part_v1=="t.va", variable, with=F])) ,
                       color="blue") +
      
      geom_density(data=df[part_v2=="t.va"],
                   aes(x=unlist(df[part_v2=="t.va", variable, with=F])) ,
                   color="red") +
      
      geom_density(data=df[part3=="t.va"],
                   aes(x=unlist(df[part3=="t.va", variable, with=F])) ,
                   color="green") +
    
      geom_density(data=df[part_v4=="class"],
                 aes(x=unlist(df[part_v4=="class", variable, with=F])) ,
                 color="black") +
      geom_text(aes(hjust = 0,vjust=1, label=paste0("KS-Stat: ", round(ks.stat_p1$statistic, 3), " and p-value: ", ks.stat_p1$p.value, " (part1)"))) +
      geom_text(aes(400, .05, label=paste0("KS-Stat: ", round(ks.stat_p2$statistic, 3), " and p-value: ", ks.stat_p2$p.value, " (part2)"))) +
      geom_text(aes(400, .04, label=paste0("KS-Stat: ", round(ks.stat_p3$statistic, 3), " and p-value: ", ks.stat_p3$p.value, " (part3)")))
  return(p1)}
    
plot.numerical(df, "rrp")
    #annotate(geom="text", x=400, y=0.05, label=paste0("KS-Stat:", ks.stat$statistic), colour="black",
    #         size=5, family="Courier", fontface="bold", angle=0) +
    scale_colour_hue(guide = "none")
  
  
  p2<- ggplot(df, aes(x=unlist(df[, variable, with=F]))) + 
    stat_ecdf(aes(group=unlist(df[,"part3", with=F]), color=unlist(df[,"part3", with=F])), geom = "point")+
    scale_fill_discrete(name="Experimental\nCondition")
  
  grid.arrange(p1, p2, ncol=2)
}

plot.numerical(df, "rrp")

#========================================================
#Apply functions:
#Find factor vars:
factor.vars<- names(Filter(is.factor, df))

#Find numerical vars:
numeric.vars<- names(Filter(is.numeric, df))

plot.numerical(df, "quantity")
