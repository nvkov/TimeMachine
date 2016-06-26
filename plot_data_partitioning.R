#Data partitioning:
rm(list=ls())

# Find one time customers:
library("data.table")
library("ggplot2")
library("lattice")
library("vcd")
library("gridExtra")


load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

#Prepare dataset for assesment:

#Cut the last 3 months for pseudo-classification:
#class<- df[df$orderDate>="2015-07-01",]
#train<- df[df$orderDate<="2015-07-01",]
#df$group<-ifelse(df$orderDate>="2015-07-01", "class", "train")
df$group<-ifelse(df$orderDate>="2015-10-01", "class", "train")


#Find the structure of variables:
#==========================================================
variable<- "rrp"

plot.numerical<- function(df, variable){
ks.stat<- ks.test(unlist(df[group=="train", variable, with=F]), unlist(df[group=="class", variable, with=F]))

p1<- ggplot(df, aes(x=unlist(df[, variable, with=F]))) + 
  geom_density(aes(group=unlist(df[,"group", with=F]), color=unlist(df[,"group", with=F]))) +
  annotate(geom="text", x=400, y=0.05, label=paste0("KS-Stat:", ks.stat$statistic), colour="black",
          size=5, family="Courier", fontface="bold", angle=0) +
  scale_colour_hue(guide = "none")


p2<- ggplot(df, aes(x=unlist(df[, variable, with=F]))) + 
  stat_ecdf(aes(group=unlist(df[,"group", with=F]), color=unlist(df[,"group", with=F])), geom = "point")+
  scale_fill_discrete(name="Experimental\nCondition")

grid.arrange(p1, p2, ncol=2)
}

plot.numerical(df, "rrp")

#==============================================================
variable<- "sizeCode"
plot.categorical<- function(train, class, variable){
  
  
  par(oma=c(0,0,3,0), mfrow=c(1,2))
  tab<-xtabs(~ unlist(df[,variable, with=F])+ unlist(df[, "group", with=F]))
  mosaic(tab, xlab=variable, ylab="group",shade=T)
  chisq.stat<- chisq.test(tab)
  
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region
  legend(-0.5,1.15, # Find suitable coordinates by trial and error
         c("train", "class"), lty=1, lwd=3, col=c("black", "red"), box.col=NA)
  mtext(paste0("Compare distributions for ", variable, chisq.stat), line=1, font=2, cex=1.2)
  text(paste0(ks.stat$statistic))
}

plot.categorical(train, class, "sizeCode")

#========================================================
#Apply functions:
#Find factor vars:
factor.vars<- names(Filter(is.factor, df))

#Find numerical vars:
numeric.vars<- names(Filter(is.numeric, df))

plot.numerical(df, "quantity")



