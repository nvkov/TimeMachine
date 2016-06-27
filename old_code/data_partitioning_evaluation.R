#Data partitioning:
rm(list=ls())

# Find one time customers:
library("data.table")
library("ggplot2")
library("lattice")
library("vcd")


load("C:/Users/Nk/Documents/Uni/APA/data_2_7_full.RDa")

#Prepare dataset for assesment:
df<- df[!is.na(df$returnQuantity)]

relevantCols<- c("unique_ID", "customerID", "articleID", "orderID","orderDate", "returnBin", 
                 "returnQuantity", "quantity", "sizeCode", "colorCode", "rrp", "productGroup")
df<- df[ ,relevantCols, with=F]


#Cut the last 3 months for pseudo-classification:
class<- df[df$orderDate>="2015-07-01",]
train<- df[df$orderDate<="2015-07-01",]

#Find the structure of variables:
variable<- "rrp"
plot.distributions<- function(train, class, variable){

  ks.stat<- ks.test(unlist(train[, variable, with=F]), unlist(class[, variable, with=F]))
    
  par(oma=c(0,0,3,0), mfrow=c(1,2))
    class.d<- density(unlist(class[,variable, with=F]))
    train.d<- density(unlist(train[,variable, with=F]))
    plot(class.d, main="")
    lines(train.d, col="red")

    class.cdf<- ecdf(unlist(class[,variable, with=F]))
    train.cdf<- ecdf(unlist(train[,variable, with=F]))
    plot(class.cdf, main="")
    lines(train.cdf, col="red")
    
    
    
    op <- par(usr=c(0,1,0,1), # Reset the coordinates
              xpd=NA)         # Allow plotting outside the plot region
    legend(-0.5,1.15, # Find suitable coordinates by trial and error
           c("train", "class"), lty=1, lwd=3, col=c("black", "red"), box.col=NA)
    mtext(paste0("Compare distributions for ", variable, ks.stat$statistic), line=1, font=2, cex=1.2)
    text(paste0(ks.stat$statistic))
    }

plot.distributions(train, class, "rrp")

df$group<-ifelse(df$orderDate>="2015-07-01", "class", "train")

#==============================================================
variable<- "sizeCode"
plot.mosaic<- function(train, class, variable){
  
  
  par(oma=c(0,0,3,0), mfrow=c(1,2))
  tab<-xtabs(~ unlist(df[,variable, with=F])+ unlist(df[, "group", with=F]))
  mosaic(tab, xlab=variable, ylab="group")
  chisq.stat<- chisq.test(tab)
  
  op <- par(usr=c(0,1,0,1), # Reset the coordinates
            xpd=NA)         # Allow plotting outside the plot region
  legend(-0.5,1.15, # Find suitable coordinates by trial and error
         c("train", "class"), lty=1, lwd=3, col=c("black", "red"), box.col=NA)
  mtext(paste0("Compare distributions for ", variable, ks.stat$statistic), line=1, font=2, cex=1.2)
  text(paste0(ks.stat$statistic))
}

plot.mosaic(train, class, "sizeCode")


#====================================
#plot2<-function(train, class, variable){
#p1 <- ggplot(data = train, aes(x=variable))+
#  geom_density(fill="red", alpha=0.2) +
  # Change the fill colour to differentiate it
#  geom_density(data=class, fill="green", alpha=0.2) +
#  labs(title = "Distribution of income for 2010")+
#  labs(y="Density")+
#  labs(x="Household Income")
#return(p1)
#}

