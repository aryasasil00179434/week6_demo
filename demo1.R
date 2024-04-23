?beavers
str(beaver2)
View(beaver2)
is.factor(beaver2)
#H0:Body temperature is not affected by activity
#H1:Body temperature is affected by activity
beavers_data<-beaver2
beavers_data
beavers_data$activ<-factor(beavers_data$activ,labels=c("No","Yes"))
is.factor(beavers_data$activ)
str(beavers_data)
#independent variable->active(we can control activ but not temp)dependent variable=temp
install.packages("ggplot2")
library(ggplot2)
windows(16,10)
ggplot(beavers_data,aes(x=temp))+geom_histogram()+theme_bw()
ggplot(beavers_data,aes(x=temp))+
  geom_histogram(breaks=seq(36,38,.2))+
  theme_bw()+
  labs(x="temp",y="Activity")+
  scale_y_continuous(breaks=seq(0,60,5))
#another method to plot histogram
install.packages("lattice")
library(lattice)
windows(20,10)
attach(beavers_data)
histogram(~temp | activ,
          data=beavers_data,
          main="Distribution of beaver activity data",
          xlab="Activity%")
detach(beavers_data)
attach(beavers_data)
windows(20,10)
qqnorm(temp)
qqline(temp,col="red")

opar<-par(no.readonly = TRUE)
windows(20,10)
par(mfrow=c(1,2))

with(beavers_data,{
  qqnorm(temp[activ=="Yes"],
         main="Beavers Active Data")
  qqline(temp[activ=="Yes"])
})
with(beavers_data,{
  qqnorm(temp[activ=="No"],
         main="Beavers InActive Data")
  qqline(temp[activ=="No"])
})

normality_test<-shapiro.test(beavers_data$temp)
normality_test
