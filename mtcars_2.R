cars<-mtcars
str(cars)
#Transmission=independent Variable
#hp=dependent variable
cars$am<-factor(cars$am,labels=c("Manual","Automatic"))
is.factor(cars$am)

normality_test<-shapiro.test(cars$am)
normality_test
normality_test1 <- shapiro.test(cars$hp)
normality_test1
# Create box plot
library(lattice)
windows(16,10)
histogram(~am | hp, 
          data = cars, 
          main = "Distribution of cars activity data", 
          xlab = "transmission", 
          ylab = "no of cars")
windows(16,10)
opar <- par(no.readonly = TRUE)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))
with(cars, {
  qqnorm(hp[am == "Manual"], 
         main = "Manual Transmission")
  qqline(hp[am == "Manual"])
})

with(cars, {
  qqnorm(hp[am == "Automatic"], 
         main = "Automatic Transmission")
  qqline(hp[am == "Automatic"])
})
tapply(hp,am,shapiro.test)
shapiro.test(cars$hp)

attach(cars)
wilcox.test(hp~am)
