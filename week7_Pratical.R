cars<-mtcars
str(cars)
View(cars)
#gear-independent variable
#hp-dependent variable

cars$gear<-factor(cars$gear)
is.factor(cars$gear)

#H0: no correlation
# H1: correlation
normality_test<-shapiro.test(cars$hp)
normality_test


# Create box plot
library(lattice)
windows(16,10)
histogram(~hp | gear, 
          data = cars, 
          main = "Distribution of cars activity data", 
          xlab = "gears", 
          ylab = "no of cars")
windows(16,10)
opar <- par(no.readonly = TRUE)
# arrange plots in 1 rows and 2 column
par(mfrow = c(1, 2))
with(cars, {
  qqnorm(hp[gear ==3], 
         main =3)
  qqline(hp[gear == 3])
})

with(cars, {
  qqnorm(hp[gear == 4], 
         main = 4)
  qqline(hp[gear == 4])
})
with(cars, {
  qqnorm(hp[gear ==5], 
         main =5)
  qqline(hp[gear == 5])
})

attach(cars)
tapply(hp,gear,shapiro.test)
 kruskal.test(hp~gear)


 
 
 
#vs-shape of the engine-dependent
#gear-No of gearbox-independent
library(lattice)
windows(16,10)
histogram(~vs | gear, 
          data = cars, 
          main = "Distribution of cars activity data", 
          xlab = "gears", 
          ylab = "shape")
tapply(vs,gear,shapiro.test)
shapiro.test(cars$vs)
attach(cars)
chisq.test(vs,gear)
