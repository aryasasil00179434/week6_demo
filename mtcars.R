cars<-mtcars
str(cars)
View(cars)
#H0: weight is not affected by mpg-Null hypo
#H1:weight is affected by mpg-Alternate hypo
library(ggplot2)
attach(cars)
windows(16,10)
ggplot(cars,aes(x=))+geom_histogram()+theme_bw()
ggplot(cars,aes(x=mpg))+
  geom_histogram(breaks=seq(36,38,.2))+
  theme_bw()+
  labs(x="mpg",y="weight")+
  scale_y_continuous(breaks=seq(0,60,5))
histogram(~mpg | wt, 
          data = cars, 
          main = "Distribution of MPG activity data", 
          xlab = "MPG(Miles)", 
          ylab = "Weight lbs")
windows(16,10)
qqnorm(mpg)
# this line represents normal distribution
qqline(mpg, col = "red")


normality_test<-shapiro.test(cars$wt)
normality_test
normality_test1 <- shapiro.test(cars$mpg)
normality_test1

attach (cars)
wilcox.test(mpg~wt)
wilcox.test(beavers_data,mpg~wt)
windows(16,10)
pairs(cars, labels = colnames(cars), main = "cars dataset correlation plot")


summary(cars)
#Visual check for normal distribution
library(lattice)
attach(cars)


windows(16,10)
pairs.panels(cars,
             smooth = FALSE,      # If TRUE, draws less smooths
             scale = FALSE,      # If TRUE, scales the correlation text font
             density = TRUE,     # If TRUE, adds density plots and histograms
             ellipses = FALSE,    # If TRUE, draws ellipses
             method = "spearman",# Correlation method (also "pearson" or "kendall")
             pch = 21,           # pch symbol
             lm = FALSE,         # If TRUE, plots linear fit rather than the smoothed fit
             cor = TRUE,         # If TRUE, reports correlations
             jiggle = FALSE,     # If TRUE, data points are jittered
             factor = 2,         # Jittering factor
             hist.col = 4,       # Histograms color
             stars = TRUE,       # If TRUE, adds significance level with stars
             ci = TRUE)          # If TRUE, adds confidence interval

windows(16,10)
hist(cars$wt, main = "Histogram of Car Weight", xlab = "Car Weight (1000 lbs)", col = "skyblue")


attach(cars)
cor.test(wt,mpg,method="pearson")




#extra
#