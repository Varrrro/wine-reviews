library("ggplot2")
library(reshape2)
library(stringr)

# Read data
wineData <- read.csv(file="../data/wine-reviews-trimmed.csv", header=TRUE, sep=",")
wineData <- na.omit(wineData)
wineData <- subset(wineData, price < 100)

scatter.smooth(x=wineData$points, y=wineData$price, main="price ~ points") 

# divide graph area in 2 columns
par(mfrow=c(1, 2))  
# box plot for 'points'
boxplot(wineData$points, main="Points", sub=paste("Outlier rows: ", boxplot.stats(wineData$points)$out))  
# box plot for 'price'
boxplot(wineData$price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(wineData$price)$out))  


library(e1071)
par(mfrow=c(1, 2))  # divide graph area in 2 columns
plot(density(wineData$points), main="Density Plot: Points", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(wineData$points), 2)))
polygon(density(wineData$points), col="red")
plot(density(wineData$price), main="Density Plot: Price", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(wineData$price), 2))) 
polygon(density(wineData$price), col="red")

cor(wineData$points, wineData$price)
cor(wineData$points, wineData$year)
cor(wineData$year, wineData$price)

regr <- lm(points ~ price, data=wineData)
print(regr)
summary(regr)

dev.off()

plot(wineData$price,wineData$points,xlab="Price",ylab="Points",col=2,main="Regresion Lineal 1 variable")
abline(regr,col=4)

