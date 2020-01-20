library("ggplot2")
library(reshape2)
library(stringr)
library(e1071)

# Read data
wineData <- read.csv(file="../data/wine-reviews-trimmed.csv", header=TRUE, sep=",")
wineData <- na.omit(wineData)


# Plot data
plot(points ~ price, data = wineData)
with(wineData, lines(loess.smooth(price, points), col = "red"))


# Divide graph area in 2 columns
par(mfrow=c(1, 2))  


# Box plot for 'points'
boxplot(wineData$points, main="Points", sub=paste("Outlier rows: ", boxplot.stats(wineData$points)$out))  
# Box plot for 'price'
boxplot(wineData$price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(wineData$price)$out))  


# There are too many outliers in price, so we decide to remove 
# rows with price higher than 100
wineData <- subset(wineData, price < 100)


# Again box plot
# Box plot for 'points'
boxplot(wineData$points, main="Points", sub=paste("Outlier rows: ", boxplot.stats(wineData$points)$out))  
# Box plot for 'price'
boxplot(wineData$price, main="Price", sub=paste("Outlier rows: ", boxplot.stats(wineData$price)$out))  


# Plot density points
plot(density(wineData$points), main="Density Plot: Points", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(wineData$points), 2)))
polygon(density(wineData$points), col="red")
# Plot density price
plot(density(wineData$price), main="Density Plot: Price", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(wineData$price), 2))) 
polygon(density(wineData$price), col="red")


# Calculate correlation between price and points
cor(wineData$points, wineData$price, method = "spearman")


# Get linear regression model
regr <- lm(points ~ price, data=wineData)
print(regr)
summary(regr)
  
dev.off() # Clear graphics

# Linear regression visualization
# Simple plot
plot(wineData$price,wineData$points,xlab="Price",ylab="Points",col=2,main="Regresion Lineal 1 variable")
abline(regr,col=4)
# Beauty plot
ggplot(wineData, aes(wineData$price, wineData$points)) + geom_point() + geom_smooth(method="lm") 


# Separate dataset in train and test
trainingRowIndex <- sample(1:nrow(wineData), 0.8*nrow(wineData)) 
trainingData <- wineData[trainingRowIndex, ]  # model training data
testData  <- wineData[-trainingRowIndex, ]   # test data

# Build the model with training dataset
lmMod <- lm(points ~ price, data=trainingData) 
summary(lmMod) 


# Predict points using trained model
pointsPred <- predict(lmMod, testData)
cor(testData$points,pointsPred)

# Plot real and pred values
plot(testData$points,pointsPred,xlab="Real Points",ylab="Pred Points",col=2)
abline(a=0, b=1)

