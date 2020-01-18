library("ggplot2")
library(reshape2)
library(stringr)

wineData <- read.csv(file="../data/wine-reviews-trimmed.csv", header=TRUE, sep=",")

# See data
dim(wineData)
names(wineData)
str(wineData)

# View summary
summary(wineData)

hist(wineData$points, main = "Evaluación", xlab = "Puntos", ylab = "Frecuencia")
hist(wineData$year, main = "Año", xlab = "Año", ylab = "Frecuencia")
hist(wineData$price, main = "Precio", xlab = "Precio", ylab = "Frecuencia")

wineDataYear<-subset(wineData, year < 1900)


wineDataPrice<-subset(wineData, price < 150)
hist(wineDataPrice$price, main = "Precio", xlab = "Precio", ylab = "Frecuencia")

ggplot(wineDataPrice, aes(wineDataPrice$price, wineDataPrice$points, col = variety)) + 
  geom_point()

ggplot(wineDataPrice, aes(wineDataPrice$price, wineDataPrice$points)) + 
  geom_point() + geom_smooth(method="lm") 

ggplot(wineData, aes(wineData$year, wineData$price, col = variety)) + 
  geom_point()

ggplot(wineData, aes(wineData$year, wineData$points, col = variety)) + 
  geom_point()

# Create subsets for each class
pinotNoirWine <- subset(wineData, variety == "Pinot Noir")
chardonnayWine <- subset(wineData, variety == "Chardonnay")
cabernetWine <- subset(wineData, variety == "Cabernet Sauvignon")
redBlendWine <- subset(wineData, variety == "Red Blend")

ggplot(pinotNoirWine, aes(pinotNoirWine$year, pinotNoirWine$price)) + 
  geom_point()

ggplot(chardonnayWine, aes(chardonnayWine$year, chardonnayWine$price)) + 
  geom_point()

ggplot(cabernetWine, aes(cabernetWine$year, cabernetWine$price)) + 
  geom_point()

