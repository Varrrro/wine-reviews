wineData <- read.csv(file="../data/wine-reviews-trimmed.csv", header=TRUE, sep=",")

# See data
dim(wineData)
names(wineData)
str(wineData)

# View summary
summary(wineData)

hist(wineData$points, main = "Evaluación", xlab = "Puntos", ylab = "Frecuencia")
hist(wineData$price, main = "Precio", xlab = "Precio", ylab = "Frecuencia")

wineDataPrice<-subset(wineData, price < 200)
hist(wineDataPrice$price, main = "Precio", xlab = "Precio", ylab = "Frecuencia")


tab = table(wineData$variety)
pie(tab, labels = names("Pinot Noir"))
barplot(tab)