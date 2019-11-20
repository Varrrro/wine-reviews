# Read data from CSV
wineData <- read.csv(file="../data/winemag-data-130k-v2.csv", header=TRUE, sep=",")

# View information about the dataset
dim(wineData)
names(wineData)
str(wineData)

# View summary
summary(wineData)

# Score histogram.
hist(wineData$points, main = "Evaluación", xlab = "Puntos", ylab = "Frecuencia")

# Price histogram
hist(wineData$price, main = "Precio", xlab = "Precio", ylab = "Frecuencia")

# Price histogram for wines under 100
wineDataPrice <- subset(wineData, price < 100)
hist(wineDataPrice$price, main = "Precio", xlab = "Precio", ylab = "Frecuencia")
