library("ggplot2")
library(reshape2)
library(stringr)
library('factoextra')
set.seed(20)

# Read data
wineData <- read.csv(file="../data/wine-reviews-trimmed.csv", header=TRUE, sep=",")

# Preprocess dataset
wineData <- na.omit(wineData)
wineData <- subset(wineData, price < 100)

# Select numeric columns
# points -> column 5
# price -> column 6
# year -> column 15
wineDataNumeric = wineData[c(5,6,15)]

# Normalization
for(j in 1:3){
  x=wineDataNumeric[,j];
  v=(x-mean(x))/sqrt(var(x));
  wineDataNumeric[,j]=v
}

# Optimal number of cluster
datos <- scale(wineDataNumeric)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")

kmeans.result=kmeans(wineDataNumeric,4)
kmeans.result
table(wineData$variety,kmeans.result$cluster)

#Analisis de bondad
plot(wineDataNumeric[c(1,2,3)], col=kmeans.result$cluster)
