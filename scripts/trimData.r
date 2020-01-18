library(stringr)
library(tm)
library(qdap)

# Read CSV into R
wineData <- read.csv(file="../data/winemag-data-130k-v2.csv", header=TRUE, sep=",")

# Create subsets for each class
pinotNoirWine <- subset(wineData, variety == "Pinot Noir")
chardonnayWine <- subset(wineData, variety == "Chardonnay")
cabernetWine <- subset(wineData, variety == "Cabernet Sauvignon")
redBlendWine <- subset(wineData, variety == "Red Blend")

# Remove null rows
pinotNoirWine <- na.omit(pinotNoirWine)
chardonnayWine <- na.omit(chardonnayWine)
cabernetWine <- na.omit(cabernetWine)
redBlendWine <- na.omit(redBlendWine)

# Keep only 1000 rows of each subset
pinotNoirWine <- pinotNoirWine[sample(nrow(pinotNoirWine), 1000), ]
chardonnayWine <- chardonnayWine[sample(nrow(chardonnayWine), 1000), ]
cabernetWine <- cabernetWine[sample(nrow(cabernetWine), 1000), ]
redBlendWine <- redBlendWine[sample(nrow(redBlendWine), 1000), ]

# Join the four subsets
trimmedData <- do.call("rbind", list(pinotNoirWine, chardonnayWine, cabernetWine, redBlendWine))

# Remove X column 
trimmedData <- subset( trimmedData, select = -X )

# Extract year from title
trimmedData$year = str_extract(trimmedData[,12], "[0-2][0,1,9][0-9][0-9]")

# TEXT MINING
data_description <- as.vector(trimmedData$description)
description_source <- VectorSource(data_description)
description_corpus <- Corpus(description_source)

frequent_terms <- freq_terms(data_description, 30)
plot(frequent_terms)

# Write data to CSV
write.csv(trimmedData,"../data/wine-reviews-trimmed.csv", row.names=FALSE)

