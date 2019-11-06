# Read CSV into R
wineData <- read.csv(file="../data/winemag-data-130k-v2.csv", header=TRUE, sep=",")

pinotNoirWine <- subset(wineData, variety == "Pinot Noir")
chardonnayWine <- subset(wineData, variety == "Chardonnay")
cabernetWine <- subset(wineData, variety == "Cabernet Sauvignon")
redBlendWine <- subset(wineData, variety == "Red Blend")

pinotNoirWine <- na.omit(pinotNoirWine)
chardonnayWine <- na.omit(chardonnayWine)
cabernetWine <- na.omit(cabernetWine)
redBlendWine <- na.omit(redBlendWine)

pinotNoirWine <- pinotNoirWine[sample(nrow(pinotNoirWine), 1000), ]
chardonnayWine <- chardonnayWine[sample(nrow(chardonnayWine), 1000), ]
cabernetWine <- cabernetWine[sample(nrow(cabernetWine), 1000), ]
redBlendWine <- redBlendWine[sample(nrow(redBlendWine), 1000), ]

trimmedData <- do.call("rbind", list(pinotNoirWine, chardonnayWine, cabernetWine, redBlendWine))
