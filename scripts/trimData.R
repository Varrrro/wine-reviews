library(stringr)
library(tm)
library(qdap)
library(wordcloud)
library(SnowballC)
library(data.table)

# Read CSV into R
wineData <- read.csv(file = "../data/winemag-data-130k-v2.csv", header = TRUE, sep  = ",")

# Extract year from title
wineData$year = str_extract(wineData[,12], "[0-2][0,1,9][0-9][0-9]")

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
pinotNoirWine <- pinotNoirWine[sample(nrow(pinotNoirWine), 1000),]
chardonnayWine <- chardonnayWine[sample(nrow(chardonnayWine), 1000),]
cabernetWine <- cabernetWine[sample(nrow(cabernetWine), 1000),]
redBlendWine <- redBlendWine[sample(nrow(redBlendWine), 1000),]

# Join the four subsets
trimmedData <- do.call("rbind", list(pinotNoirWine, chardonnayWine, cabernetWine, redBlendWine))

# Write to CSV
write.csv(trimmedData, "../data/wine-reviews-trimmed.csv", row.names = FALSE)

# ---------------------TEXT MINING --------------------- #

# Extract year from title
trimmedData$year = str_extract(trimmedData[,12], "[0-2][0,1,9][0-9][0-9]")

data_description <- as.vector(trimmedData$description)
description_source <- VectorSource(data_description)
description_corpus <- Corpus(description_source)

frequent_terms <- freq_terms(data_description, 50)
plot(frequent_terms)

# Replace contraction
description_corpus <- tm_map(description_corpus, replace_contraction)
# Convert to lower case
description_corpus <- tm_map(description_corpus, content_transformer(tolower))
# Remove punctuation marks in text
description_corpus <- tm_map(description_corpus, removePunctuation)
# Replace numbers with words
description_corpus <- tm_map(description_corpus, content_transformer(replace_number))
# Replace abbreviation
description_corpus <- tm_map(description_corpus, content_transformer(replace_abbreviation))
# Replace symbol
description_corpus <- tm_map(description_corpus, content_transformer(replace_symbol))
# Remove most common English words
description_corpus <- tm_map(description_corpus, removeWords, stopwords("en"))
# Remove excess whitespace
description_corpus <- tm_map(description_corpus, stripWhitespace)
# Stemming
description_corpus <- tm_map(description_corpus,  stemDocument)

frequent_terms <- freq_terms(description_corpus, 20)
plot(frequent_terms)

# Create the dtm from the corpus
description_dtm <- DocumentTermMatrix(description_corpus)
# Print data
description_dtm
 
# Convert description_dtm to a matrix
description_m <- as.matrix(description_dtm)
description_m[1:18, 1:12]

# Remove low frequency terms
description_dtm_rm_sparse <- removeSparseTerms(description_dtm, sparse=0.999)
# Print data
description_dtm_rm_sparse

# Convert description_dtm_rm_sparse to a matrix
description_m <- as.matrix(description_dtm_rm_sparse)
# Print a portion of the matrix
description_m[1:18, 1:12]


colnames(description_m) <- make.names(colnames(description_m))

freqWords <- colSums(description_m)
freqWords <- freqWords[order(freqWords, decreasing = T)]
wordcloud(freq = as.vector(freqWords), words = names(freqWords),
          random.order = FALSE, colors = brewer.pal(8, 'Dark2'),max.words =100)
