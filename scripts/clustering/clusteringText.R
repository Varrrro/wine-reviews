library("ggplot2")
library(reshape2)
library(stringr)
library(tm)
library(qdap)
library(wordcloud)
library(SnowballC)
library(data.table)
library(proxy)

set.seed(20)

wineData <- read.csv(file="../data/wine-reviews-trimmed.csv", header=TRUE, sep=",")

data_description <- as.vector(wineData$description)
description_source <- VectorSource(data_description)
description_corpus <- Corpus(description_source)

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

# Create the dtm from the corpus
description_dtm <- DocumentTermMatrix(description_corpus)

description_dtm.tfidf <- tm::weightTfIdf(description_dtm)

description_dtm.tfidf <- tm::removeSparseTerms(description_dtm.tfidf, 0.985) 
tfidf.matrix <- as.matrix(description_dtm.tfidf) 

dist.matrix = proxy::dist(tfidf.matrix, method = "cosine")

datos <- scale(tfidf.matrix)
fviz_nbclust(x = datos, FUNcluster = kmeans, method = "silhouette", k.max = 15) +
  labs(title = "Número óptimo de clusters")

n_cluster = 2

clustering.kmeans <- kmeans(tfidf.matrix, n_cluster) 
points <- cmdscale(dist.matrix, k = 2) 
# Creating a color palette 
palette <- colorspace::diverge_hcl(n_cluster) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 

table(wineData$variety,clustering.kmeans$cluster)

# -------------------------------------------------

n_cluster = 4

clustering.kmeans <- kmeans(tfidf.matrix, n_cluster) 
points <- cmdscale(dist.matrix, k = 2) 
# Creating a color palette 
palette <- colorspace::diverge_hcl(n_cluster) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 

table(wineData$variety,clustering.kmeans$cluster)

# -------------------------------------------------

dev.off() # Clear graphics
