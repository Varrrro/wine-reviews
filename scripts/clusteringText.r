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


clustering.kmeans <- kmeans(tfidf.matrix, 4) 
clustering.hierarchical <- hclust(dist.matrix, method = "ward.D2") 
clustering.dbscan <- dbscan::hdbscan(dist.matrix, minPts = 10)

table(wineData$variety,clustering.kmeans$cluster)

master.cluster <- clustering.kmeans$cluster 
slave.hierarchical <- cutree(clustering.hierarchical, k = 4) 
slave.dbscan <- clustering.dbscan$cluster 
stacked.clustering <- rep(NA, length(master.cluster))  
names(stacked.clustering) <- 1:length(master.cluster) 
for (cluster in unique(master.cluster)) { 
  indexes = which(master.cluster == cluster, arr.ind = TRUE) 
  slave1.votes <- table(slave.hierarchical[indexes]) 
  slave1.maxcount <- names(slave1.votes)[which.max(slave1.votes)]   
  slave1.indexes = which(slave.hierarchical == slave1.maxcount, arr.ind = TRUE) 
  slave2.votes <- table(slave.dbscan[indexes]) 
  slave2.maxcount <- names(slave2.votes)[which.max(slave2.votes)]   
  stacked.clustering[indexes] <- slave2.maxcount 
}

points <- cmdscale(dist.matrix, k = 2) 
palette <- colorspace::diverge_hcl(4) # Creating a color palette 
previous.par <- par(mfrow=c(2,2), mar = rep(1.5, 4)) 

plot(points, main = 'K-Means clustering', col = as.factor(master.cluster), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Hierarchical clustering', col = as.factor(slave.hierarchical), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0),  
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Density-based clustering', col = as.factor(slave.dbscan), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
plot(points, main = 'Stacked clustering', col = as.factor(stacked.clustering), 
     mai = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), 
     xaxt = 'n', yaxt = 'n', xlab = '', ylab = '') 
par(previous.par) # recovering the original plot space parameters
