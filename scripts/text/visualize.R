library(wordcloud)
source("text/process.R")

# Read dataset from CSV
data <- read.csv(file = "../data/wine-reviews-trimmed.csv", header = TRUE, sep = ",")

# Create document-term matrix
dtm <- text.process(data$description)

# Convert to matrix
matrix <- as.matrix(dtm)

# Print a portion of the matrix
matrix[1:18, 1:12]

# Set correct column names
colnames(matrix) <- make.names(colnames(matrix))

# Draw word cloud
freqWords <- colSums(matrix)
freqWords <- freqWords[order(freqWords, decreasing = T)]
wordcloud(freq = as.vector(freqWords), words = names(freqWords),
          random.order = FALSE, colors = brewer.pal(8, 'Dark2'),max.words =100)
