library(tm)
source("text/process.R")

# Read dataset from CSV
data <- read.csv(file = "../data/wine-reviews-trimmed.csv", header = TRUE, sep = ",")

# Create document-term matrix
dtm <- text.process(data$description)

# Find word associations
findAssocs(dtm, findFreqTerms(dtm, 1000), 0.1)
