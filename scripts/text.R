library(tm)
library(qdap)

text.process <- function(text) {
  source <- VectorSource(text)
  corpus <- Corpus(source)
  
  # Replace contraction
  corpus <- tm_map(corpus, replace_contraction)
  # Convert to lower case
  corpus <- tm_map(corpus, content_transformer(tolower))
  # Remove punctuation marks in text
  corpus <- tm_map(corpus, removePunctuation)
  # Replace numbers with words
  corpus <- tm_map(corpus, content_transformer(replace_number))
  # Replace abbreviation
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  # Replace symbol
  corpus <- tm_map(corpus, content_transformer(replace_symbol))
  # Remove most common English words
  corpus <- tm_map(corpus, removeWords, stopwords("en"))
  # Remove wine varieties
  corpus <- tm_map(corpus, removeWords, c("blend", "pinot", "chardonnay", "cabernet"))
  # Remove excess whitespace
  corpus <- tm_map(corpus, stripWhitespace)
  # Stemming
  corpus <- tm_map(corpus,  stemDocument)
  
  dtm <- removeSparseTerms(DocumentTermMatrix(corpus), sparse = 0.999)
}