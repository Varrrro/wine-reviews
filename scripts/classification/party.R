library(party)
source("text/process.R")
source("classification/measures.R")

# Read dataset from CSV
data <- read.csv(file = "../data/wine-reviews-trimmed.csv", header = TRUE, sep = ",")

# Create document-term matrix
dtm <- text.process(data$description)

# Obtain dataframe from dtm
frame <- as.data.frame(as.matrix(dtm))

# Add class column to dataframe
frame <- cbind(frame, variety = data$variety)

# Set correct column names for classifier
colnames(frame) <- make.names(colnames(frame))

# Split into two partitions: 80% train and 20% test
ind <- sample(2, nrow(frame), replace = TRUE, prob = c(0.8, 0.2))
train <- frame[ind==1,]
test <- frame[ind==2,]

# Create tree
wineTree <- ctree(variety ~ ., train)

# Plot tree
plot(wineTree)
plot(wineTree, type = "simple")

# Predict train data
table(predict(wineTree), train$variety)

# Predict test data
tt <- table(predict(wineTree, test), test$variety)

# Measures
successRate <- measures.success(tt, nrow(test))
acc <- measures.accuracy(tt)
rec <- measures.recall(tt)
fmes <- measures.fmeasure(tt)
fmesTotal <- measures.fmeasure.total(tt)
