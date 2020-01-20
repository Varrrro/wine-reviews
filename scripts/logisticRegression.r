library("ggplot2")
library(reshape2)
library(stringr)
library(e1071)

# Read data
wineData <- read.csv(file="../data/wine-reviews-trimmed.csv", header=TRUE, sep=",")
wineData <- na.omit(wineData)
wineData <- subset(wineData, price < 100)

# Coonvert points to a binary variable
wineData$points <- ifelse(wineData$points < 5, 0, 1)

# Separate dataset in train and test
trainingRowIndex <- sample(1:nrow(wineData), 0.8*nrow(wineData)) 
# model training data
trainingData <- wineData[trainingRowIndex, ] 
# test data
testData  <- wineData[-trainingRowIndex, ]   

# Build logistic regression model
logit_model <- glm(points ~ price, data = trainingData, family = "binomial")

# Plot model
ggplot(data = trainingData, aes(x = price, y = points)) +
  geom_point(aes(color = as.factor(points)), shape = 1) + 
  stat_function(fun = function(x){predict(logit_model,
                                          newdata = data.frame(price = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad default") +
  theme(legend.position = "none")

accuracy_logistic_regression <- function(logit_model, data){
  # Make predictions
  probPred <- predict(logit_model, data)
  # Convert to binary
  pointsPred <- ifelse(probPred < 0.5, 0, 1)
  
  aciertos = 0
  
  for (i in 1:length(pointsPred)) { 
    if(data$points[i] == pointsPred[i]){
      aciertos = aciertos+1
    }
  }
  
  # Print accuracy
  print(100.0*aciertos/length(pointsPred))
}

# Accuracy training data
accuracy_logistic_regression(logit_model, trainingData)
# Accuracy test data
accuracy_logistic_regression(logit_model, testData)



