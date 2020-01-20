library(arules)
library(arulesViz)

# Read dataset from CSV
data <- read.csv(file = "../data/wine-reviews-trimmed.csv", header = TRUE, sep = ",")

# Remove unwanted columns
data$title <- NULL
data$description <- NULL
data$designation <- NULL
data$taster_name <- NULL
data$taster_twitter_handle <- NULL
data$province <- NULL
data$region_1 <- NULL
data$region_2 <- NULL

# Discretize numerical columns
data$price <- discretize(data$price, "interval", 100)
data$points <- discretize(data$points, "interval", 4, labels = c("Low","Medium","High","Very high"))
data$year <- discretize(data$year, "interval", 6)

# Convert to transactions database
tr <- as(data, "transactions")

itemFrequencyPlot(tr, support = 0.2)

# Training Apriori on the database
rules <- apriori(tr, parameter = list(support = 0.1, confidence = 0.6, minlen = 2))

# View rules
inspect(sort(rules, by = "confidence"), ruleSep = "---->", itemSep = " + ", setStart = "", setEnd = "", linebreak = TRUE)

# Plot rules
plot(rules, method = "scatterplot", measure = c("support", "confidence"))
plot(rules[1:10], method = "graph", measure = "confidence", control = list(main = "Grafo"))
