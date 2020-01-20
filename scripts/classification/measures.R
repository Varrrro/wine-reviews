# Success rate
measures.success <- function(table, n) {
  success <- (sum(diag(tt)) / n) * 100
}

# Accuracy
measures.accuracy <- function(table) {
  dd <- diag(tt)
  acc <- c(1:nrow(tt))
  
  for(i in 1:nrow(tt)) {
    acc[i] <- dd[i] / sum(tt[i,])
  }
  
  return(acc)
}

# Recall
measures.recall <- function(table) {
  dd <- diag(tt)
  rec <- c(1:nrow(tt))
  
  for(i in 1:nrow(tt)) {
    rec[i] <- dd[i] / sum(tt[,i])
  }
  
  return(rec)
}

# F-measure
measures.fmeasure <- function(table) {
  dd <- diag(tt)
  fmes <- c(1:nrow(tt))
  
  for(i in 1:nrow(tt)) {
    fmes[i] <- 2 * dd[i] / (sum(tt[i,]) + sum(tt[,i]))
  }
  
  return(fmes)
}

# Total f-measure
measures.fmeasure.total <- function(table) {
  dd <- diag(tt)
  fmes <- c(1:nrow(tt))
  ft <- 0
  
  for(i in 1:nrow(tt)) {
    fmes[i] <- 2 * dd[i] / (sum(tt[i,]) + sum(tt[,i]))
    ft <- ft + (fmes[i] / nrow(tt))
  }
  
  return(ft)
}
