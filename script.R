### PROJECT 1

## TASK 1
data <- read.csv("office.csv", stringsAsFactors = FALSE)

## TASK 2

# attitudinal variable dataset
data_att <- data[, 2:7]

# z-score standardisation
for(i in 1:ncol(data_att)){
  for(n in 1:nrow(data_att)){
    m <- mean(data_att[, i])
    s <- sd(data_att[, i])
    data_att[n, i] <- (data_att[n, i] - m) - s
  }
}

# inspect for smallest min (low_prices) and largest max (return_policy)
summary(data_att)


