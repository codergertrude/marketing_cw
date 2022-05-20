### PROJECT 1

## TASK 1
data <- read.csv("office.csv", stringsAsFactors = FALSE)

## TASK 2

# attitudinal variable dataset
data_att <- data[, 2:7]

# z-score standardisation
data_att <- scale(data_att, center=TRUE, scale=TRUE)

# inspect for smallest min (electronics) and largest max (electronics)
summary(data_att)

## TASK 3



