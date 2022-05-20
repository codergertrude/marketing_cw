### LIBRARIES

library(tidyverse)

### PROJECT 2

## TASK 1

data <- read.csv("ecommerce.csv", stringsAsFactors = TRUE)

## TASK 2

# logistic regression model of conversion on discount 
m1 <- glm(conversion ~ discount, data = data, family = binomial)

# view coefficients
summary(m1)

## TASK 3

# odds ratio for discountyes
exp(1.10090)

## TASK 4


