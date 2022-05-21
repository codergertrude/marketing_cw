### LIBRARIES

library(dfidx)
library(mlogit)

### PROJECT 3

## TASK 1

data <- read.csv("cloud.csv", stringsAsFactors = TRUE)

## TASK 2

# price and cloud_storage ordinal factors
data$price <- factor(data$price, order = TRUE, 
                     levels = c("p6", "p12", "p18"))

data$cloud_storage <- factor(data$cloud_storage, order = TRUE,
                             levels = c("30gb", "2000gb", "5000gb"))

## TASK 3

data_p3t3 <- data

# adding price_n
for(i in 1:nrow(data_p3t3)){
  if(data_p3t3[i, 8] == "p6"){
    data_p3t3[i, 10] = 6
  } else if(data_p3t3[i, 8] == "p12"){
    data_p3t3[i, 10] = 12
  } else {
    data_p3t3[i, 10] = 18
  }
}

names(data_p3t3)[names(data_p3t3) == 'V10'] <- 'price_n'

# price_n mean (12.01533)
mean(data_p3t3$price_n)

## TASK 4

data_p3t4 <- data_p3t3

# filtering dataset for choice == 1
data_p3t4 <- filter(data_p3t4, choice == 1)

# times 30gb was chosen (830)
table(data_p3t4$cloud_storage)

# percentage that chose email only (20.8%)
prop.table(table(data_p3t4$cloud_services)) * 100

## TASK 5
data_p3t5 <- data_p3t3

# idx dataframe (column number same?)
m_data <- dfidx(data_p3t5,
                choice = "choice",
                idx = list(c("choice_id", "respondent_id"),
                           "alternative_id"))

m_data

## TASK 6

# set seed
set.seed(123)

# multinominal logit 
model1 <- mlogit(choice ~ 0 + cloud_storage + customer_support + cloud_services + price, data = m_data)

# coefficients
summary(model1)

## TASK 7

# set seed
set.seed(123)

# model2 with price_n
model2 <- mlogit(choice ~ 0 + cloud_storage + customer_support + cloud_services + price_n, data = m_data)

# coefficients
summary(model2)

## TASK 8

# likelihood ratio test
lrtest(model2, model1)

## TASK 9

# predictions
predicted_probabilities <- predict(model2, m_data) %>% 
  as_tibble()

# view probabilities (probability of choosing 3rd alternative in 1st choice set: 0.0284%)
predicted_probabilities

## TASK 10

predicted_alt <- data.frame(prediction = numeric())

# mapping most likely choice as prediction (prediction for choice set 3 is 2)
for(i in 1:nrow(predicted_probabilities)){
  if(predicted_probabilities[i, 1] > predicted_probabilities[i, 2] & predicted_probabilities[i, 1] > predicted_probabilities[i, 3]){
    predicted_alt[i, 1] <- 1
  } else if(predicted_probabilities[i, 2] > predicted_probabilities[i, 1] & predicted_probabilities[i, 2] > predicted_probabilities[i, 3]){
    predicted_alt[i, 1] <- 2
  } else {
    predicted_alt[i, 1] <- 3
  }
}

## TASK 11

# extracting selections from original dataset (selected alternative for 15th choice set is 2)
selected_alt <- as.data.frame(data_p3t4$alternative_id)

## TASK 12

# accuracy calculation (60.56667% calculated accuracy)
good_pred_hyp <- 0

for(i in 1:nrow(selected_alt)){
  if(selected_alt[i, 1] == predicted_alt[i, 1]){
    good_pred_hyp <- 1 + good_pred_hyp
  }
}

accuracy_hyp <- good_pred_hyp/nrow(selected_alt)
accuracy_hyp*100

## TASK 13
