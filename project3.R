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

# extracting predicted alternatives
predicted_alternative <-
  predicted_probabilities %>% 
  rowid_to_column("choiceset_id") %>% 
  pivot_longer(!choiceset_id, names_to = "choice", values_to = "prob") %>% 
  group_by(choiceset_id) %>% 
  slice(which.max(prob)) %>% 
  ungroup() %>% 
  select(choice) %>% 
  as_vector()

## TASK 11

# extracting selected alternatives
selected_alternative <- 
  data %>% 
  filter(choice > 0) %>% 
  select(alternative_id) %>% 
  as_vector()

## TASK 12

# confusion matrix
table(selected_alternative, predicted_alternative)

# accuracy calculation (60.56667%)
accuracy = (579 + 624 + 614)/3000
accuracy

## TASK 13

