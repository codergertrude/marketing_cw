### LIBRARIES

library(dfidx)
library(mlogit)

### PROJECT 3

## TASK 1

data <- read.csv("cloud.csv", stringsAsFactors = TRUE)

summary(data)

# plots for each variable
plot_list = list()
for(i in 1:ncol(data)){
  var = names(data)[i]
  if(is.numeric(data[, i]) == TRUE){
    plot_list[[i]] = ggplot(data, aes(, data[, i])) + geom_histogram(bins = 10) + labs(title = var) + coord_flip()
    print(plot_list[[i]])
  }
  else if(is.factor(data[, i]) == TRUE){
    plot_list[[i]] = ggplot(data, aes(, data[, i])) + geom_bar() + labs(title = var) + coord_flip()
    print(plot_list[[i]])
  } else {
    cat(sprintf("feature %s is not graphable\n", i))
  }
}

## TASK 2

# price and cloud_storage ordinal factors
data$price <- relevel(data$price, ref = 'p6')

data$cloud_storage <- relevel(data$cloud_storage, ref = '30gb')

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

# view probabilities (probability of choosing 3rd alternative in 1st choice set: 2.84%)
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
predicted_alternative

## TASK 11

# extracting selected alternatives
selected_alternative <- 
  data %>% 
  filter(choice > 0) %>% 
  select(alternative_id) %>% 
  as_vector()
selected_alternative

## TASK 12

# confusion matrix
table(selected_alternative, predicted_alternative)

# accuracy calculation (60.56667%)
accuracy = (579 + 624 + 614)/3000
accuracy

## TASK 13

# custom function for hypothetical market share
predict.share <- function(model, d) {
  temp <- model.matrix(update(model$formula, 0 ~ .), data = d)[, -1] # generate dummy matrix
  u <- temp %*% model$coef[colnames(temp)] # calculate utilities
  probs <- t(exp(u) / sum(exp(u))) # calculate probabilities
  colnames(probs) <- paste("alternative", colnames(probs))
  return(probs)
}

## TASK 14

# create d_base for hypothetical market using present rows
d_base <- as_tibble(data_p3t3[
  c(135, 31, 61, 20, 72),
  c("cloud_storage", "customer_support", "cloud_services", "price_n")
])

## TASK 15

# run function on model2 and d_base, bind market share to last col of d_base
d_base <- cbind(d_base, as.vector(predict.share(model2, d_base)))
colnames(d_base)[5] <- "predicted_share"
d_base

## TASK 16

# change last row of hypothetical market
d_base_2 <- as_tibble(data_p3t3[
  c(135, 31, 61, 20, 69),
  c("cloud_storage", "customer_support", "cloud_services", "price_n")
])

# bind marketshares for new hypothetical scenario
d_base_2 <- cbind(d_base_2, as.vector(predict.share(model2, d_base_2)))
colnames(d_base_2)[5] <- "predicted_share"

d_base_2

## TASK 17

# compare both scenarios, percent change from scenario 1 to 2 (alternative 5 reduced by 17.72%)
d_base_2[, 5] - d_base[, 5]

## TASK 18

# willingness to pay for customer support (pounds per month)
- coef(model2)["customer_supportyes"] / coef(model2)["price_n"]

## TASK 19

# willingness to pay for 2000gb from 30gb (pounds per month)
- coef(model2)["cloud_storage2000gb"] / coef(model2)["price_n"]

## TASK 20

# willingness to pay for 5000gb from 2000gb (pounds per month)
- (coef(model2)["cloud_storage5000gb"] - coef(model2)["cloud_storage2000gb"]) / coef(model2)["price_n"]


