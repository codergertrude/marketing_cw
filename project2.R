### LIBRARIES

library(tidyverse)
library(ggmosaic)
library(ggpubr)
library(broom)
library(ROCR)

### PROJECT 2

## TASK 1

data <- read.csv("ecommerce.csv", stringsAsFactors = TRUE)

## TASK 2

# logistic regression model of conversion on discount 
m1 <- glm(conversion ~ discount, data = data, family = binomial)

# view coefficient estimates
summary(m1)

## TASK 3

# odds ratio for discountyes
exp(coef(m1))

## TASK 4

# confidence interval
exp(confint(m1))

## TASK 5

# mosaic plot for source impact on conversion
ggplot(data = data) + # start a ggplot
  geom_mosaic(aes( # use a mosaic geom
    x = product(discount),  # put 'promo' on x-axis
    fill = conversion), # use `pass` as fill color
    offset = 0.02, # set the space in-between 
    divider = ddecker()) + # double decker plot
  facet_grid(~source,# forms a matrix of panels by 'channel'
             scales = "free") + # let both facet height and width vary
  theme_pubr() + # use this theme
  theme(axis.text.y = element_blank(), # clustomize the theme
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 90)) + 
  labs(x = "", y = "") # don't use axis labels

## TASK 6

# logistic regression model of conversion on discount and source
m2 <- glm(conversion ~ discount + source, data = data, family = binomial)

# view coefficient estimates
summary(m2)

## TASK 7

# odds ratios 
exp(coef(m2))

## TASK 8 

# logistic regression model of conversion on discount and source with interactions
m3 <- glm(conversion ~ discount + source + discount:source, data = data, family = binomial)

# view coefficient estimates
summary(m3)

## TASK 9

# confidence interval
exp(confint(m3))

## TASK 10 

# logistic regression model of conversion on all variables, with interaction of discount and source
m4 <- glm(conversion ~ .+ discount:source, data = data, family = binomial)

# view coefficient estimates
summary(m4)

# confidence interval 
exp(confint(m4))

## TASK 11

# calculate correlation between two variables
cor(data$total_pages_visited, data$visit_duration)

## TASK 12

# removing visit_duration on new data object
data_p2t12 <- data[,-5]

# logistic regression model of conversion on all variables, with interaction of discount and source, without visit_duration
m5 <- glm(conversion ~ .+ discount:source, data = data_p2t12, family = binomial)

# view coefficient estimates
summary(m5)

# confidence interval 
exp(confint(m5))

## TASK 13

# plot
tidy(m5) %>% # tidy function from broom package
  mutate(exp_beta_llci = exp(confint(m5))[, 1], # lower ci
         exp_beta = exp(estimate), # odds ratio, midpoint
         exp_beta_ulci = exp(confint(m5))[, 2]) %>% # upper 
  select(term, estimate, exp_beta_llci, exp_beta, exp_beta_ulci) %>% 
  ggplot(aes(x = term, 
             y = exp_beta,
             ymin = exp_beta_llci,
             ymax = exp_beta_ulci)) +
  geom_point(size = 4) + 
  geom_errorbar(width = 0.25) +
  # add a horizontal line where odds ratio == 1.0 (no effect):
  geom_hline(yintercept = 1, linetype = "dashed", 
             size = 1, color = "dodgerblue") + 
  labs(title = "95% CI: Pass sign up odds by factor",
       x = NULL,
       y = "Likehood by Factor (odds ratio, main effect)") + 
  coord_flip() + # rotates the plot
  theme_pubr()

## TASK 14

# make predictions on m5 
predm5 <- predict(m5, type = "response")

# note mean of probabilities
summary(predm5)

# add probabilities as new column 'base_probs'
data_p2t14 <- data_p2t12
data_p2t14[, 6] <- predm5 
names(data_p2t14)[names(data_p2t14) == 'V6'] <- 'base_probs'
mean(data_p2t14$base_probs)

## TASK 15

data_p2t15 <- data_p2t14

# calculating indicator variable
pred_conversion <- as.data.frame(ifelse(predm5 > 0.5, "yes", "no"))
data_p2t15[, 7] <- pred_conversion
names(data_p2t15)[names(data_p2t15) == 'ifelse(predm5 > 0.5, "yes", "no")'] <- 'pred_conversion'
data_p2t15$pred_conversion <- as.factor(data_p2t15$pred_conversion)

table(data_p2t15$pred_conversion)

## TASK 16

# accuracy calculation
good_pred <- 0

for(i in 1:nrow(data_p2t15)){
  if(data_p2t15[i, 2] == data_p2t15[i, 7]){
    good_pred <- 1 + good_pred
  }
}

accuracy <- good_pred/nrow(data_p2t15)
accuracy*100

## TASK 17

# AUC calculation
ROCRpred <- prediction(predm5, data$conversion)
as.numeric(performance(ROCRpred, "auc")@y.values)

## TASK 18

# new dataset 
data_p2t18 <- data_p2t12

# increase total_pages_visited values by 1
for(i in 1:nrow(data_p2t18)){
  data_p2t18[i, 4] <- data_p2t18[i, 4] + 1
}

# new model definition
m6 <- glm(conversion ~ .+ discount:source, data = data_p2t18, family = binomial)

# new predictions
predm6 <- predict(m6, type = "response")

# adding probabilities as new column 'new_prob'
data_p2t18[, 6] <- predm6
names(data_p2t18)[names(data_p2t18) == 'V6'] <- 'new_prob'
mean(data_p2t18$new_prob)

# summary statistics for predictions, same as m5
summary(predm6)

## TASK 19

# lift calculation
(mean(data_p2t18$new_prob) - mean(data_p2t14$base_probs)) / mean(data_p2t14$base_probs)
