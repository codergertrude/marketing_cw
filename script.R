### LIBRARIES

library(tidyverse)
library(flexclust)
library(janitor)
library(cowplot)

### PROJECT 1

## TASK 1

data <- read.csv("office.csv", stringsAsFactors = FALSE)

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

# attitudinal variable dataset
data_att <- data[, 2:7]

# z-score standardisation
data_att_norm <- as.data.frame(scale(data_att, center=TRUE, scale=TRUE))

# inspect for smallest min (electronics) and largest max (electronics)
summary(data_att_norm)

## TASK 3

# set seed
set.seed(123)

# calculate dist
dist <- dist(data_att_norm, method = 'euclidean')

# run clustering with method = "ward.D2"
hier_clust <- hclust(dist, method = "ward.D2") 

# plot dendogram
plot(hier_clust)

## TASK 4

# 6-cluster visualisation
rect.hclust(hier_clust, k = 6, border = "red")

# 6-cluster solution
hcluster_groups <- cutree(hier_clust, k = 6)

# number of obs per cluster
table(hcluster_groups)

## TASK 5

# add cluster assignment column to normalised dataset
data_att_norm_p1t5 <- data_att_norm %>% 
  mutate(hcluster_groups = hcluster_groups)

# means for individual clusters 
data_att_norm_p1t5 %>%
  group_by(hcluster_groups) %>% # group by cluster
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  print(width = Inf) # prints all variables (all columns)

# flexclust profiles 
hier_clust_flex <- as.kcca(hier_clust, data_att_norm, k = 6)

table(hcluster_groups, clusters(hier_clust_flex))

barchart(hier_clust_flex, main = "Segment Profiles")

## TASK 6

## TASK 7

## TASK 8

# remove previous clustering
data_att_norm_p1t8 <- data_att_norm

# 5-cluster solution
hcluster_groups_5 <- cutree(hier_clust, k = 5)

# number of obs per cluster
table(hcluster_groups_5)

## TASK 9

# add cluster assignment column to normalised dataset
data_att_norm_p1t9 <- data_att_norm %>% 
  mutate(hcluster_groups_5 = hcluster_groups_5)

# means for individual clusters 
data_att_norm_p1t9 %>%
  group_by(hcluster_groups_5) %>% # group by cluster
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  print(width = Inf) # prints all variables (all columns)

# flexclust profiles 
hier_clust_flex_5 <- as.kcca(hier_clust, data_att_norm, k = 5)

table(hcluster_groups_5, clusters(hier_clust_flex_5))

barchart(hier_clust_flex_5, main = "Segment Profiles")

## TASK 10

## TASK 11

# add cluster groups to original dataset
data_p1t11 <- data %>% mutate(hcluster_groups_5 = hcluster_groups_5)

data_p1t11 <- data_p1t11 %>% 
  mutate(professional = factor(professional))

# function to sort averages using clusters
seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))  
}

seg.summ(data, data_p1t11$hcluster_groups_5)

# demographic on professional
data_p1t11 %>%
  tabyl(hcluster_groups_5, professional) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

# demographic on age
data_p1t11 %>%
  tabyl(hcluster_groups_5, age) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns() 

# demographic on income
data_p1t11 %>%
  tabyl(hcluster_groups_5, income) %>% 
  adorn_totals(c("row", "col")) %>% 
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()

## TASK 12

# remove hierarchical clustering assignment col
data_att_norm_p1t12 <- data_att_norm

# set seed
set.seed(123)

# k means clustering
kmeans_clust <- kmeans(data_att_norm_p1t12, 
                       centers = 5, 
                       iter.max = 1000,
                       nstart = 100)

table(kmeans_clust$cluster)

## TASK 13

# add k means clustering groups to normalised dataset
data_att_norm_p1t13 <- data_att_norm %>% 
  mutate(kcluster_groups = kmeans_clust$cluster)

# means for each cluster (kcluster)
data_att_norm_p1t13 %>%
  group_by(kcluster_groups) %>% # group by cluster
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  print(width = Inf) # prints all variables (all columns)

# means for each cluster (hcluster) 
data_att_norm_p1t9 %>%
  group_by(hcluster_groups_5) %>% # group by cluster
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  print(width = Inf) # prints all variables (all columns)

# cluster matches: 1-2, 2-3, 3-4, 4-5, 5-1

# hits calculation (returns reassigned observations)
hits = 0

for(i in 1:nrow(data)){
  if(data_att_norm_p1t9[i, 7] == 1 & data_att_norm_p1t13[i, 7] == 2){
    hits = hits + 1
    cat(sprintf("hits increment at i = %f\n", i))
  } else if(data_att_norm_p1t9[i, 7] == 2 & data_att_norm_p1t13[i, 7] == 3){
    hits = hits + 1
    cat(sprintf("hits increment at i = %f\n", i))
  } else if(data_att_norm_p1t9[i, 7] == 3 & data_att_norm_p1t13[i, 7] == 4){
    hits = hits + 1
    cat(sprintf("hits increment at i = %f\n", i))
  } else if(data_att_norm_p1t9[i, 7] == 4 & data_att_norm_p1t13[i, 7] == 5){
    hits = hits + 1
    cat(sprintf("hits increment at i = %f\n", i))
  } else if(data_att_norm_p1t9[i, 7] == 5 & data_att_norm_p1t13[i, 7] == 1){
    hits = hits + 1
    cat(sprintf("hits increment at i = %f\n", i))
  }
}

# hit percentage
1 - (hits/200)

### LIBRARIES

library(tidyverse)
library(ggmosaic)
library(ggpubr)
library(broom)
library(ROCR)

### PROJECT 2

## TASK 1

data <- read.csv("ecommerce.csv", stringsAsFactors = TRUE)

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
