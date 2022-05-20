### LIBRARIES

library(tidyverse)
library(ggmosaic)
library(ggpubr)
library(broom)

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
