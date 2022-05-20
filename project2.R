### LIBRARIES

library(tidyverse)
library(ggmosaic)
library(ggpubr)

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
