### LIBRARIES

library(tidyverse)
library(flexclust)

### PROJECT 1

## TASK 1

data <- read.csv("office.csv", stringsAsFactors = FALSE)

## TASK 2

# attitudinal variable dataset
data_att <- data[, 2:7]

# z-score standardisation
data_att_norm <- as.data.frame(scale(data_att, center=TRUE, scale=TRUE))

# inspect for smallest min (electronics) and largest max (electronics)
summary(data_att)

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
data_att_norm <- data_att_norm %>% 
  mutate(hcluster_groups = hcluster_groups)

# means for individual clusters 
data_att_norm %>%
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

# 5-cluster solution
hcluster_groups_5 <- cutree(hier_clust, k = 5)

# number of obs per cluster
table(hcluster_groups_5)

## TASK 9


