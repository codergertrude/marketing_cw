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
data_att_norm_p1t5 <- data_att_norm %>% 
  mutate(hcluster_groups = hcluster_groups)

# means for individual clusters 
data_att_norm_p1t5 %>%
  group_by(hcluster_groups) %>% # group by cluster
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  print(width = Inf) # prints all variables (all columns)

# flexclust profiles 
hier_clust_flex <- as.kcca(hier_clust, data_att_norm_p1t5, k = 6)

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
hier_clust_flex_5 <- as.kcca(hier_clust, data_att_norm_p1t9, k = 5)

table(hcluster_groups_5, clusters(hier_clust_flex_5))

barchart(hier_clust_flex_5, main = "Segment Profiles")

## TASK 10

## TASK 11

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

# means for each cluster
data_att_norm_p1t13 %>%
  group_by(kcluster_groups) %>% # group by cluster
  summarise_all(~ mean(.x)) %>% # calculate the mean per group 
  print(width = Inf) # prints all variables (all columns)
