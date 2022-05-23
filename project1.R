### LIBRARIES

library(tidyverse)
library(flexclust)
library(janitor)

### PROJECT 1

## TASK 1

data <- read.csv("office.csv", stringsAsFactors = FALSE)

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

# demographic on professional
data_p1t11 %>%
  tabyl(hcluster_groups_5, professional) %>% 
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
