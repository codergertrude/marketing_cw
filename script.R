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
