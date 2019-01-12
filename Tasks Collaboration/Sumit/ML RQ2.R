install.packages("factoextra")
library(factoextra)
library(cluster)
library(clValid)
library(tidyverse)

## Set file path
file_path <- "Input Dataset/Cleaned Dataset/Supermarket_Data_Clustering.csv"

## Read data from a file
supermarket_data_clus <- read_csv(file_path)

## Scale the data
supermarket_data_scaled <- scale(supermarket_data_clus[,-26])


## Computed correlation based distances
sm_dist_spear = get_dist(supermarket_data_scaled, method = "spearman")

## Visualize distance matrix
fviz_dist(sm_dist_spear)


### Assess the clustering tendency ###
## Compute Hopkins stats
sm_hop <-  get_clust_tendency(supermarket_data_scaled, n = nrow(supermarket_data_scaled)-1, graph = FALSE)

## Check Hopkins stats
sm_hop$hopkinfhopkins_stats

## Visualize Hopkins stats


### Determine the optimal number of clusters ###
## Set value range for k
K <- c(2:30)

## Apply k-means for different values of k and record the SSE
sm_sse <- map_dfr(K, ~ tibble(k = .x, sse = kmeans(supermarket_data_scaled, centers = .x)$tot.withinss))

## Plot SSE for different values of k
ggplot(sm_sse, aes(k, sse)) +
  geom_line() + 
  geom_point() +
  scale_x_continuous(breaks = seq(2, 30, 2))

clustering <- kmeans(supermarket_data_scaled, centers = 7)

names(clustering)
head(clustering$cluster)
clustering$size
clustering$iter





## Compute the distance matrix
sm_dist_eucl <- dist(supermarket_data_scaled)

## Apply k-means with different value of k and record the average Silhouette Coefficient
sm_sico <- map_dfr(K, function(k) {
  clustering <- kmeans(supermarket_data_scaled, centers = k)
  si <- silhouette(clustering$cluster, sm_dist_eucl)
  tibble(k = k, si = mean(si[, 3]))
})

## Plot average Silhouette Coefficient for different values of k
ggplot(sm_sico, aes(k, si)) +
  geom_line() + geom_point() +
  labs(y = "Avg. Silhouette\ncoefficient")


## Apply k-means with different value of k and record the Gap Statistic
sm_gapstat <- clusGap(supermarket_data_scaled, FUN = kmeans, nstart = 30, K.max = 10, B = 500)

## Plot Gap Statistic for different values of k
plot(gap_stat, frame = FALSE, xlab = "Number of clusters k")
abline(v = 3, lty = 2)


## Set the clustering methods to be used
sm_clmethods <- c("hierarchical","kmeans","pam")

## Compute the cluster validity based on internal validation
sm_internal <- clValid(supermarket_data_scaled, nClust = 2:6, clMethods = sm_clmethods, validation = "stability", metric = "correlation", method = "average")

## Summary of internal validation
summary(sm_internal)

## Compute the cluster validity based on stability validation
sm_stability <- clValid(supermarket_data_scaled, nClust = 2:6, clMethods = sm_clmethods, validation = "stability", metric = "correlation", method = "average")

## Summary of stability validation
summary(sm_stability)

# Display only optimal Scores
optimalScores(sm_stability)