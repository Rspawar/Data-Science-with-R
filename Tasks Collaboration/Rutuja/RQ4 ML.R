library(tidyverse)
library(dplyr)
library(ggplot2)
library(factoextra)
library("RColorBrewer")
library(cluster)
library("metricsgraphics")


# read file contents
#supermarket_data_clean_matrix <- as.matrix((read.csv("Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv")))
supermarket_data_clean <- read.csv("Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv")

# Prepare data frames for clustering
cluster.slice.temp <- supermarket_data_clean[,c(1,29,30,31,32,33)]
cluster.slice.data <- supermarket_data_clean[,c(29,30,31,32,33)]
cluster.slice <- supermarket_data_clean_matrix[,c(1,29,30,31,32,33)]
#rownames(cluster.slice) <- supermarket_data_clean[,1]
#cluster.slice <- cluster.slice[,-1]

# Scale the data and Determine the ideal number of clusters
cluster.slice.scale <- scale(cluster.slice.data)

wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(cluster.slice.scale)

# The sharp decreases from 1 to 4 clusters 
#with a little decrease between 4 to 5 estimates a 5-cluster solution

# Visualize the different separable clusters in the data
# There exits some customer clusters based on the amount purchased by them in each shops


set.seed(123) # fix the random starting clusters
final <- kmeans(cluster.slice.data, 5, nstart = 25)
final

#PCA

cluster.pc <- prcomp(cluster.slice.data, center = FALSE, scale. = FALSE)$x %>% as.data.frame()

cluster.pc$kmeans.cluster <- final$cluster

p1<-mjs_plot(cluster.pc, x=PC1, y=PC2) %>%
  mjs_point(color_accessor=kmeans.cluster) %>%
  mjs_labs(x="principal comp 1", y="principal comp 2")

fviz_cluster(final, data = cluster.slice.data, geom = "point",
             stand = FALSE, ellipse.type = "norm") + 
  ggtitle(label='Customer Clusters') 

## retrieve customer ID's in each cluster
head(gather(data.frame(cluster.slice.temp[final$cluster == 1,])))
head(gather(data.frame(cluster.slice.temp[final$cluster == 2,])))
head(gather(data.frame(cluster.slice.temp[final$cluster == 3,])))
head(gather(data.frame(cluster.slice.temp[final$cluster == 4,])))
head(gather(data.frame(cluster.slice.temp[final$cluster == 5,])))


#cluster statistics
cluster.slice.temp %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

#Customer segmentation
cluster.slice.kmeans.aggregate <- aggregate(cluster.slice.data, by = list(final$cluster), mean)
View(cluster.slice.kmeans.aggregate)

cluster<-c(cluster.slice.kmeans.aggregate$Group.1)
shop1<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_1)
shop2<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_2)
shop3<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_3)
shop4<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_4)
shop5<-c(cluster.slice.kmeans.aggregate$amount_purchased_shop_5)

# Plot a Bar graph
Legends <-c(rep("Customers Shop 1", 5), rep("Customers Shop 2", 5), rep("Customers Shop 3", 5), rep("Customers Shop 4", 5), rep("Customers Shop 5", 5))
values <-c(shop1,shop2,shop3,shop4,shop5)
mydata <-data.frame(cluster, values)

p <-ggplot(mydata, aes(cluster, values))
p +geom_bar(stat = "identity", aes(fill = Legends)) +
  xlab("Cluster") + ylab("Total") +
  ggtitle("Customer Segmentation") +
  theme_bw() + scale_y_continuous(labels = scales::comma)

