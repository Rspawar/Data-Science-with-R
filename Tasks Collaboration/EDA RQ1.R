library(dplyr)
library(tidyverse)
library(ggplot2)

#Splitting the data
get_slice_for_shop <- function(col1, col2){
  shop_slice <- shop_ordered_slice3_ext[,col1:col2]
  colnames(shop_slice) <- c("distance","price")
  return(shop_slice)
}

shop_1_data <- get_slice_for_shop(1,2)
shop_2_data <- get_slice_for_shop(3,4)
shop_3_data <- get_slice_for_shop(5,6)
shop_4_data <- get_slice_for_shop(7,8)
shop_5_data <- get_slice_for_shop(9,10)
shop_avg_data <- get_slice_for_shop(13,14)
shop_agg_min_data <- get_slice_for_shop(11,14)
shop_agg_max_data <- get_slice_for_shop(12,14)

#Combine data to the one mutated table to show all shops at the one graph
joined_shops_data <- mutate(shop_1_data, Shop="1") %>%
  union_all(mutate(shop_2_data, Shop="2")) %>%
  union_all(mutate(shop_3_data, Shop="3")) %>%
  union_all(mutate(shop_4_data, Shop="4")) %>%
  union_all(mutate(shop_5_data, Shop="5")) 

#Create base for plots
get_base_for_plot <- function(dataset, caption){
  plot_base <- ggplot(data=dataset, aes(distance, price)) + ggtitle(caption)
  return(plot_base)
}

plot_base_1 <- get_base_for_plot(shop_1_data, "Shop 1")
plot_base_2 <- get_base_for_plot(shop_2_data, "Shop 2")
plot_base_3 <- get_base_for_plot(shop_3_data, "Shop 3")
plot_base_4 <- get_base_for_plot(shop_4_data, "Shop 4")
plot_base_5 <- get_base_for_plot(shop_5_data, "Shop 5")
plot_base_avg <- get_base_for_plot(shop_avg_data, "Average price comparing with average distance")
plot_base_min_avg <- get_base_for_plot(shop_agg_min_data, "Average price comparing with min distance") 
plot_base_max_avg <- get_base_for_plot(shop_agg_max_data, "Average price comparing with max distance") 
plot_base_joined <- get_base_for_plot(joined_shops_data, "All shops")

#Visualisation part
colours_shema <- c("Red", "Green", "Yellow", "Pink", "Blue", "Purple", "steelblue1", "tomato1")

#Covariation 

#geom_point

# The point geom is used to create scatterplots. The scatterplot is most useful for displaying the relationship between 
# two continuous variables.
# It can be used to compare one continuous and one categorical variable, or two categorical variables
draw_cov_geom_plot <- function(plot_base, colorNum){
  cov_geom_plot <- plot_base + geom_point(colour=colours_shema[colorNum])
  return(cov_geom_plot)
} 

p1_1 <- draw_cov_geom_plot(plot_base_1, 1)
p2_1 <- draw_cov_geom_plot(plot_base_2, 2)
p3_1 <- draw_cov_geom_plot(plot_base_3, 3)
p4_1 <- draw_cov_geom_plot(plot_base_4, 4)
p5_1 <- draw_cov_geom_plot(plot_base_5, 5)
pavg_1 <- draw_cov_geom_plot(plot_base_avg, 6)
pmin_1 <- draw_cov_geom_plot(plot_base_min_avg, 7)
pmax_1 <- draw_cov_geom_plot(plot_base_max_avg, 8)

pall_1 <- plot_base_joined + geom_point(mapping = aes(colour = Shop))

#geom_frequently
# Visualise the distribution of a single continuous variable by dividing the x axis into bins and counting the number of observations in each bin