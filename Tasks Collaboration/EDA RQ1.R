library(dplyr)
library(tidyverse)
library(ggplot2)
library(modelr)

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
  plot_base <- ggplot(data = dataset, mapping = aes(x = distance, y = price)) + ggtitle(caption)
  return(plot_base)
}

#Visualisation part
colours_shema <- c("Red", "Green", "Yellow", "Pink", "Blue", "Purple", "steelblue1", "tomato1")

#Covariation 

# The point geom is used to create scatterplots. The scatterplot is most useful for displaying the relationship between 
# two continuous variables.
# It can be used to compare one continuous and one categorical variable, or two categorical variables
add_geom_point <- function(colorNum){
  geom_p <- geom_point(colour=colours_shema[colorNum], alpha=0.3)
  return(geom_p)
}

draw_cov_point_plot <- function(dataset, colorNum, caption){
  cov_geom_plot <- get_base_for_plot(dataset, caption) + add_geom_point(colorNum)
  return(cov_geom_plot)
} 

p1_1 <- draw_cov_point_plot(shop_1_data, 1, "Shop 1")
p2_1 <- draw_cov_point_plot(shop_2_data, 2, "Shop 2")
p3_1 <- draw_cov_point_plot(shop_3_data, 3, "Shop 3")
p4_1 <- draw_cov_point_plot(shop_4_data, 4, "Shop 4")
p5_1 <- draw_cov_point_plot(shop_5_data, 5, "Shop 5")
pavg_1 <- draw_cov_point_plot(shop_avg_data, 6, "Average price with average distance")
pmin_1 <- draw_cov_point_plot(shop_agg_min_data, 7, "Average price with min distance")
pmax_1 <- draw_cov_point_plot(shop_agg_max_data, 8, "Average price with max distance")

pall_1 <- get_base_for_plot(joined_shops_data, "All shops") + geom_point(mapping = aes(colour = Shop), alpha=0.3)

# Missing values
# Covers the situation when average price is 0

draw_missing_values_plot  <- function(dataset, colorNum, caption){
  dataset_with_na <- dataset %>% 
    mutate(price = ifelse(price == 0, NA, price))%>% 
    mutate(missed = is.na(price))
  missing_values_plot <- get_base_for_plot(dataset_with_na, caption) + 
    add_geom_point(colorNum)
  return(missing_values_plot)
}

p1_2 <- draw_missing_values_plot(shop_1_data, 1, "Shop 1")
p2_2 <- draw_missing_values_plot(shop_2_data, 2, "Shop 2")
p3_2 <- draw_missing_values_plot(shop_3_data, 3, "Shop 3")
p4_2 <- draw_missing_values_plot(shop_4_data, 4, "Shop 4")
p5_2 <- draw_missing_values_plot(shop_5_data, 5, "Shop 5")

pavg_2 <- draw_missing_values_plot(shop_avg_data, 6, "Average price with average distance")

# Visualizing distribution
# It’s common to want to explore the distribution of a continuous variable broken down by a categorical variable.

# It’s much easier to understand overlapping lines than bars.
pavg_3 <- ggplot(data = shop_avg_data, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(colour=colours_shema[6], binwidth = 500) + ggtitle("Average price distribution")
pall_2 <- ggplot(data = joined_shops_data, mapping = aes(x = price, y = ..density..)) + 
  geom_freqpoly(mapping = aes(colour = Shop), binwidth = 500) + ggtitle("Common average price distribution")

#Pattern visualisation
# It’s possible to use a model to remove the very strong relationship between price and distance
# so we can explore the subtleties that remain. The following code fits a model that predicts price from dependencies and then computes the residuals (the difference between the predicted value and the actual value). 
# The residuals give us a view of the price, once the effect of distance has been removed
joined_shops_without_null <- filter(joined_shops_data, price != 0)
mod <- lm(log(price) ~ log(distance), data = joined_shops_without_null)

joined_shops_data2 <- joined_shops_without_null %>% 
  add_residuals(mod) %>% 
  mutate(resid = exp(resid))


pall_3 <- ggplot(data = joined_shops_data2) + 
  geom_point(mapping = aes(x = price, y = resid)) + ggtitle("Average price pattern")

# Once you’ve removed the strong relationship between distance and price, you can see what you expect in the relationship between shop and price relative other external factors (better quality of products, products alternative, shop location and so on)

pall_4 <- ggplot(data = joined_shops_data2) + 
  geom_boxplot(mapping = aes(x = Shop, y = resid)) + ggtitle("Average price pattern")