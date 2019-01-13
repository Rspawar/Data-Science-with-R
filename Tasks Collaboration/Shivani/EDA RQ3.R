
library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(readr)
library(dplyr)
library(plotly)

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)


#plotting based on all combinations of columns
ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$distance_avg, y=supermarket_data_predict$products_purchased_avg,colour=supermarket_data_predict$most_pref_shop)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) +scale_x_discrete(name="Average distance")+scale_y_continuous(name="Average products purchased")

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$products_purchased_avg, y=supermarket_data_predict$unique_products_purchased_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) +scale_x_discrete(name="Average products purchased")+scale_y_continuous(name="Average unique products purchased")

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$distance_avg, y=supermarket_data_predict$unique_products_purchased_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$distance_avg, y=supermarket_data_predict$product_price_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$distance_avg, y=supermarket_data_predict$amount_purchased_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$products_purchased_avg, y=supermarket_data_predict$product_price_avg,colour=supermarket_data_predict$most_pref_shop)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 

ggplot (supermarket_data_predict, aes (x = supermarket_data_predict$products_purchased_avg, y = supermarket_data_predict$product_price_avg, fill = supermarket_data_predict$most_pref_shop)) + stat_binhex (bins=5, aes (alpha = ..count..)) + facet_grid (. ~ supermarket_data_predict$most_pref_shop)


ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$products_purchased_avg, y=supermarket_data_predict$amount_purchased_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$unique_products_purchased_avg, y=supermarket_data_predict$product_price_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$unique_products_purchased_avg, y=supermarket_data_predict$amount_purchased_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 

ggplot(supermarket_data_predict, aes(x=supermarket_data_predict$product_price_avg, y=supermarket_data_predict$amount_purchased_avg)) +
  geom_point(shape=1, aes(color = factor(supermarket_data_predict$most_pref_shop))) 
