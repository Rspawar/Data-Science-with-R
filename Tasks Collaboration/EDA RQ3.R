library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(readr)
library(dplyr)


file_path<- "Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
supermarket_data_clean <- read_csv(file_path)

#select related columns

slice1<-select(supermarket_data_clean,1,17,18,19,20,21,29,30,31,32,33)
View(slice1)

#get ratio for maximum likelihood of a customer to select or buy high value items from particular shop
value_ratio_shop_1<-(slice1$amount_purchased_shop_1/slice1$unique_products_purchased_shop_1)
value_ratio_shop_2<-(slice1$amount_purchased_shop_2/slice1$unique_products_purchased_shop_2)
value_ratio_shop_3<-(slice1$amount_purchased_shop_3/slice1$unique_products_purchased_shop_3)
value_ratio_shop_4<-(slice1$amount_purchased_shop_4/slice1$unique_products_purchased_shop_4)
value_ratio_shop_5<-(slice1$amount_purchased_shop_5/slice1$unique_products_purchased_shop_5)

customer_value_shop<-data.frame(slice1$customer_id,value_ratio_shop_1,value_ratio_shop_2,value_ratio_shop_3,value_ratio_shop_4,value_ratio_shop_5)

#convert all nan values to 0
customer_value_shop[is.na(customer_value_shop)] <- 0
<<<<<<< HEAD
View(customer_value_shop)
=======
View(customer_value_shop)
>>>>>>> 14ec35239da2286060b02f9524c8a3a31b053016
