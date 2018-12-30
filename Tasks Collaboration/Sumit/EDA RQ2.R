library(tidyverse)
library(ggplot2)
library("RColorBrewer")

# Every customer is expected to buy a certain number of unique products. The amount of unique
# products he buys from a shop shows his satisfaction level with his shopping experience. Higher 
# the product value, higher satisfaction he expects and thus not adverse to travelling long distance
# or pay higher price.

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
supermarket_data_clean <- read_csv(file_path)

unique_prod_purch_sum <- with(supermarket_data_clean, unique_products_purchased_shop_1 + unique_products_purchased_shop_2 + unique_products_purchased_shop_3 + unique_products_purchased_shop_4 + unique_products_purchased_shop_5)


cus_satis_shop_1 = with(supermarket_data_clean, unique_products_purchased_shop_1/unique_prod_purch_sum)
cus_satis_shop_2 = with(supermarket_data_clean, unique_products_purchased_shop_2/unique_prod_purch_sum)
cus_satis_shop_3 = with(supermarket_data_clean, unique_products_purchased_shop_3/unique_prod_purch_sum)
cus_satis_shop_4 = with(supermarket_data_clean, unique_products_purchased_shop_4/unique_prod_purch_sum)
cus_satis_shop_5 = with(supermarket_data_clean, unique_products_purchased_shop_5/unique_prod_purch_sum)

unique_prod_purch <- data.frame(supermarket_data_clean$customer_id, cus_satis_shop_1, cus_satis_shop_2, cus_satis_shop_3, cus_satis_shop_4, cus_satis_shop_5)

qplot(supermarket_data_clean$distance_shop_1, unique_prod_purch$cus_satis_shop_1)
