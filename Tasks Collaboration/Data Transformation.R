library(tidyverse)

# Read cleaned dataset from the input csv file
file_path<-"Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
cleared_supermarket_data<-read_csv(file_path)

## Converts data to tbl class. and tbl’s are easier to examine than data frames
cleared_supermarked_tbl <- tbl_df(cleared_supermarket_data)
cleared_supermarked_tbl

# Select if average distance in row more then average distance in row
view1 <- cleared_supermarked_tbl %>% 
  filter(avg_distance_to_all_shops >= mean(cleared_supermarked_tbl$avg_distance_to_all_shops)) %>%
  arrange(desc(avg_distance_to_all_shops))
View(view1)

# Show percent of situations, when customers traveled a longer then average distance 
# Research question 1
count1 <- nrow(cleared_supermarked_tbl)
count2 <- nrow(view1)
sprintf("%.0f%%", count2/count1 * 100)

# Select a columns related to the distance and product purchased
shop_distances <- cleared_supermarked_tbl[,3:7]
View(shop_distances)

shop_ordered_slice1 <- select(cleared_supermarked_tbl, 3,11,4,12,5,13,6,14,7,15)
View(shop_ordered_slice1)


# Select a columns related to the distance and unique product purchased
shop_ordered_slice2 <- select(cleared_supermarked_tbl, 3,17,4,18,5,19,6,20,7,21)
View(shop_ordered_slice2)

# Join slices by distance of shops
unordered_bind <- shop_ordered_slice1 %>% 
  bind_cols(shop_ordered_slice2[, grepl( "unique_products_purchased", names(shop_ordered_slice2))  ])
                                                                   
jnd_shop_ordered_products <- unordered_bind[,c(1,2,11,3,4,12,5,6,13,7,8,14,8,9,15)]
View(jnd_shop_ordered_products)

# Select segment from shops and prices
shop_ordered_slice3 <- select(cleared_supermarked_tbl, 3,23,4,24,5,25,6,26,7,27)
View(shop_ordered_slice3)

# Select segment from shops and average purchase
shop_ordered_slice4 <- select(cleared_supermarked_tbl, 3,29,4,30,5,31,6,32,7,33)
View(shop_ordered_slice4)