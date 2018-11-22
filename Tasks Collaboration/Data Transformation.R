library(tidyverse)
library(dplyr)

# Read cleaned dataset from the input csv file
file_path<-"Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
supermarket_data<-read_csv(file_path)

## Converts data to tbl class. and tbl’s are easier to examine than data frames
supermarket_tbl <- tbl_df(supermarket_data)
supermarket_tbl

# Select if average distance in row more then average distance in row
view1 <- arrange(filter(supermarket_tbl, avg_distance_to_shops >= mean(supermarket_tbl$avg_distance_to_shops)), desc(avg_distance_to_shops))
View(view1)

# Show percent of situations, when customers traveled a longer then average distance 
count1 <- nrow(supermarket_tbl)
count2 <- nrow(view1)
sprintf("%.0f%%", count2/count1 * 100)

# Select a columns related to the distance and product purchased
shop_distances <- supermarket_tbl[,3:7]
View(shop_distances)

shop_ordered_slice1 <- select(supermarket_tbl, 3,11,4,12,5,13,6,14,7,15)
View(shop_ordered_slice1)


# Select a columns related to the distance and unique product purchased
shop_ordered_slice2 <- select(supermarket_tbl, 3,17,4,18,5,19,6,20,7,21)
View(shop_ordered_slice2)

# Join slices by distance of shops

unordered_bind <- bind_cols(shop_ordered_slice1, shop_ordered_slice2[, grepl( "unique_products_purchased", names(shop_ordered_slice2))  ])
                                                                   #colnames(shop_ordered_slice2)%in%(c("unique_products_purchased"))])
jnd_shop_ordered_products <- unordered_bind[,c(1,2,11,3,4,12,5,6,13,7,8,14,8,9,15)]
View(jnd_shop_ordered_products)


# Count correlation between distance and product purchased 
# cor(select(shop_ordered_slice1, 1,3,5,7,9), select(shop_ordered_slice1,2,4,6,8,10))
 
# Count correlation between distance and product purchased 
#cor(select(shop_ordered_slice2, 1,3,5,7,9), select(shop_ordered_slice2,2,4,6,8,10))

# Count correlation between product purchased and unique product purchased
#cor(select(shop_ordered_slice1, 2,4,6,8,10), select(shop_ordered_slice2,2,4,6,8,10))
