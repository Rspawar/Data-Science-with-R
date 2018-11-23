library(tidyverse)
library(dplyr)

## Read data from the input csv file
file_path <- "Input Dataset/Supermarket aggr.Customer.csv"
supermarket_data <- read_csv(file_path)

# Converts data to tbl class as tbl’s are easier to examine than data frames
supermarket_tbl <- tbl_df(supermarket_data)
supermarket_tbl

# Header info with data types
sapply(supermarket_tbl, class)

# Information dense summary of tbl data
glimpse(supermarket_tbl)

# View data set in spreadsheet-like display 
View(supermarket_tbl)

# Determine the number of rows and columns 
dim(supermarket_tbl)

## Data summary
summary(supermarket_tbl)

# Counting incomplete cases, (rows of a data frame where one or more columns contain NA (Should be 0))
missing_data_count = sum(!complete.cases(supermarket_tbl))
missing_data_count

# Percentage of missing or incomplete data
total_data = dim(supermarket_tbl)[1]
missing_data_percent = (missing_data/total_data) * 100
missing_data_percent

# Remove incomplete records (NA) from the data
supermarket_tbl_clean <- na.omit(supermarket_tbl)
na.action(supermarket_tbl_clean)
View(supermarket_tbl_clean)

# Remove duplicate rows
distinct(supermarket_tbl_clean)

## Plot the count of feature based on different data types
data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
}
data_types(supermarket_tbl_clean)

## Convert the data type of columns from integer to numerical
supermarket_tbl_clean <- supermarket_tbl_clean %>% mutate_if(is.integer, as.numeric)

## Round-off the numerical valus to 4 digits after the decimal (Approach 1)
supermarket_tbl_clean <- sapply(supermarket_tbl_clean, round, 4)

# Round the decimal value columns upto 4 decimal places (Approach 2)
is.num <- sapply(supermarket_tbl_clean, is.numeric)
supermarket_tbl_clean[is.num] <- lapply(supermarket_tbl_clean[is.num], round, 4)
View(supermarket_tbl_clean) 

# Rename column names
## From products_purchased to products_purchased_total
names(supermarket_tbl_clean)[names(supermarket_tbl_clean) == 'products_purchased'] <- 'products_purchased_total'

## From shops_used to shops_used_total
names(supermarket_tbl_clean)[names(supermarket_tbl_clean) == 'shops_used'] <- 'shops_used_total'

## From amount_purchased to amount_purchased_total
names(supermarket_tbl_clean)[names(supermarket_tbl_clean) == 'amount_purchased'] <- 'amount_purchased_total'

# Reorder Columns
supermarket_tbl_clean <- supermarket_tbl_clean[c(1,10,11,12,13,14,15,3,4,2,16,17,18,19,20,5,21,22,23,24,25,6,36,37,38,39,40,9,26,27,28,29,30,7,31,32,33,34,35,8)]

# Write the cleaned data tbl to csv
clean_filepath = "~/R GitHub/Data-Science-with-R/Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"
write.csv(supermarket_tbl_clean, file = clean_filepath, row.names = FALSE)

##### Data Cleaning Ends. supermarket_tbl_clean is the cleaned data tbl 

##### Data Statistics

# Subset Observations
count1 <- count(filter(supermarket_tbl_clean, shops_used_total > 2))
count1 # Gives a count of higher than average i.e.2

count2 <- count(filter(supermarket_tbl_clean, shops_used_total < 2))
count2 # Gives a count of lower than average i.e.2

count3 <- count(filter(supermarket_tbl_clean, shops_used_total == 2))
count3 # Gives a count of equal to average i.e.2

total_count <- count1 + count2 +count3 # Is the total number of rows

# Select columns related to shop distance and products purchased
slice1 <- select(supermarket_tbl_clean, 3,4,5,6,7,11,12,13,14,15)
View(slice1)

# select columns related to shops used and customer id
slice2 <- select(supermarket_tbl_clean, 1,2)
View(slice2)

# Customer Segmentation in terms of total amount purchased
mean_value<-mean(supermarket_tbl_clean$amount_purchased_total)
mean_value

count4<-count(filter(supermarket_tbl_clean, amount_purchased_total > mean_value))
count4 # Gives a count of higher than average
slice3<-(filter(supermarket_tbl_clean, customer_id, amount_purchased_total > mean_value))
Segment1<-arrange(slice3, desc(amount_purchased_total))
View(Segment1)

count5<-count(filter(supermarket_tbl_clean, amount_purchased_total < mean_value))
count5 # Gives a count of lower than average
slice4<-(filter(supermarket_tbl_clean, customer_id, amount_purchased_total < mean_value))
Segment2<-arrange(slice4, desc(amount_purchased_total))
View(Segment2)

count6<-count(filter(supermarket_tbl_clean, amount_purchased_total == mean_value))
count6 # Gives a count of equal to average 
slice5<-(filter(supermarket_tbl_clean, customer_id, amount_purchased_total == mean_value))
View(slice5)
Segment3<-arrange(slice5, desc(amount_purchased_total))
View(Segment3)

# Find the classes (bins/categories) in the data

## Finds the unique categories with their frequency for products_purchased
#products<-table(supermarket_tbl_clean$products_purchased)
#products

## Find the number of unique bins/categories in the data for products_purchased
#products_length<-length(table(supermarket_tbl_clean$products_purchased))
#products_length

## Finds the unique categories with their frequency for unique_products_purchased
#uniqueproducts<-table(supermarket_tbl_clean$unique_products_purchased)
#uniqueproducts

## Find the number of unique bins/categories in the data for unique_products_purchased
#uniqueproducts_length<-length(table(supermarket_tbl_clean$unique_products_purchased))
#uniqueproducts_length

## Finds the unique categories with their frequency for shops_used
#shops<-table(supermarket_tbl_clean$shops_used)
#shops

## Find the number of unique bins/categories in the data for unique_products_purchased
#shops_length<-length(table(supermarket_tbl_clean$shops_used))
#shops_length















