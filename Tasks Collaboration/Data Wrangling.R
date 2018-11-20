library(tidyverse)

## Read data from a .csv file
supermarket <- read_csv("/home/sumit/Data-Science-with-R/Input Dataset/SupermarketAgrCustomer.csv")



## View data
View(supermarket)
glimpse(supermarket)
header(supermarket)


##Show classes of all columns
sapply(supermarket, class)



##Summary of the data
summary(supermarket)



## Determine the number of rows and columns present in the data frame
dim(supermarket)



## Convert the data type of columns from integer to numerical
prod_purch <- as.numeric(as.character(supermarket$products_purchased))

## Replace the column in the data frame with the new vector
supermarket$products_purchased <- products_purchased



## Counting incomplete cases, (rows of a data frame where one or more columns contain NA)
missing_data = sum(!complete.cases(supermarket))

## Total number of rows
total_data = dim(supermarket)[1]

## Percentage of data missing
missing_data_percent = (missing_data/total_data) * 100
print(missing_data_percent)
print("per cent of rows have one or more columns containing 'NA' value.")