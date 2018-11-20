library(tidyverse)

## Read data from a .csv file
supermarket <- read_csv("/Home/Data-Science-with-R/Input Dataset/SupermarketAgrCustomer.csv")
Supermarket_aggr_Customer <- read_csv("Input Dataset/Supermarket aggr.Customer.csv")

##convert all non-numeric values id to numeric
Supermarket_aggr_Customer$customer_id<-as.integer(Supermarket_aggr_Customer$customer_id)

##remove incomplete records from the data
Supermarket_aggr_Customer_complete<-na.omit(Supermarket_aggr_Customer)
na.action(Supermarket_aggr_Customer_complete)


## View data
View(Supermarket_aggr_Customer)
View(Supermarket_aggr_Customer_complete)
View(supermarket)
glimpse(Supermarket_aggr_Customer)


##Summary of the data
summary(supermarket)
summary(Supermarket_aggr_Customer)


## Determine the number of rows and columns present in the data frame
dim(supermarket)
dim(Supermarket_aggr_Customer)


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
