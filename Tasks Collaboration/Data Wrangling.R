library(tidyverse)

## Read data from a .csv file
supermarket <- read_csv("Input Dataset/Supermarket aggr.Customer.csv")



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



## Plot the count of feature based on different data types
data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
}

data_types(supermarket)



## Convert the data type of columns from integer to numerical
supermarket <- supermarket %>% mutate_if(is.integer, as.numeric)



## Counting incomplete cases, (rows of a data frame where one or more columns contain NA)
missing_data = sum(!complete.cases(supermarket))

## Total number of rows
total_data = dim(supermarket)[1]

## Percentage of data missing
missing_data_percent = (missing_data/total_data) * 100
print(missing_data_percent)
print("per cent of rows have one or more columns containing 'NA' value.")



## Remove incomplete records from the data
supermarket <- na.omit(supermarket)
na.action(supermarket)