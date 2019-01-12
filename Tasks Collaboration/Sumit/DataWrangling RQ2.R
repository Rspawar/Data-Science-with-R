## Call the packages
library(tidyverse)
library(dplyr)

## Set file path
file_path <- "Input Dataset/Cleaned Dataset/Supermarket_DataCleaned.csv"

## Read data from a file
supermarket_data_clean <- read_csv(file_path)

## List of all features present in the data frame
all_features <- colnames(supermarket_data_clean)

## List of selected features
sel_features <- all_features[-c(1,2,8,9,10,16,22,28,34,35,36,37,38,39,40)]

## Create a data frame with only the selected features
supermarket_data_model <- supermarket_data_clean %>% select(sel_features)

## Generate new columns to find the closest shop, shops from which most products are purchased, shops from which most unique products are purchased, 
## shops offfering least average product price and shops at which maximum amount of money is spent by a customers.  
supermarket_data_model$min_dist <- str_sub(colnames(supermarket_data_model[,1:5]),-1,-1)[apply(supermarket_data_model[,1:5],1,which.min)]
supermarket_data_model$most_prod_purch_from <- str_sub(names(supermarket_data_model[,6:10]),-1,-1)[max.col(supermarket_data_model[,6:10], "last")]
supermarket_data_model$most_uni_prod_purch_from <- str_sub(names(supermarket_data_model[,11:15]),-1,-1)[max.col(supermarket_data_model[,11:15], "last")]
supermarket_data_model$least_avg_prod_pri <- str_sub(colnames(supermarket_data_model[,16:20]),-1,-1)[apply(supermarket_data_model[,16:20],1,which.min)]
supermarket_data_model$max_amt_purch <- str_sub(names(supermarket_data_model[,21:25]),-1,-1)[max.col(supermarket_data_model[,21:25], "last")]

## Create a data frame having only the newly generated columns
test <- supermarket_data_model[,26:30]

## Gnereate new columns - most preferred shop and categorise the customers based on the reasons
for (row in 1:nrow(test)){
  ## Create a vector for each row
  vec <- c(test[row, "most_prod_purch_from"], test[row, "most_uni_prod_purch_from"], test[row, "least_avg_prod_pri"], test[row, "max_amt_purch"])
  
  ## Sort and find the most preferred shop
  supermarket_data_model[row, "most_pref_shop"] <- names(sort(summary(as.factor(unlist(vec))), decreasing=T)[1:1])
  
  ## Assign lables to customers (0 or 'dist' - '' and 1 or 'yes'- 'Willing to travel far for shopping')
  if (supermarket_data_model[row, "min_dist"] == supermarket_data_model[row, "most_pref_shop"] && supermarket_data_model[row, "least_avg_prod_pri"] == supermarket_data_model[row, "most_pref_shop"] && supermarket_data_model[row, "most_uni_prod_purch_from"] == supermarket_data_model[row, "most_pref_shop"]){
    supermarket_data_model[row, "factor"] <- 6
  }
  else if (supermarket_data_model[row, "min_dist"] == supermarket_data_model[row, "most_pref_shop"] && supermarket_data_model[row, "least_avg_prod_pri"] == supermarket_data_model[row, "most_pref_shop"]){
    supermarket_data_model[row, "factor"] <- 3
  }
  else if (supermarket_data_model[row, "min_dist"] == supermarket_data_model[row, "most_pref_shop"] && supermarket_data_model[row, "most_uni_prod_purch_from"] == supermarket_data_model[row, "most_pref_shop"]){
    supermarket_data_model[row, "factor"] <- 4
  }
  else if (supermarket_data_model[row, "least_avg_prod_pri"] == supermarket_data_model[row, "most_pref_shop"] && supermarket_data_model[row, "most_uni_prod_purch_from"] == supermarket_data_model[row, "most_pref_shop"]){
    supermarket_data_model[row, "factor"] <- 5
  }
  else if (supermarket_data_model[row, "min_dist"] == supermarket_data_model[row, "most_pref_shop"]){
    supermarket_data_model[row, "factor"] <- 0
  }
  else if (supermarket_data_model[row, "least_avg_prod_pri"] == supermarket_data_model[row, "most_pref_shop"]){
    supermarket_data_model[row, "factor"] <- 1
  }
  else if (supermarket_data_model[row, "most_uni_prod_purch_from"] == supermarket_data_model[row, "most_pref_shop"]){
    supermarket_data_model[row, "factor"] <- 2
  }
  else{
    #supermarket_data_model[row, "factor"] <- 0
  }
}

## Re-order columns
supermarket_data_clus <- supermarket_data_model[c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,32)]

## Set file path 
clean_filepath = "~/Data-Science-with-R/Input Dataset/Cleaned Dataset/Supermarket_Data_Clustering.csv"

## Write the dataframe to csv file
write.csv(supermarket_data_clus, file = clean_filepath, row.names = FALSE)
