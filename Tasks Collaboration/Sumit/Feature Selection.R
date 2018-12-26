## Set file path
file_path <- "Input Dataset/Cleaned Dataset/Supermarket_Data_Classification.csv"

## Read data from a file
supermarket_data_class <- read_csv(file_path)


#### Feature selection using Boruta ####
## Install the package
##install.packages("Boruta")

library(Boruta)

## Set Pseudo Random Number Generator
set.seed(1234)

## Implement Boruta
sm_boruta <- Boruta(class~., data = supermarket_data_class, doTrace = 1)

## Check the performance of Boruta
print(sm_boruta)


### Feature selection using Random Forest ###
library(randomForest)
library(caret)

## Implement Random Forest
sm_rf <- randomForest(class~., data = supermarket_data_class)

## Generate the importance value for features 
varImp(sm_rf)

## Plot the importance value of features
varImpPlot(sm_rf)