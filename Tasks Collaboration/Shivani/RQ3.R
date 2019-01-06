library(tidyverse)
library(ggplot2)
library("RColorBrewer")
library(readr)
library(dplyr)
library(e1071)
library (caret)

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)
supermarket_data_predict$most_pref_shop=factor(supermarket_data_predict$most_pref_shop)
str(supermarket_data_predict)
#split data into 70% training dataset and 30% test dataset
## set the seed to make your partition reproducible
set.seed(7267166)
trainIndex=createDataPartition(supermarket_data_predict$most_pref_shop, p=0.7)$Resample1
train=mydata[trainIndex, ]
test=mydata[-trainIndex, ]


#naive bayes classifier
NBclassfier=naiveBayes(most_pref_shop~., data=train)
print(NBclassfier)
trainPred=predict(NBclassfier,newdata =train, type = "class")
trainTable=table(train$most_pref_shop, trainPred)
testPred=predict(NBclassfier, newdata=test, type="class")
testTable=table(test$most_pref_shop, testPred) 
trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3])/sum(trainTable)
testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3])/sum(testTable)
message("Contingency Table for Training Data")
message("Contingency Table for Test Data")
print(testTable)
message("Accuracy")
print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))


