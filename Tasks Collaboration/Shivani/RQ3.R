library(caret)
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine 
library(pROC)	
library(e1071)
library(rpart)
library(readr)
library(tidyverse)
library(rpart.plot)
library(neuralnet)

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)
supermarket_data_predict$most_pref_shop=factor(supermarket_data_predict$most_pref_shop)
str(supermarket_data_predict)

#stratified k-fold(5)
flds <- createFolds(factor(supermarket_data_predict$most_pref_shop), k = 5, list = TRUE, returnTrain = TRUE)
comb_factor <- tbl_df(cbind(supermarket_data_predict, flds))
train_folders <- c(1,3,4)
test_folders <- c(2,5)
#test train data separation
train <- supermarket_data_predict[comb_factor$flds %in% train_folders,]
test <- supermarket_data_predict[comb_factor$flds %in% test_folders,]
View(train)


#hypertuning rpart
obj3 <- tune.rpart(most_pref_shop~., data =train, minsplit = c(5,10,15))
summary(obj3)
#decision tree classifier
modelr<-rpart(most_pref_shop ~., data=train, method="class",control=rpart.control(cp=0.001))
print(modelr$cptable)
plotcp(modelr)
print(modelr)
prp(modelr)

#the relative error is reduced but gaph is overplotted
modelr<-rpart(most_pref_shop ~., data=train, method="class",control=rpart.control(cp=0.0001))
print(modelr$cptable)

#naive bayes classifier
#hypertuning isn't recommended for naive bayes because the overall performance is affected by just one parameter i.e laplace
NBclassfier=naiveBayes(most_pref_shop ~., data=train,laplace=3)
print(NBclassfier)

#neural network
m<-model.matrix(most_pref_shop,data=train)
NN <-neuralnet(most_pref_shop ~ distance_avg+products_purchased_avg,m, hidden = 3 , linear.output = T )

# plot neural network
plot(NN)
#function to calculate accuracy of different models
printALL=function(model,name){
  trainPred=predict(model,newdata =train, type = "class")
  trainTable=table(train$most_pref_shop, trainPred)
  testPred=predict(model, newdata=test, type="class")
  testTable=table(test$most_pref_shop, testPred) 
  trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3])/sum(testTable)
  message(name)
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
printALL(NBclassfier,"naive bayes")
printALL(modelr,"decision trees")




