<<<<<<< HEAD

library(e1071)
library (caret)
library(rpart)

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)
supermarket_data_predict$most_pref_shop=factor(supermarket_data_predict$most_pref_shop)
str(supermarket_data_predict)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(supermarket_data_predict))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(supermarket_data_predict)), size = smp_size)

train <- supermarket_data_predict[train_ind, ]
test <- supermarket_data_predict[-train_ind, ]

svmmodel=svm(most_pref_shop ~., data=train)
print(svmmodel)
#decision tree classifier
modelr = rpart(most_pref_shop ~., data=train, method="class")

#naive bayes classifier
NBclassfier=naiveBayes(most_pref_shop ~., data=train)
#print(NBclassfier)

#function to calculate accuracy of different models
printALL=function(model,name){
  trainPred=predict(model,newdata =train, type = "class")
  trainTable=table(train$most_pref_shop, trainPred)
  testPred=predict(model, newdata=test, type="class")
  testTable=table(test$most_pref_shop, testPred) 
  trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3])/sum(testTable)
  #message("Contingency Table for Training Data")
  #message("Contingency Table for Test Data")
  #print(testTable)
  message(name)
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
printALL(NBclassfier,"naive bayes")
printALL(modelr,"decision trees")
printALL(svmmodel,"SVM")


=======

library(e1071)
library (caret)
library(rpart)

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)
supermarket_data_predict$most_pref_shop=factor(supermarket_data_predict$most_pref_shop)
str(supermarket_data_predict)
## 75% of the sample size
smp_size <- floor(0.75 * nrow(supermarket_data_predict))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(supermarket_data_predict)), size = smp_size)

train <- supermarket_data_predict[train_ind, ]
test <- supermarket_data_predict[-train_ind, ]

svmmodel=svm(most_pref_shop ~., data=train, method="class")

#decision tree classifier
modelr = rpart(most_pref_shop ~., data=train, method="class")

#naive bayes classifier
NBclassfier=naiveBayes(most_pref_shop ~., data=train)
#print(NBclassfier)

#function to calculate accuracy of different models
printALL=function(model,name){
  trainPred=predict(model,newdata =train, type = "class")
  trainTable=table(train$most_pref_shop, trainPred)
  testPred=predict(model, newdata=test, type="class")
  testTable=table(test$most_pref_shop, testPred) 
  trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3])/sum(testTable)
  #message("Contingency Table for Training Data")
  #message("Contingency Table for Test Data")
  #print(testTable)
  message(name)
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
printALL(NBclassfier,"naive bayes")
printALL(modelr,"decision trees")


>>>>>>> 6dc4b2206d136c71aba67e4d2b6099eebabcd1ad
