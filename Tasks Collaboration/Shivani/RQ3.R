library(caret)
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine 
library(pROC)	
library(e1071)
library(rpart)
library(readr)
library(tidyverse)
library(rpart.plot)
library(glmnet)
library(msaenet)

file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)
supermarket_data_predict$most_pref_shop=factor(supermarket_data_predict$most_pref_shop)
str(supermarket_data_predict)

#stratified k fold validation(5)

set.seed(123)
folds <- cut(seq(1,nrow(supermarket_data_predict)),breaks=5,labels=FALSE)
#5 folds 5 different train-test dataset combinations
for(i in 1:5){
  #Segement your data by fold using the which() function 
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- supermarket_data_predict[testIndexes, ]
  train <- supermarket_data_predict[-testIndexes, ]
  
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
  NBclassfier=naiveBayes(most_pref_shop ~., data=train,laplace=3)
  modelr<-rpart(most_pref_shop ~., data=train, method="class",control=rpart.control(cp=0.0001))
  printALL(NBclassfier,"naive bayes")
  printALL(modelr,"decision trees")
  
  
}



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

#glmnet

x <- model.matrix(most_pref_shop ~ 0 + . , data=train)
cvfit<- cv.glmnet(x, y=rnorm(train$most_pref_shop))
plot(cvfit)
print(cvfit$lambda.min)
t                             <- 0.2*nrow(train) #20% of data
t                             <- as.integer(t)
glmnetmodel<-cv.glmnet(x, y=rnorm(train$most_pref_shop), alpha=1,lambda=seq(0.0001, 1, length = 10))
predicted_y<-predict(glmnetmodel, newx = x[1:5,], s = "lambda.min",type="class")
Yactual                       <- train[1:t, ncol(train)]
confusion_matrix              <- ftable(Yactual, predicted_y)
accuracy                      <- (sum(diag(confusion_matrix)) / length(predicted_y))
print(accuracy)
