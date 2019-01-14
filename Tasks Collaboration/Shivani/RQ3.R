<<<<<<< HEAD
library(caret)
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine 
library(pROC)	
library(e1071)
library(rpart)
library(readr)
library(tidyverse)
library(rpart.plot)


file_path<- "Input Dataset/Cleaned Dataset/Supermarket_Data_Prediction.csv"
supermarket_data_predict <- read_csv(file_path)
View(supermarket_data_predict)
supermarket_data_predict$most_pref_shop=factor(supermarket_data_predict$most_pref_shop)
str(supermarket_data_predict)

#stratified k fold validation(5)

set.seed(123)
folds <- cut(seq(1,nrow(supermarket_data_predict)),breaks=5,labels=FALSE)
#to store accuracy,sensitivity ,specificity for naive bayes and decision tree
resnb<-matrix(ncol=3, nrow=5)
resdec<-matrix(ncol=3, nrow=5)
#5 folds 5 different train-test dataset combinations
for(i in 1:5){
  #Segement your data by fold using the which() function 
  
  testIndexes <- which(folds==i,arr.ind=TRUE)
  test <- supermarket_data_predict[testIndexes, ]
  train <- supermarket_data_predict[-testIndexes, ]
  ytrain<-train%>%pull(most_pref_shop)
  ytest<-test%>%pull(most_pref_shop)
  #function to calculate accuracy of different models
  printALL=function(model,name,result){
    print(name)
    testPred=predict(model, newdata=test, type="class")
    conftest<-confusionMatrix(ytest,testPred,"YES")
    print(conftest$overall["Accuracy"])
    #print(conftest)
    result[i,1]= conftest$overall["Accuracy"]
    print("Sensitivity")
    print(max(conftest$byClass[,"Sensitivity"]))
    result[i,2]=max(conftest$byClass[,"Sensitivity"])
    print("Specificity")
    print(max(conftest$byClass[,"Specificity"]))
    result[i,3]=max(conftest$byClass[,"Specificity"])
    return(result)
  }
  
  NBclassfier=naiveBayes(most_pref_shop ~., data=train,laplace=3)
  modelr<-rpart(most_pref_shop ~., data=train, method="class",control=rpart.control(cp=0.0001))
  resnb=printALL(NBclassfier,"naive bayes",resnb)
  resdec=printALL(modelr,"decision trees",resdec)
  
  
}

#cross-validation accuracy plot
y<-list(resnb[1,1],resnb[2,1],resnb[3,1],resnb[4,1],resnb[5,1])
ynew<-list(resdec[1,1],resdec[2,1],resdec[3,1],resdec[4,1],resdec[5,1])
x<-list(1,2,3,4,5)
xaxis<-unlist(x)
plot(xaxis,unlist(y),type="l" ,lwd=2,col="red",xlab="Folds",ylab="Accuracy",ylim=range( c(y, ynew) ),main="Cross Validation - Accuracy")
lines(xaxis,unlist(ynew),type="l",lwd=2,col="green")
legend("topleft", 
       legend = c("Naive Bayes", "Decision Tree"),lwd=2,col=c("red","green"))

#cross-validation sensitivity plot
y<-list(resnb[1,2],resnb[2,2],resnb[3,2],resnb[4,2],resnb[5,2])
ynew<-list(resdec[1,2],resdec[2,2],resdec[3,2],resdec[4,2],resdec[5,2])
x<-list(1,2,3,4,5)
xaxis<-unlist(x)
plot(xaxis,unlist(y),type="l" ,lwd=2,col="red",xlab="Folds",ylab="Sensitivity",ylim=range( c(y, ynew) ),main="Cross Validation - Sensitivity")
lines(xaxis,unlist(ynew),type="l",lwd=2,col="green")
legend("topleft", 
       legend = c("Naive Bayes", "Decision Tree"),lwd=2,col=c("red","green"))

#cross-validation specificity plot
y<-list(resnb[1,3],resnb[2,3],resnb[3,3],resnb[4,3],resnb[5,3])
ynew<-list(resdec[1,3],resdec[2,3],resdec[3,3],resdec[4,3],resdec[5,3])
x<-list(1,2,3,4,5)
xaxis<-unlist(x)
plot(xaxis,unlist(y),type="l" ,lwd=2,col="red",xlab="Folds",ylab="Specificity",ylim=range( c(y, ynew) ),main="Cross Validation - Specificity")
lines(xaxis,unlist(ynew),type="l",lwd=2,col="green")
legend("topleft", 
       legend = c("Naive Bayes", "Decision Tree"),lwd=2,col=c("red","green"))





#hypertuning rpart
obj3 <- tune.rpart(most_pref_shop~., data =train, minsplit = c(5,10,15))
summary(obj3)
#decision tree classifier
modelr<-rpart(most_pref_shop ~., data=train, method="class",control=rpart.control(cp=0.001))
print(modelr$cptable)
plotcp(modelr)
print(modelr)
prp(modelr,main="Decision tree")

#the relative error is reduced but gaph is overplotted
modelr<-rpart(most_pref_shop ~., data=train, method="class",control=rpart.control(cp=0.0001))
print(modelr$cptable)

#naive bayes classifier
#hypertuning isn't recommended for naive bayes because the overall performance is affected by just one parameter i.e laplace
NBclassfier<-naiveBayes(most_pref_shop ~., data=train,laplace=3)
print(NBclassfier)


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


>>>>>>> 5bd6c20ea30814287ac8323188f8ed06ff618c96
