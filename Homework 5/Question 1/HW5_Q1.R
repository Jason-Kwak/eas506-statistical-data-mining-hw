############################################################################################
##
## HW5 Question 1
##
## Author: Paul M Girdler
## Created: December 16, 2018
##
############################################################################################

rm(list=ls())

install.packages("Metrics")
library(ElemStatLearn)
library(class)
library(glmnet)
library(randomForest)
library(Metrics)

data(spam)
data = spam
set.seed(123);

index <- sample(x=nrow(spam), size=0.67*nrow(spam))
train <- spam[index,]
test <- spam[-index,]

spam.random=randomForest(train$spam~.,data=train,mtry=20,importance=TRUE)
spam.random

predicSpam = predict(spam.random,newdata=test)

summary(predicSpam)
summary(test$spam)

mValues <- 20
OOBErr <- matrix(0, 1,mValues)
testErr<- matrix(0, 1,mValues)


for (i in 1:mValues) {
  print(i)
  spamPredic.random=randomForest(train$spam~.,data=train,mtry=i,importance=TRUE)
  spamPredic.random
  
  predict.randomForest = predict(spamPredic.random,newdata=test)
  OOBErr[i]  <- mean(spamPredic.random$err.rate[,1])
  testErr[i] <- rmse(summary(test$spam),summary(predict.randomForest));
   
}

which(OOBErr==min(OOBErr))
which(testErr==min(testErr))
OOBErr[which(OOBErr==min(OOBErr))]
testErr[which(OOBErr==min(OOBErr))]

mValues <- 1:mValues

plot(mValues, OOBErr, ylab= "Error", type = 'l', lty = 1, xlab= "No. of randomly selected inputs, m",main = "OOB Error",col = "red")
plot(mValues, testErr, ylab= "Error", type = 'l', lty = 2, main = "RMSE Test Error",col = "blue")


