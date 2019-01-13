############################################################################################
##
## HW3 Question 1
##
## Author: Paul M Girdler
## Created: October 28, 2018
##
############################################################################################

# Clear the memory
rm(list = ls())

# Set working directory
setwd("~/Course Material/EAS506 - Stat Data Mining/Homework/Homework 3/Question 1")

# Install packages if required
#install.packages("caret")
#install.packages("MASS")
#install.packages("class")
#install.packages("mclust")
#install.packages("pls")

library("MASS")
library("caret") 
library("class")
library("mclust")
library("pls")

boston <- Boston

# Predict whether a given suburb has a crime rate above or below the median

# Calculate the median crime rate in Boston

median_crime_rate <- median(boston$crim)
median_crime_rate

crim <- cut(boston$crim, breaks = c(0,median_crime_rate,100))
crim <- as.numeric(crim)-1

# Create a crim cat

#  0 = below median crim rate
#  1 = above median crim rate

#histogram(crim)

# perfect 50/50 class split as one would expect

# Want to standardize data

data <- boston

# Create new data set with crim = class

crim <- as.data.frame(crim)
data$crim <- crim

data <- cbind(data[,1], scale(data[,-1]))

# double checked data against boston etc before removing

remove(crim)
remove(boston)

############################################################################################
# Create a training and test set
############################################################################################

set.seed(123)
train <- sample(1:nrow(data), .80*nrow(data))
data_train <- data[train,]
data_test <- data[-train,]

y_true_train <- as.numeric(data_train$crim)
y_true_test <- as.numeric(data_test$crim)

###########################################################################################
##
## PCA - Looking to examine what factors are involved in the most dominant PC
##
###########################################################################################

pcr.fit = pcr(crim~., data = data, scale = TRUE, validation = "CV")
# Validation = "none" i.e. No cross validation

summary(pcr.fit)

pcr.fit$loadings

# The first 4 PC appear to be linear combinations of a number of variables.
# This does not help interpretibility much.

plot(pcr.fit)

validationplot(pcr.fit, val.type = "MSEP")

# Data looks like it can be adequately explained by 2 to 4 Principle Components

###########################################################################################
##
## Logistic Regression
##
###########################################################################################

regfit.full <- regsubsets(crim~., data = data_train, nbest = 1, nvmax = 14, method = "exhaustive")
summary(regfit.full)
my_sum <- summary(regfit.full)

#########################################
##  Calculate the error rates
#########################################

select = summary(regfit.full)$outmat
lr_train_mae<-NULL # store training error
lr_test_mae<-NULL  # store test error

# NOTE: Prediction given as a probability.
# Round to give a discrete class.

for (i in 1:13){
  temp <- which(select[i,] == "*")
  temp <- temp + 1 # account for the response variable and shift across 1
  
  red.training <- data_train[, c(1,temp)] # keeping only important variables in a subset
  red.testing <- data_test[,c(1,temp)]
  
  red.fit <- glm(crim ~., data = red.training, family = "binomial")
  
  pred.train = round(predict(red.fit, newdata = red.training, type = "response"))
  pred.test = round(predict(red.fit, newdata = red.testing, type = "response"))
  
  lr_train_mae[i] <- mean(abs(pred.train - y_true_train))
  lr_test_mae[i]  <-  mean(abs(pred.test - y_true_test))

}

x11()
par(mfrow=c(1,2))
plot(lr_train_mae, type="l", 
     main ="LOG R MAE Training - K predictors", 
     ylab ="Mean Absolute Error",
     xlab = "No. of Predictors",
     ylim = c(0,0.20))
plot(lr_test_mae, type="l",
     main ="LOG R MAE Test - K predictors", 
     ylab ="Mean Absolute Error",
     xlab = "No. of Predictors",
     ylim = c(0,0.20))

which(lr_train_mae==min(lr_train_mae)) # 12
which(lr_test_mae==min(lr_test_mae))   # Models greater than 10

# Chose model of 12 predictors

i = 12

temp <- which(select[i,] == "*")
temp <- temp + 1 # account for the response variable and shift across 1

red.training <- data_train[, c(1,temp)] # keeping only important variables in a subset
red.testing <- data_test[,c(1,temp)]

lr_train_mae[12] #0.08168317
lr_test_mae[12] #0.1176471

lr.fit <- glm(crim ~., data = red.training, family = "binomial")
summary(lr.fit)

y_hat_train = round(predict(lr.fit, newdata = red.training, type = "response"))
y_hat_test = round(predict(lr.fit, newdata = red.testing, type = "response"))

#########################################
#  Confusion Matrix
#########################################

lr_conf_train <- confusionMatrix(as.factor(y_hat_train), as.factor(y_true_train))
lr_conf_train$table

lr_conf_test <- confusionMatrix(as.factor(y_hat_test), as.factor(y_true_test))
lr_conf_test$table

###########################################################################################
##
## Linear Discriminant Analysis (LDA)
##
###########################################################################################

#########################################
##  Calculate the error rates
#########################################

lda_train_mae<-NULL # store training error
lda_test_mae<-NULL  # store test error

# NOTE: Prediction given as a probability.
# Round to give a discrete class.

for (i in 1:13){
  temp <- which(select[i,] == "*")
  temp <- temp + 1 # account for the response variable and shift across 1
  
  red.training <- data_train[, c(1,temp)] # keeping only important variables in a subset
  red.testing <- data_test[,c(1,temp)]
  
  red.fit <- lda(crim ~., data = red.training)
  
  pred.train = predict(red.fit, newdata = red.training, type = "response")
  pred.test = predict(red.fit, newdata = red.testing, type = "response")
  
  y_hat_train <- as.numeric(pred.train$class)-1
  y_hat_test <- as.numeric(pred.test$class)-1
  
  lda_train_mae[i] <- mean(abs(y_hat_train - y_true_train))
  lda_test_mae[i]  <-  mean(abs(y_hat_test - y_true_test))
}

x11()
par(mfrow=c(1,2))
plot(lda_train_mae, type="l", 
     main ="LDA MAE Training - K predictors", 
     ylab ="Mean Absolute Error",
     xlab = "No. of Predictors",
     ylim = c(0,0.20))
plot(lda_test_mae, type="l",
     main ="LDA MAE Test - K predictors", 
     ylab ="Mean Absolute Error",
     xlab = "No. of Predictors",
     ylim = c(0,0.20))

which(lda_train_mae==min(lda_train_mae)) # 4
which(lda_test_mae==min(lda_test_mae))   # 5

# Chose model of 4 predictors

i = 4

temp <- which(select[i,] == "*")
temp <- temp + 1 # account for the response variable and shift across 1

red.training <- data_train[, c(1,temp)] # keeping only important variables in a subset
red.testing <- data_test[,c(1,temp)]

lda.fit <- lda(crim ~., data = red.training)

lda.fit$scaling

pred.train = predict(lda.fit, newdata = red.training, type = "response")
pred.test = predict(lda.fit, newdata = red.testing, type = "response")

lda_train_mae[4]
lda_test_mae[4]

y_hat_train <- as.numeric(pred.train$class)-1
y_hat_test <- as.numeric(pred.test$class)-1

#########################################
#  Confusion Matrix
#########################################

lda_conf_train <- confusionMatrix(as.factor(y_hat_train), as.factor(y_true_train))
lda_conf_train$table

lda_conf_test <- confusionMatrix(as.factor(y_hat_test), as.factor(y_true_test))
lda_conf_test$table

###########################################################################################
##
## KNN
##
###########################################################################################

#########################################
##  Calculate the error rates
#########################################

knn_train_error<-NULL # store training error
knn_test_error<-NULL  # store test error

for (i in 1:13){

knn.fit <- knn(data_train[,-1], data_train[,-1], data_train[,1], k = i, l = 0, prob = FALSE)

knn_train_error[i] <- classError(knn.fit,data_train[,1])$errorRate

knn.fit <- knn(data_train[,-1], data_test[,-1], data_train[,1], k = i, l = 0, prob = FALSE)

knn_test_error[i] <- classError(knn.fit,data_test[,1])$errorRate

}

x11()
par(mfrow=c(1,2))
plot(knn_train_error, type="l", 
     main ="KNN Error Rate Training - K", 
     ylab ="Error Rate",
     xlab = "K",
     ylim = c(0,0.20))
plot(knn_test_error, type="l",
     main ="KNN Error Rate  Test - K", 
     ylab ="Error Rate",
     xlab = "K",
     ylim = c(0,0.20))

#########################################
#  Confusion Matrix
#########################################

# Pick KNN = K = 1
knn.fit <- knn(data_train[,-1], data_train[,-1], data_train[,1], k = 1, l = 0, prob = FALSE, use.all = TRUE)

cat("Confusion matrix:\n")
xtab = table(knn.fit, y_true_train)
print(xtab)

knn_train_error[1] # 0

knn.fit <- knn(data_train[,-1], data_test[,-1], data_train[,1], k = 1, l = 0, prob = FALSE, use.all = TRUE)

cat("Confusion matrix:\n")
xtab = table(knn.fit, y_true_test)
print(xtab)

knn_test_error[1] # 0.0392
