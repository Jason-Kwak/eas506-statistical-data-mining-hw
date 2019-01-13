############################################################################################
##
## HW4 Question 2
##
## Author: Paul M Girdler
## Created: November 25, 2018
##
############################################################################################

# Clear the memory
rm(list = ls())

# Set working directory
setwd("~/Course Material/EAS506 - Stat Data Mining/Homework/Homework 4/Question 2")

#install.packages("caret")
#install.packages("ISLR")
#install.packages("MASS")
#install.packages ("e1071")
#install.packages("caTools")
#install.packages("leaps")
#install.packages("rpart")
#install.packages("randomForest")
#install.packages("gbm")
#install.packages("geneplotter")
library("caret")
library("ISLR")
library("MASS")
library("e1071")
library("caTools")
library("leaps")
library("rpart")
library("randomForest")
library("gbm")
library("geneplotter")

#########################################
# Create a training and test set
#########################################

data1 <- read.csv("wine.csv")
data2 <- data.frame(data1)

wine <-data2[,-c(2,7,8)]

dim(wine) #177 11

set.seed(123123)

train <-sample(1:nrow(wine),nrow(wine)*0.70)
test <- setdiff(1:nrow(wine),train)

trainDF = wine[train, ]
dim(trainDF) #123 11
testDF = wine[-train, ]
dim(testDF) #54 11

train_y <- trainDF[[1]]

train_x <- trainDF[,-1]
dim(train_x) #123 10
head(train_x)

test_y <- testDF[[1]]
test_y <- as.numeric(test_y)

test_x <- testDF[,-1]
dim(test_x) #54 10
head(test_x)

###########################################
# Single tree
##########################################

set.seed(123123)
model.control <- rpart.control(minsplit = 3, xval=10, cp=0)
fit <- rpart(X1~.,data = trainDF, method = "class", control = model.control)

plot(fit, main = " Training (Pre-pruning)")
text(fit, use.n = TRUE,cex = .8)

# Prune the tree

min_cp = which.min(fit$cptable[,4]) 
min_cp #6

plot(fit$cptable[,4],main = "Cp for model selection", ylab = "CV error")


prune_fit <- prune(fit, cp = fit$cptable[min_cp,1])

plot(prune_fit, main = "Training (Post-pruning)")
text(prune_fit,use.n=TRUE , cex=.8)

#test error of single pruned tree

my_pred <- predict(prune_fit, newdata = testDF, type = "class")
y_hat <- as.numeric(my_pred)

misclass_tree <- sum(abs(test_y - y_hat))/length(y_hat)
misclass_tree #0.167

################################################
## train  data count on prune_fit tree
##############################################

set.seed(123123)

# Count number of training obeservations in nodes

nodes_position <- prune_fit
nodes_position$frame$yval = as.numeric(rownames(nodes_position$frame))

train_nodes <- predict(nodes_position, trainDF, type = "vector")
trainNodeDf = data.frame(row_count = c(1:length(train_nodes)), train_nodes)
trainNodeData = data.frame(aggregate(row_count~train_nodes,data = trainNodeDf,FUN = length))

wineTreePred_train <- as.numeric(predict(prune_fit, newdata = trainDF, type= 'class'))
wineTreePred_test <- as.numeric(predict(prune_fit, newdata = testDF, type = 'class'))

train_err <- mean(abs(train_y - wineTreePred_train))
test_err <- mean(abs(test_y - wineTreePred_test))

train_err #0.0163
test_err #0.167

# We see how many training observations land in each node

trainNodeData

#########################################
# test data count on prune_fit tree
#########################################

test_nodes <- predict(nodes_position, testDF, type="vector")
testNodeDf = data.frame(row_count = c(1:length(test_nodes)), test_nodes)
testNodeData = data.frame(aggregate(row_count~test_nodes,data = testNodeDf,FUN = length))

# We see how many test observations land in each node

testNodeData