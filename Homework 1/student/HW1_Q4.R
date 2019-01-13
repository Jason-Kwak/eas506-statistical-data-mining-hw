###########################################################################
## This code is for Q4 Homework 1
## Paul M Girdler
## Created: Sep 09, 2018
## 
###########################################################################

#install.packages("ElemStatLearn") #if required
#install.packages("class")
#install.packages("mclust")
# this contains the zip data
library(ElemStatLearn)
library(class)
library(ggplot2)
library(mclust)

?zip.test
?zip.train

# Clear the memory
rm(list = ls())

# assign training and test data
train <- as.data.frame(zip.train)
test <- as.data.frame(zip.test)

# Look at the data

#dim(train)
#dim(test)
#names(train)
#head(train)

# NOTES from lecturer
# Without loss of generality 
# let "2" -> 0
# let "3" -> 1

# Remove indexes V1
train_new <- train[(train$V1 == 2 | train$V1 == 3 ),]

#head(train_new)
test_new <- test[(test$V1 == 2 | test$V1 == 3 ),]

###########################################################################
## Build a Linear Regression Model
##
##
###########################################################################

# NOTES from lecturer
# use round to convert continuous into discrete if required
# test error = 1/N * sum(absolute value (Ytest - Ytrue))
# the above will be one value = Misclassifciation rate

model <- lm(V1 ~ ., data = train_new)
class_function <- function(x) { 
  
  if (x < 2.5) 2 else 3 
  
}
p_lm_train <- sapply(predict(model, train_new[2:257]),class_function)
p_lm_test <- sapply(predict(model, test_new[2:257]),class_function) 
p_error_lm_train <- classError(p_lm_train,train_new[,1])$errorRate
p_error_lm_test <- classError(p_lm_test,test_new[,1])$errorRate

###########################################################################
## Build a KNN function that takes K as an argument
##
##
###########################################################################

# NOTES from lecturer
# Create a vector to store test values of K
# Use K odd numbers to avoid ties
# for i in 1:length(comp)
# KK <- comp[i]
# fit <- KNN
# testerror <- 1/N * sum missed
# store <- c(store, testerrorr)
# Need to display the result
# Error vs K
# Use "hline" function to plot a straight line of the linear error over the picture.

k <- c(1,3,5,7,9,11,13,15)

# KNN function
knn_function_test <- function(k){
  p <- knn(train_new[,2:257], test_new[,2:257], train_new[,1], k = k)
  p_error_knn_test <- classError(p,test_new[,1])$errorRate*100
  return(p_error_knn_test)
}
knn_function_train <- function(k){
  p <- knn(train_new[,2:257], train_new[,2:257], train_new[,1], k = k)
  p_error_knn_train <- classError(p,train_new[,1])$errorRate*100
  return(p_error_knn_train)
}

# Store Predicted Error
pred_error <- NULL
for (i in k){
  p_test <- knn_function_test(i)
  p_train <- knn_function_train(i)
  pred_error <- rbind(pred_error,data.frame("K" = i,
                                            "KNN_Test"= p_test,
                                            "KNN_Train" = p_train))
}

plot()

###########################################################################
## Plot the results
##
##
###########################################################################

plot(pred_error$K,pred_error$KNN_Test, type="b", pch=19, col="red", ylim=c(0,5), main = "Predicted Error vs K value", xlab="K value", ylab="Prediction Error (%)")
lines(pred_error$K,pred_error$KNN_Train, type="b", pch=19, col="blue")
axis(1, seq(1, 15, by = 2), las = 1)
legend("topleft", legend=c("Test", "Training"),
       col=c("red", "blue"), lty=1:2, cex=0.9, box.lty=1)
abline(h = 4.12, pch=19, col="red")
abline(h = 0.58, pch=19, col="blue")

# Comment on dimensionality i.e. rule of thumbk approx (N training)^(1/features)



