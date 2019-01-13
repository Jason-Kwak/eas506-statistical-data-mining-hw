############################################################################################
# Paul M Girdler 28/09/18
# Homework 2
# Question 1
#
############################################################################################
#
# 1) (10 points) (Exercise 9 modified, ISL) In this exercise, we will predict the number
# of applications received using the other variables in the College data set in the ISLR
# package.
# (a) Split the data set into a training set and a test set. Fit a linear model using least
# squares on the training set, and report the test error obtained.
# (b) Fit a ridge regression model on the training set, with ?? chosen by crossvalidation.
# Report the test error obtained.
# (d) Fit a lasso model on the training set, with ?? chosen by crossvalidation.
# Report the test error obtained, along with the number of non-zero coefficient
# estimates.
# (e) Fit a PCR model on the training set, with k chosen by cross-validation. Report the
# test error obtained, along with the value of k selected by cross-validation.
# (f) Fit a PLS model on the training set, with k chosen by crossvalidation.
# Report the test error obtained, along with the value of k selected by cross-validation.
# (g) Comment on the results obtained. How accurately can we predict the number of
# college applications received? Is there much difference among the test errors resulting
# from these five approaches?

rm(list = ls())

#install.packages("ISLR")
#install.packages("caTools")
#install.packages("glmnet")
library(ISLR)
library(caTools)
library(glmnet)
library(pls)

setwd("C:\Users\paul_\Documents\Course Material\EAS506 - Stat Data Mining\Homework\Homework 2\Q1")

college <- ISLR::College
rownames(college ) = college [,1]
fix(college)

# There is now a row names column with the university recorded
# R will not perform any calculations on these values

college =college [,-1] # Remove row names column as it is not required
fix(college)
summary(college) # provides a summary of the variables


############################################################################################
# Split into training and test
#
############################################################################################

set.seed(123)   #  set seed to ensure you always have same random numbers generated

sample = sample.split(college,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 = subset(college,sample == TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1  = subset(college, sample == FALSE)

# divide this into x (predictors) and y (response)
# need x as a matrix

X_train <- as.matrix(train1[,2:17])
Y_train <- train1[,1]
X_test <- as.matrix(test1[,2:17])
Y_test <- test1[,1]

# X predictors and Y response
# The function will choose lambda
# This function needs X as a matrix NOT a data frame

############################################################################################
# Fit a linear model using least
# squares on the training set, and report the test error obtained.
#
############################################################################################

lm.mod <- lm(Apps~., data = train1)
summary(lm.mod)

# Residual standard error: 998.9 on 531 degrees of freedom
# Multiple R-squared:  0.924,	Adjusted R-squared:  0.9217 
# F-statistic: 403.7 on 16 and 531 DF,  p-value: < 2.2e-16

lm.pred1 <- predict(lm.mod, train1)

Y_hat <- lm.pred1 # predicted response
RSS_train_0 <- sum((Y_hat - Y_train)^2)
RSS_train_0 # 529849177

lm.pred2 <- predict(lm.mod, test1)

Y_hat <- lm.pred2 # predicted response
RSS_test_0 <- sum((Y_hat - Y_test)^2)  # 400655474
RSS_test_0 #
MSE_test_0 <- RSS_test_0/229 # 1749587
MSE_test_0

############################################################################################
# Fit a ridge regression model on the training set, with lambda chosen by cross validation.
# Report the test error obtained.
#
############################################################################################

ridge.mod = glmnet(X_train, Y_train, alpha=0) # default number of folds = 10

cv.out <- cv.glmnet(X_train, Y_train, alpha = 0) # output of cross validation
x11()
plot(cv.out) #plot the results of cross validation
title("College Applications: CV Results (Ridge)", line=2.5)

bestlam_1 <- cv.out$lambda.min # BEST lambda from cross validation

bestlam_1 # 365.9084

bestlam1_index <- which(ridge.mod$lambda == bestlam_1)
coef(ridge.mod)[,bestlam1_index]

# fit a new optimal model

ridge.pred <- predict(ridge.mod, s= bestlam_1, type = "coefficients")
ridge.pred1 <- predict(ridge.mod, s= bestlam_1, newx = X_train, type = "response")

Y_hat <- ridge.pred1 # predicted response
RSS_train_1 <- sum((Y_hat - Y_train)^2)  # RSS test_error
RSS_train_1 # 620147652

# predict a response with our model X_test, Y_test
# need to use a new x (take everything NOT in our training set) i.e. test data

ridge.pred2 <- predict(ridge.mod, s = bestlam_1, newx = X_test, type = "response")

Y_hat <- ridge.pred2 # predicted response
RSS_test_1 <- sum((Y_hat - Y_test)^2)  # RSS test_error
RSS_test_1 # 710408276
MSE_test_1 <- RSS_test_1/229
MSE_test_1

plot(Y_hat, Y_test, main="College Applications: Actual vs Predicted (Ridge)", 
     xlab="Y Predicted", ylab="Y Actual", pch=19)
abline(lm(Y_test~Y_hat), col="red") # regression line (y~x)

# There appears to be an outlier: Rutgers at New Brunswick

############################################################################################
# Fit a lasso model on the training set, with ?? chosen by crossvalidation.
# Report the test error obtained, along with the number of non-zero coefficient
# estimates.
#
############################################################################################

lasso.mod = glmnet(X_train, Y_train, alpha = 1) # default number of folds = 10

cv.out <- cv.glmnet(X_train, Y_train, alpha = 1) # output of cross validation
x11()
plot(cv.out) #plot the results of cross validation
title("College Applications: CV Results (Lasso)", line=2.5)

bestlam_2 <- cv.out$lambda.min # BEST lambda from cross validation

bestlam_2 # 5.963396

bestlam2_index <- which(lasso.mod$lambda == bestlam_2)
lasso_df <- lasso.mod$df[bestlam2_index] # The number of nonzero coefficients for each value of lambda. 
lasso_df 
coef(lasso.mod)[,bestlam2_index]
# 3 factors dropped
# Enroll, Personal, Terminal

# fit a new optimal model

lasso.pred <- predict(lasso.mod, s= bestlam_2, type = "coefficients")
summary(lasso.pred)

# predict a response with our model X_test, Y_test
# need to use a new x (take everything NOT in our training set) i.e. test data

lasso.pred1 <- predict(lasso.mod, s= bestlam_2, newx = X_train, type = "response")

Y_hat <- lasso.pred1 # predicted response
RSS_train_1 <- sum((Y_hat - Y_train)^2)  # RSS test_error
RSS_train_1 # 531164806

lasso.pred2 <- predict(lasso.mod, s = bestlam_2, newx = X_test, type = "response")

Y_hat <- lasso.pred2 # predicted response
RSS_test_2 <- sum((Y_hat - Y_test)^2)  # RSS test_error
RSS_test_2 # 411049325
MSE_test_2 <- RSS_test_2/229
MSE_test_2

plot(Y_hat, Y_test, main="College Applications: Actual vs Predicted (Lasso)",
     xlab="Y Predicted", ylab="Y Actual", pch=19)
abline(lm(Y_test~Y_hat), col="red") # regression line (y~x)

############################################################################################
# Fit a PCR model on the training set, with k chosen by cross-validation. Report the
# test error obtained, along with the value of k selected by cross-validation.
#
############################################################################################

pcr.fit = pcr(Apps ~. , data = train1, scale = TRUE, validation = "CV")

summary(pcr.fit)

x11()
validationplot(pcr.fit, val.type = "MSEP", main="College Applications: CV Results (PCR)") # Plot to visualize the optimum number of components

# k = 6 or 7 appears to be a good compromise between MSEP and model complexity
# NOTE: MSEP = mean squared error of prediction

# empty array to store error data
training_error_store <- c()
test_error_store <- c()

# for loop to fit the model, calculate and store error for training and test
for (i in 1:16){
  pcr.pred.train = predict(pcr.fit, X_train, ncomp = i)
  pcr.pred.test = predict(pcr.fit, X_test, ncomp = i)
  train.error <- sum((pcr.pred.train-Y_train)^2) # RSS
  test.error <- sum((pcr.pred.test-Y_test)^2) # RSS
  training_error_store <- c(training_error_store, train.error)
  test_error_store <- c(test_error_store, test.error)
}

x11()
plot(training_error_store, main="College Applications (PCR)", log = "y",
     xlim=c(1, 16),
     xlab="Number of Components",
     ylab="RSS")

# K number of components selected = 6
RSS_train_3 <- training_error_store[6]
RSS_train_3 # 851608699
RSS_test_3 <- test_error_store[6]
RSS_test_3 # 958264707

############################################################################################
# Fit a PLS model on the training set, with k chosen by cross-validation. Report the
# test error obtained, along with the value of k selected by cross-validation.
#
############################################################################################

pls.fit = plsr(Apps ~. , data = train1, scale = TRUE, validation = "CV")

summary(pls.fit)

x11()
validationplot(pls.fit, val.type = "MSEP", main="College Applications: CV Results (PLS)") # Plot to visualize the optimum number of components

# k = 6 appears to be a good compromise between MSEP and model complexity
# NOTE: MSEP = mean squared error of prediction

# empty array to store error data
training_error_store <- c()
test_error_store <- c()

# for loop to fit the model, calculate and store error for training and test
for (i in 1:16){
  pls.pred.train = predict(pls.fit, X_train, ncomp = i)
  pls.pred.test = predict(pls.fit, X_test, ncomp = i)
  train.error <- sum((pls.pred.train-Y_train)^2) # RSS
  test.error <- sum((pls.pred.test-Y_test)^2) # RSS
  training_error_store <- c(training_error_store, train.error)
  test_error_store <- c(test_error_store, test.error)
}

x11()
plot(training_error_store, main="College Applications (PLS)", log = "y",
     xlim=c(1, 16),
     xlab="Number of Components",
     ylab="RSS")

RSS_train_4<- training_error_store[6]
RSS_train_4 # 544240736
RSS_test_4 <- test_error_store[6]
RSS_test_4 # 460767507

############################################################################################
# Calculate R^2 for all models
#
############################################################################################

train.avg <- mean(Y_train)
lm.r1 <- 1 - mean((lm.pred1 - Y_train)^2) / mean((train.avg - Y_train)^2)       #0.92
ridge.r1 <- 1 - mean((ridge.pred1 - Y_train)^2) / mean((train.avg - Y_train)^2) #0.91
lasso.r1 <- 1 - mean((lasso.pred1 - Y_train)^2) / mean((train.avg - Y_train)^2) #0.92
pcr.r1 <- 1 - mean((pcr.pred.train - Y_train)^2) / mean((train.avg - Y_train)^2) #0.92
pls.r1 <- 1 - mean((pls.pred.train - Y_train)^2) / mean((train.avg - Y_train)^2) #0.92


test.avg <- mean(Y_test)
lm.r2 <- 1 - mean((lm.pred2 - Y_test)^2) / mean((test.avg - Y_test)^2)       #0.91
ridge.r2 <- 1 - mean((ridge.pred2 - Y_test)^2) / mean((test.avg - Y_test)^2) #0.85
lasso.r2 <- 1 - mean((lasso.pred2 - Y_test)^2) / mean((test.avg - Y_test)^2) #0.91
pcr.r2 <- 1 - mean((pcr.pred.test - Y_test)^2) / mean((test.avg - Y_test)^2) #0.91
pls.r2 <- 1 - mean((pls.pred.test - Y_test)^2) / mean((test.avg - Y_test)^2) #0.91
