############################################################################################
# Paul M Girdler 06/10/18
# Homework 2
# Question 3
#
############################################################################################

# 3) (10 points) (Exercise 9 modified, ISL) We have seen that as the number of
# features used in a model increases, the training error will necessarily decrease, but
# the test error may not. We will now explore this in a simulated data set.
# Generate a data set with p = 20 features, n = 1, 000 observations, and an
# associated quantitative response vector generated according to the model
# Y = Xß + e where ß has some elements that are exactly equal to zero. Split your data set into
# a training set containing 100 observations and a test set containing 900
# observations.
# Perform best subset selection on the training set, and plot the training set MSE
# associated with the best model of each size. Plot the test set MSE associated with
# the best model of each size.
# For which model size does the test set MSE take on its minimum value?
#  Comment on your results. How does the model at which the test set MSE is
# minimized compare to the true model used to generate the data? Comment on the
# coefficient values.

rm(list = ls())

#install.packages("leaps")
library(leaps)

setwd("C:/Users/paul_/Documents/Course Material/EAS506 - Stat Data Mining/Homework/Homework 2/Q2")

############################################################################################
# Generate a data set with p = 20 features, n = 1, 000 observations, and an
# associated quantitative response vector generated according to the model
# Y = Xß + e where ß has some elements that are exactly equal to zero.
############################################################################################

set.seed(123)
X <- matrix(rnorm(1000 * 20), 1000, 20)
b <- rexp(20, rate = 10) 
b[3] <- 0
b[4] <- 0
b[9] <- 0
b[10] <- 0
b[19] <- 0
eps <- rnorm(1000)
Y <- X %*% b + eps

############################################################################################
# Split your data set into a training set containing 100 observations and a test set 
# containing 900 observations.
# 
############################################################################################

train <- sample(seq(1000), 100, replace = FALSE)
test <- -train
X_train <- X[train, ]
X_test <- X[test, ]
Y_train <- Y[train]
Y_test <- Y[test]

############################################################################################
# Perform best subset selection on the training set, and plot the training set MSE  
# associated with the best model of each size.
# 
############################################################################################

data.train <- data.frame(Y = Y_train, X = X_train)
regfit.full <- regsubsets(Y ~ ., data = data.train, nvmax = 20)
train.mat <- model.matrix(Y ~ ., data = data.train, nvmax = 20)
val.errors.train <- rep(NA, 20)

for (i in 1:20) {
  coefi <- coef(regfit.full, id = i)
  pred <- train.mat[, names(coefi)] %*% coefi
  val.errors.train[i] <- mean((pred - Y_train)^2) # Train MSE
}

plot(val.errors.train, xlab = "Number of predictors", ylab = "Training MSE", pch = 19, type = "b")

############################################################################################
# Plot the test MSE associated with the best model of each size. 
# 
# 
############################################################################################

data.test <- data.frame(Y = Y_test, X = X_test)
test.mat <- model.matrix(Y ~ ., data = data.test, nvmax = 20)
val.errors.test <- rep(NA, 20)
for (i in 1:20) {
  coefi <- coef(regfit.full, id = i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors.test[i] <- mean((pred - Y_test)^2) # MSE
}
#plot(val.errors.test, xlab = "Number of predictors", ylab = "Test MSE", pch = 19, type = "b")

############################################################################################
# NOTE: Plots combined below 
#
############################################################################################

np <- seq(1, 20, by=1)
x11()
plot(np, val.errors.train, xlab = "Number of predictors", ylab = "MSE", pch = 19, type = "b", col="blue")
title("Training & Test MSE vs Number of Predictors")
par(new = TRUE)
plot(np, val.errors.test, axes = FALSE, pch = 19, type = "b", xlab = "", ylab = "", col="red")

############################################################################################
# For which model size does the test set MSE take on its minimum value?
# Comment on your results. How does the model at which the test set MSE is
# minimized compare to the true model used to generate the data? Comment on the
# coefficient values. 
############################################################################################

which.min(val.errors.test)

b_est_4 <- coef(regfit.full, which.min(val.errors.test))
b_table_4 <- as.data.frame(b_est_4)
b_table_4

b_est_15 <- coef(regfit.full, 15)
b_table_15 <- as.data.frame(b_est_15)

b_table_true_20 <- as.data.frame(b)
b_table_true_20

# (Note: If it takes on its minimum value for a model containing only an intercept or a
#  model containing all of the features, then play around with the way that you are
#  generating the data in until you come up with a scenario in which the test set MSE is
#  minimized for an intermediate model size.)
