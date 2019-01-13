############################################################################################
# Paul M Girdler 28/09/18
# Homework 2
# Question 2
#
############################################################################################

# 2) (10 points) The insurance company benchmark data set gives information on
# customers. Specifically, it contains 86 variables on product-usage data and sociodemographic
# data derived from zip area codes. There are 5,822 customers in the
# training set and another 4,000 in the test set. The data were collected to answer
# the following questions: Can you predict who will be interested in buying a
# caravan insurance policy and give an explanation why? Compute the OLS
# estimates and compare them with those obtained from the following variable selection
# algorithms: Forwards Selection, Backwards Selection, Lasso regression,
# and Ridge regression. Support your answer.
# (The data can be downloaded from https://kdd.ics.uci.edu/databases/tic/tic.html. )

rm(list = ls())

#install.packages("leaps")
#install.packages("glmnet")
#install.packages("mclust")
#install.packages("caret")
#install.packages('e1071', dependencies=TRUE)
library(leaps)
library(glmnet)
library(mclust)
library(caret)

setwd("C:/Users/paul_/Documents/Course Material/EAS506 - Stat Data Mining/Homework/Homework 2/Q2")

train <- read.table("ticdata2000.txt")
G_train <- train[86]
X_train <- train[-86]

# For reading "tab-separated value" files (".txt"). 
# By default, point (".") is used as decimal points.

X_test <- read.table("ticeval2000.txt")
G_test <- read.table("tictgts2000.txt")


# Each record consists of 86 attributes, containing6 sociodemographic data (attribute 1-43) and product 
# ownership (attributes 44-86).The sociodemographic data is derived from zip codes. All customers living 
# in areas with the same zip code have the same sociodemographic attributes. Attribute 86, 
# "CARAVAN:Number of mobile home policies", is the target variable.

############################################################################################
# Roundt Function
#
############################################################################################

threshold = 0.5 # BY LOWERING THIS WE CAN INCREASE THE NUMBER OF CLASS CUSTOMER (G = 1) PREDICTED
roundt <- function(x) { 
  
  if (x < threshold) 0 else 1
}

############################################################################################
# Forwards Selection
#
############################################################################################

n_fwd <- 85
regfit.fwd <- regsubsets(as.matrix(X_train), as.matrix(G_train), nvmax = n_fwd, method = "forward")
fwd_summary <- summary(regfit.fwd)
xrange <- c(0 , n_fwd)

x11()
par(mfrow = c(2,2))
plot(fwd_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", xlim=xrange)
plot(fwd_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", xlim=xrange)
plot(fwd_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", xlim=xrange)
plot(fwd_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l", xlim=xrange)
mtext("Forward SS: Customer Prediction Model", side = 3, line = -2, outer = TRUE)

# Ideal Number of variables:
# Cp      = 25
# BIC     = 10
# Adj R^2 = 30

which(fwd_summary$cp == min(fwd_summary$cp))       #23
which(fwd_summary$bic == min(fwd_summary$bic))     #8
which(fwd_summary$rss == min(fwd_summary$rss))     #85
which(fwd_summary$adjr2 == max(fwd_summary$adjr2)) #47

summ.fwd_23 <- summary(regfit.fwd$outmat[23,])
coef_fwd_23 <- coef(regfit.fwd, 23)
coef_fwd_23 # V4,V7,V10,V16,V18,V21,V35,V36,V41,V42,V43,V44,V46,V47,V57,V58,V59,V78,V79,V80,V82,V83,V85,V86
train_fwd23 <- subset(train, select = c(V4,V7,V10,V16,V18,V21,V35,V36,V41,V42,V43,V44,V46,V47,V57,V58,V59,V78,V79,V80,V82,V83,V85,V86))

lm_fwd23 <- lm(V86~., data = train_fwd23)
summary(lm_fwd23)

pred_train_fwd23 <- sapply(predict(lm_fwd23, X_train), roundt)

error_fwd23_train <- classError(pred_train_fwd23, as.matrix(G_train))$errorRate
error_fwd23_train #0.05977327

pred_test_fwd23 <- sapply(predict(lm_fwd23, X_test), roundt)

error_fwd23_test <- classError(pred_test_fwd23, as.matrix(G_test))$errorRate
error_fwd23_test #0.0595


MSE_fwd23_test <- mean((pred_test_fwd23 - as.matrix(G_test))^2)
MSE_fwd23_test #0.05975

############################################################################################
# Backwards Selection
#
############################################################################################

n_bwd <- 85
regfit.bwd <- regsubsets(as.matrix(X_train), as.matrix(G_train), nvmax = n_bwd, method = "backward")
bwd_summary <- summary(regfit.bwd)
xrange <- c(0 , n_bwd)

x11()
par(mfrow = c(2,2))
plot(bwd_summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", xlim=xrange)
plot(bwd_summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l", xlim=xrange)
plot(bwd_summary$rss, xlab = "Number of Variables", ylab = "RSS", type = "l", xlim=xrange)
plot(bwd_summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l", xlim=xrange)
mtext("Backward SS: Customer Prediction Model", side = 3, line = -2, outer = TRUE)
dev.off

# Ideal Number of variables:
# Cp      = 30
# BIC     = 10
# Adj R^2 = 30

which(bwd_summary$cp == min(bwd_summary$cp))       #29
which(bwd_summary$bic == min(bwd_summary$bic))     #8
which(bwd_summary$rss == min(bwd_summary$rss))     #85
which(bwd_summary$adjr2 == max(bwd_summary$adjr2)) #39

summ.bwd_23 <- summary(regfit.bwd$outmat[29,])
coef_bwd_23 <- coef(regfit.bwd, 29)
coef_bwd_23 # V4,V6,V10,V17,V18,V21,V22,V28,V30,V35,V36,V41,V42,V44,V46,V47,V55,V57,V58,V59,V63,V76,V78,V79,V80,V82,V83,V84,V85
train_bwd23 <- subset(train, select = c(V4,V6,V10,V17,V18,V21,V22,V28,V30,V35,V36,V41,V42,V44,V46,V47,V55,V57,V58,V59,V63,V76,V78,V79,V80,V82,V83,V84,V85,V86))

lm_bwd23 <- lm(V86~., data = train_bwd23)
summary(lm_bwd23)

pred_train_bwd23 <- sapply(predict(lm_bwd23, X_train), roundt)

error_bwd23_train <- classError(pred_train_bwd23, as.matrix(G_train))$errorRate
error_bwd23_train #0.05977327

pred_test_bwd23 <- sapply(predict(lm_bwd23, X_test), roundt)

error_bwd23_test <- classError(pred_test_bwd23, as.matrix(G_test))$errorRate
error_bwd23_test #0.0595


MSE_bwd23_test <- mean((pred_test_bwd23 - as.matrix(G_test))^2)
MSE_bwd23_test #0.05975

############################################################################################
# Lasso Regression
#
############################################################################################

lasso.mod = glmnet(as.matrix(X_train), as.matrix(G_train), alpha = 1) # default number of folds = 10

cv.out.lasso <- cv.glmnet(as.matrix(X_train), as.matrix(G_train), alpha = 1) # output of cross validation
x11()
plot(cv.out.lasso) #plot the results of cross validation
title("Customer Prediction Model: CV Results (Lasso)", line=2.5)
dev.off

bestlam_lasso <- cv.out.lasso$lambda.min # BEST lambda from cross validation

bestlam_lasso # 0.003495312

bestlam_lasso_index <- which(lasso.mod$lambda == bestlam_lasso)
coef(lasso.mod)[,bestlam_lasso_index]

# fit a new optimal model

lasso.pred <- predict(lasso.mod, s= bestlam_lasso, type = "coefficients")

# predict a response with our model for training and test

lasso.pred1 <- sapply(predict(lasso.mod, s = bestlam_lasso, newx = as.matrix(X_train), type = "response"), roundt)

# optimise threshold 0 and 1

lasso.pred2 <- sapply(predict(lasso.mod, s = bestlam_lasso, newx = as.matrix(X_test), type = "response"), roundt)
error_l_train <- classError(lasso.pred1, as.matrix(G_train))$errorRate

# predict the misclassification rate with our model for training and test
error_l_test <- classError(lasso.pred2, as.matrix(G_test))$errorRate


############################################################################################
# Ridge Regression
#
############################################################################################

ridge.mod = glmnet(as.matrix(X_train), as.matrix(G_train), alpha=0) # default number of folds = 10

x11()
cv.out.ridge <- cv.glmnet(as.matrix(X_train), as.matrix(G_train), alpha = 0) # output of cross validation
plot(cv.out.ridge) #plot the results of cross validation
title("Customer Prediction Model: CV Results (Ridge)", line=2.5)
dev.off

bestlam_ridge <- cv.out.ridge$lambda.min # BEST lambda from cross validation

bestlam_ridge # 0.1227271

bestlam_ridge_index <- which(ridge.mod$lambda == bestlam_ridge)
coef(ridge.mod)[,bestlam_ridge_index]

# fit a new optimal model

ridge.pred <- sapply(predict(ridge.mod, s= bestlam_ridge, type = "coefficients"), roundt)

# predict a response with our model for training and test

ridge.pred1 <- sapply(predict(ridge.mod, s = bestlam_ridge, newx = as.matrix(X_train), type = "response"), roundt)
ridge.pred2 <- sapply(predict(ridge.mod, s = bestlam_ridge, newx = as.matrix(X_test), type = "response"), roundt)

# predict the misclassification rate with our model for training and test

error_r_train <- classError(ridge.pred1, as.matrix(G_train))$errorRate
error_r_test <- classError(ridge.pred2, as.matrix(G_test))$errorRate

############################################################################################
# Can you predict who will be interested in buying a
# caravan insurance policy and give an explanation why?
#
############################################################################################

# Confusion Table
confusion.table <- table(as.matrix(ridge.pred1), as.matrix(G_train))
confusion.matrix.ridge.pred1 <- confusionMatrix(confusion.table)
confusion.matrix.ridge.pred1$table

confusion.table <- table(as.matrix(ridge.pred2), as.matrix(G_test))
confusion.matrix.ridge.pred2 <- confusionMatrix(confusion.table)
confusion.matrix.ridge.pred2$table

regfit.fwd.20 <- regsubsets(as.matrix(X_train), as.matrix(G_train), nvmax = 20, method = "forward")
fwd_summary_20 <- summary(regfit.fwd.20)

x11()
plot(regfit.fwd.20,scale="bic")
title("Customer Prediction Model: BIC (Forward)", line=2.5)

# V10, V18, V43, V44, V47, V59, V82, V85
# Married, Lower level education, Purchasing power class, 
# Contribution private third party insurance, Contribution car policies, 
# Contribution fire policies, Number of boat policies, 
# Number of social security insurance policies

