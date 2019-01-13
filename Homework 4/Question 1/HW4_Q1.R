############################################################################################
##
## HW4 Question 1
##
## Author: Paul M Girdler
## Created: November 25, 2018
##
############################################################################################

# Clear the memory
rm(list = ls())

# Set working directory
setwd("~/Course Material/EAS506 - Stat Data Mining/Homework/Homework 4/Question 1")

#install.packages("ElemStatLearn")
library("ElemStatLearn")
library("leaps")
library("DAAG")
library("bootstrap")

data(prostate)

prostate <- prostate

#######################################
#Best Subset Selection
#######################################

regfit.full <- regsubsets(lpsa~., data = prostate, nbest = 1, nvmax = 8, method = "exhaustive")
my_sum <- summary(regfit.full)

x11()
par(mfrow = c(2,2))
plot(my_sum$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
plot(my_sum$bic, xlab = "Number of Variables", ylab = "BIC", type = "l")
plot(my_sum$rss, xlab = "Number of Variables", ylab = "RSS", type = "l")
plot(my_sum$adjr2, xlab = "Number of Variables", ylab = "Adjusted R^2", type = "l")
mtext("Best Subset Selection (Exhaustive)", side = 3, line = -3, outer = TRUE)

# 3 appears to be the most parimonious
# However, 8 variable model had the lowest RSS

my_sum$rss[which.min(my_sum$rss)] # 8 variable: 43
my_sum$rss[3]                     # 3 variable: 46.6

################################################
#linear regression using K-fold cross validation
################################################

kfold_error_10 = c()
kfold_error_5=c()
for (i in 1:8){
  predictors = which(my_sum$which[i,2:length(prostate)])
  pred <- subset(prostate , select = predictors)
  temp <- data.frame(prostate$lpsa, pred)
  par(mfrow=c(1,2))
  fit <- lm(prostate.lpsa~.,data=temp)
  lm.fit10<- cv.lm(data=temp,fit, m = 10, dots =FALSE, seed = 123123, plotit=FALSE, printit=TRUE)
  lm.fit5 <- cv.lm(data=temp,fit, m = 5,  dots =FALSE, seed = 123123, plotit=FALSE, printit=TRUE)
  kfold_error_10 = c(kfold_error_10, attr(lm.fit5,"ms"))
  kfold_error_5 = c(kfold_error_5, attr(lm.fit10,"ms"))
}

x11()
par(mfrow = c(1,2))
plot(kfold_error_5, xlab = "Number of Variabes", ylab = "Error", main = "Best Subset Selection (5-Fold CV)",type = "l")
plot(kfold_error_10, xlab = "Number of Variables", ylab = "Error", main = "Best Subset Selection (10-Fold CV)", type = "l")

# If we vary the seed, 3 appears to have consistently good error while also being parsimonious

kfold_error_5[3]  # 0.53
kfold_error_10[3] # 0.544

################################################
#Boosting 
################################################

beta.fit <- function(X,Y){
  lsfit(X,Y)
}
beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}
sq.error <- function(Y, Y_hat){
  (Y - Y_hat)^2
}

X = prostate[,]
Y = prostate[, 9]

bootstrap_error = c()
for (i in 1:8){
  predictors=which(my_sum$which[i,2:length(prostate)])
  res = bootpred(X[,predictors], Y, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  bootstrap_error = c(bootstrap_error, res[[3]])
}

plot(bootstrap_error, type = "o", lty = 2, col = "blue" , xlab = "Number of Variables", ylab = "Error (Bootstrap 0.632)", main = "Best Subset Selection (Boosting)")

bootstrap_error[4] #0.517