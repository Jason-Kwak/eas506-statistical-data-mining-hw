############################################################################################
##
## HW5 Question 3
##
## Author: Paul M Girdler
## Created: December 16, 2018
##
############################################################################################

# Clear the memory
rm(list=ls())

# Set working directory
setwd("~/Course Material/EAS506 - Stat Data Mining/Homework/Homework 5/Question 3")

#install.packages("neuralnet")
#install.packages("plyr")
library(neuralnet)
library(caret)
library(plyr) 
library(ggplot2)

#load and process data
load("pima.RData")
data <- pima[,-9]

# Convert class digit to binary integer
# numeric is required for nn
# integer will process faster
var1.matrix <- model.matrix(~ classdigit - 1, data=data)
var1.matrix <- as.integer(var1.matrix[,-1])
data <- data.frame(data, var1.matrix)
data <- data[,-c(8)]
names(data)[8]<-"classdigit"
data[, -c(8)] <- scale(data[, -c(8)])

#hist(data[,8],
#     main="Histogram for Class Labels", 
#     xlab="Class Digit",
#     breaks=2)

# Class 1: Diabetic
# Class 0: Normal

n <- names(data)
f <- as.formula(paste("classdigit ~", paste(n[!n %in% "classdigit"], collapse = " + ")))

set.seed(123123)

p <- 7

cv.summary <- data.frame(index=integer(),cv_error=double(),cv_min=double(),cv_max=double()) 
cv.error  <- matrix(0, p, 10)

#Rule of thumb: The input layer size and the output layer size, usually 2/3 of the input size.

pbar <- create_progress_bar('text')

pbar$init(p)

for(i in 1:p){
for(j in 1:10){
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- data[index,]
  test.cv <- data[-index,]
  
  nn <- neuralnet(f,data=train.cv,hidden=i, threshold =0.20,stepmax = 1e+06,err.fct='ce',act.fct='logistic',linear.output=FALSE)
  
  # test error
  pr.nn <- compute(nn,test.cv[,1:7])
  true_class = test.cv$classdigit
  pred_class = round(pr.nn$net.result[,1])
  cv.error[i,j] <- sum(abs(true_class - pred_class))/nrow(test.cv)
  
}
  cv.summary[i,1] <- i
  
  m  = mean(cv.error[i,])*100
  sd = sd(cv.error[i,])*100
  cv.summary[i,2] <- m
  cv.summary[i,3] <- m - sd
  cv.summary[i,4] <- m + sd
  
  pbar$step()
}



ggplot(cv.summary, aes(x = index, y = cv_error)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = cv_min, ymax = cv_max)) +
  ggtitle("CV Error - (1 Hidden Layer)") +
  xlab("Hidden Nodes n") +
  ylab("CV Error (%)") +
 
p <- 10

cv.summary <- data.frame(index=integer(),cv_error=double(),cv_min=double(),cv_max=double()) 
cv.error  <- matrix(0, p, 5)

#Rule of thumb: The input layer size and the output layer size, usually 2/3 of the input size.

pbar <- create_progress_bar('text')

pbar$init(p)

for(i in 1:p){
  for(j in 1:5){
    index <- sample(1:nrow(data),round(0.8*nrow(data)))
    train.cv <- data[index,]
    test.cv <- data[-index,]
    
    nn <- neuralnet(f,data=train.cv,hidden=3, threshold =0.10*i,stepmax = 1e+06,err.fct='ce',act.fct='logistic',linear.output=FALSE)
    
    # test error
    pr.nn <- compute(nn,test.cv[,1:7])
    true_class = test.cv$classdigit
    pred_class = round(pr.nn$net.result[,1])
    cv.error[i,j] <- sum(abs(true_class - pred_class))/nrow(test.cv)
    
  }
  cv.summary[i,1] <- 0.10*i
  
  m  = mean(cv.error[i,])*100
  sd = sd(cv.error[i,])*100
  cv.summary[i,2] <- m
  cv.summary[i,3] <- m - sd
  cv.summary[i,4] <- m + sd
  
  pbar$step()
}



ggplot(cv.summary, aes(x = index, y = cv_error)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = cv_min, ymax = cv_max)) +
  ggtitle("CV Error (Hidden Nodes = 3)") +
  xlab("Threshold") +
  ylab("CV Error (%)")


#p <- 10

#cv.summary <- data.frame(index=integer(),cv_error=double(),cv_min=double(),cv_max=double()) 
#cv.error  <- matrix(0, p, 5)

#Rule of thumb: The input layer size and the output layer size, usually 2/3 of the input size.

#pbar <- create_progress_bar('text')

#pbar$init(p)

    p <- 20
    
    cv.summary <- data.frame(index=integer(),cv_error=double(),cv_min=double(),cv_max=double()) 
    cv.error  <- matrix(0, p, 10)
    
    #Rule of thumb: The input layer size and the output layer size, usually 2/3 of the input size.
    
    pbar <- create_progress_bar('text')
    
    pbar$init(p)
    
    for(i in 1:p){
        pf = (1+i*1)
        
      for(j in 1:10){
        index <- sample(1:nrow(data),round(0.9*nrow(data)))
        train.cv <- data[index,]
        test.cv <- data[-index,]
        train.cv[1, 2] <- train.cv[1, 2]*pf
        
        nn <- neuralnet(f,data=train.cv,hidden=3, threshold =0.40,stepmax = 1e+06,err.fct='ce',act.fct='logistic',linear.output=FALSE)
        
        # test error
        pr.nn <- compute(nn,test.cv[,1:7])
        true_class = test.cv$classdigit
        pred_class = round(pr.nn$net.result[,1])
        cv.error[i,j] <- sum(abs(true_class - pred_class))/nrow(test.cv)
        
      }
      cv.summary[i,1] <- (1+i*1)
      
      m  = mean(cv.error[i,])*100
      sd = sd(cv.error[i,])*100
      cv.summary[i,2] <- m
      cv.summary[i,3] <- m - sd
      cv.summary[i,4] <- m + sd
      
      pbar$step()
    }
    
    
    
    ggplot(cv.summary, aes(x = index, y = cv_error)) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = cv_min, ymax = cv_max)) +
      ggtitle("CV Error - Comparison of Data Perturbation") +
      xlab("Perturbation Factor") +
      ylab("CV Error (%)")
    
    p <- 10
    cv.summary <- data.frame(index=integer(),error=double()) 

    pbar <- create_progress_bar('text')
    
    pbar$init(p)
    
    pf = 1
    
    index <- sample(1:nrow(data),round(0.9*nrow(data)))
    train.cv <- data[index,]
    test.cv <- data[-index,]
    
    for(i in 1:p){
      pf = (1+i*0.05)*pf
      
        train.cv[1, 3] <- train.cv[1, 3]*pf
        
        nn <- neuralnet(f,data=train.cv,hidden=3, threshold =0.40,stepmax = 1e+06,err.fct='ce',act.fct='logistic',linear.output=FALSE)
        
        # test error
        pr.nn <- compute(nn,test.cv[,1:7])
        true_class = test.cv$classdigit
        pred_class = round(pr.nn$net.result[,1])
        
        cv.summary[i,1] <- pf
        cv.summary[i,2] <- sum(abs(true_class - pred_class))/nrow(test.cv)
        
        pbar$step()
      }
      
      

    
    
    
    ggplot(cv.summary, aes(x = index, y = error*100)) +
      geom_line() +
      ggtitle("Error - Comparison of Data Perturbation") +
      xlab("Perturbation Factor") +
      ylab("Error (%)") 
    