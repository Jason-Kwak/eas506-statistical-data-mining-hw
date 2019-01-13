############################################################################################
##
## HW5 Question 2
##
## Author: Paul M Girdler
## Created: December 16, 2018
##
############################################################################################

rm(list=ls())

library(ElemStatLearn)
library(neuralnet)
library(plyr)
library(ggplot2)

data(spam)

spam_train = sample(1:nrow(spam), nrow(spam)*0.80)
spam_test = -spam_train
spam_train_data = spam[spam_train, ]
spam_test_data = spam[spam_test, ]

n <- names(spam_train_data)

f <- as.formula(paste("spam ~", paste(n[!n %in% "spam"], collapse = " + ")))

set.seed(1234)

p <- 6

nn.error.store <- data.frame(index=integer(),trainerror=double(),testerror=double()) 

pbar <- create_progress_bar('text')

pbar$init(p)

for(i in 1:p){
  
  nn <- neuralnet(f,data=data.matrix(spam_train_data),hidden=i,threshold = 0.2)
  
  nn_pred1 = round(compute(nn,spam_train_data[,1:57])$net.result[,1])
  nn_pred2 = round(compute(nn,spam_test_data[,1:57])$net.result[,1])
  
  spam_train_data1      <- spam_train_data
  spam_train_data1$spam <- as.character(spam_train_data1$spam)
  spam_train_data1$spam[spam_train_data1$spam=="email"] <- 1
  spam_train_data1$spam[spam_train_data1$spam=="spam"]  <- 2
  
  spam_test_data1      <- spam_test_data
  spam_test_data1$spam <- as.character(spam_test_data1$spam)
  spam_test_data1$spam[spam_test_data1$spam=="email"] <- 1
  spam_test_data1$spam[spam_test_data1$spam=="spam"]  <- 2
  
  nn.error.store[i,1] <- i
  
  nn.error.store[i,2]  <- mean(spam_train_data1$spam != nn_pred1)
  nn.error.store[i,3] <- mean(spam_test_data1$spam != nn_pred2)
  
  pbar$step()
}

plot(nn.error.store, xlab="Neurons", ylab="Error", type = 'l',main="Test Error")

ggplot(data=nn.error.store, aes(x = index, y = trainerror*100)) +
  geom_line() +
  labs(title="Comparison: Training Error against No. of Hidden Nodes",x="Hidden Nodes n", y = "Error (%)") +
  xlim(1,6) +
  ylim(0,10)

ggplot(data=nn.error.store, aes(x = index, y = testerror*100)) +
  geom_line() +
  ggtitle("Comparison: Test Error against No. of Hidden Nodes") +
  labs(title="Error - Comparison of Number of Hidden Nodes n",x="Hidden Nodes n", y = "Error (%)") +
  xlim(1,6) +
  ylim(0,10)
