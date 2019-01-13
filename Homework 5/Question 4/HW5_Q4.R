############################################################################################
##
## HW5 Question 4
##
## Author: Paul M Girdler
## Created: December 16, 2018
##
############################################################################################

rm (list=ls())

library(e1071)
library(ISLR)

data(OJ)

set.seed(123123)

C<-sample(1:nrow(OJ),.70*nrow(OJ))
train<-OJ[C,]
test<-OJ[-C,]

# Part a

lintest_error<-c()
lintrain_error<-c()

for(i in c(0.01,0.05,0.1,0.5,1,5,7,10)){
  tune.model.linear<-tune(svm,Purchase~.,data=train,kernel="linear",ranges=list(cost=i))
  y.hat.test.linear<-predict(tune.model.linear$best.model,newdata=test)
  y.ture.test<-test$Purchase
  
  test_error<-length(which(y.hat.test.linear!=y.ture.test))/length(y.ture.test)
  lintest_error<-c(lintest_error,test_error)
  
  y.hat.train.linear<-predict(tune.model.linear$best.model,newdata=train)
  y.true.train<-train$Purchase
  
  train.error<-length(which(y.hat.train.linear!=y.true.train))/length(y.true.train)
  lintrain_error<-c(lintrain_error,train.error)
}

lintest_error  
lintrain_error

cost<-c(0.01,0.05,0.1,0.5,1,5,7,10)

par(mfrow=c(1,2))
plot(cost,lintrain_error,type="b",lty=1,col="red",xlab = "Cost",main=" Linear Training Error",ylab="Training Error")
plot(cost,lintest_error,type="b",lty=2,col = "blue",xlab = "Cost",main="Linear Test Error",ylab="Test Error")


linear.error_table<-cbind(lintest_error,lintrain_error,cost)
linear.error_table



# Part B

radtest.err.1<-c()
radtrain.err.1<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,7,10)){
  tune.model.rad.1<-tune(svm,Purchase~.,data=train,kernel="radial",ranges=list(cost=i))
  
  y.hat.test.rad.1<-predict(tune.model.rad.1$best.model,newdata=test)
  y.ture.test<-test$Purchase
  
  test.error.1<-length(which(y.hat.test.rad.1!=y.ture.test))/length(y.ture.test)
  radtest.err.1<-c(radtest.err.1,test.error.1)
  
  y.hat.train.rad.1<-predict(tune.model.rad.1$best.model,newdata=train)
  y.true.train<-train$Purchase
  
  train.error.1<-length(which(y.hat.train.rad.1!=y.true.train))/length(y.true.train)
  radtrain.err.1<-c(radtrain.err.1,train.error.1)
}

radtest.err.1 
radtrain.err.1 

cost<-c(0.01,0.05,0.1,0.5,1,5,7,10)

plot(cost,radtrain.err.1,type="b",lty=1,col="red",xlab = "cost",main="Radial Kernel - Training Error",ylab="Training Error")
plot(cost,radtest.err.1,type="b",lty=2,col = "blue",xlab = "cost",main="Radial Kernel - Error",ylab="Test Error")

rad.err.1.table<-cbind(radtest.err.1,radtrain.err.1,cost)
rad.err.1.table

polytest.err<-c()
polytrain.err<-c()
for(i in c(0.01,0.05,0.1,0.5,1,5,7,10)){
  tune.model.polynomial<-tune(svm,Purchase~.,data=train,degree=2,kernel="polynomial",ranges=list(cost=i))
  
  y.hat.test.polynomial<-predict(tune.model.polynomial$best.model,newdata=test)
  y.ture.test<-test$Purchase
  
  test.error.2<-length(which(y.hat.test.polynomial!=y.ture.test))/length(y.ture.test)
  polytest.err<-c(polytest.err,test.error.2)
  
  y.hat.train.polynomial<-predict(tune.model.polynomial$best.model,newdata=train)
  y.true.train<-train$Purchase
  
  train.error.2<-length(which(y.hat.train.polynomial!=y.true.train))/length(y.true.train)
  polytrain.err<-c(polytrain.err,train.error.2)
}

polytest.err 
polytrain.err 

cost<-c(0.01,0.05,0.1,0.5,1,5,7,10)

par(mfrow=c(1,2))
plot(cost,polytrain.err,type="b",lty=1,col="red",xlab = "cost",main="Poly Kernel - Training Error",ylab="Training Error")
plot(cost,polytest.err,type="b",lty=2,col = "blue",xlab = "cost",main="Poly Kernel - Test Errorr",ylab="Test Error")

polynomial.err.table<-cbind(polytest.err,polytrain.err,cost)
polynomial.err.table
