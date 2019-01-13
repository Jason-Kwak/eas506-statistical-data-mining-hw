############################################################################################
##
## HW5 Question 5
##
## Author: Paul M Girdler
## Created: December 16, 2018
##
############################################################################################

#rm(list=ls())

setwd("C:/Users/paul_/Documents/Course Material/EAS506 - Stat Data Mining/Homework/Homework 5/Question 5")

#install.packages("uskewFactors")
#install.packages("ggfortify")
library(ggfortify)
library(uskewFactors)
library(mclust)

data <- SwissBankNotes

data$Y = as.numeric(data$Y)

autoplot(prcomp(data))
autoplot(prcomp(data[1:100,]))
autoplot(prcomp(data[101:200,]))

pairs(data)

pcaFull = prcomp(data)

pcaFull = princomp(data[,-7])
loadings(pcaFull)
summary(pcaFull)

pcaOrginal = princomp(data[1:100,-7])
loadings(pcaOrginal)
summary(pcaOrginal) 


pcaDup = princomp(data[101:200,-7])
loadings(pcaDup)
summary(pcaDup) 
