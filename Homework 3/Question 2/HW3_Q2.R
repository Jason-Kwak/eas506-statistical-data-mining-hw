############################################################################################
##
## HW3 Question 2
##
## Author: Paul M Girdler
## Created: October 28, 2018
##
############################################################################################

# Clear the memory
rm(list = ls())

# Set working directory
setwd("~/Course Material/EAS506 - Stat Data Mining/Homework/Homework 3/Question 2")

#install.packages("corrplot")
#install.packages("car")
#install.packages("MVN")
#install.packages("MASS")

library("corrplot")
library("car")
library("MVN")
library("MASS")

full_data <- read.table("DiabetesAndrews36_1.txt")

# Disregard the first three columns. 

data <- full_data[,-1:-4]

# The fourth column is the observation number, and the next five columns are the variables 
# (glucose.area, insulin.area, SSPG, relative.weight, and fasting.plasma.glucose). 
# The final column is the class number. 

names(data) <- c("glucarea", "insarea", "SSPG", "relw", "fpgluc", "class")

# Suppose an individual has (glucose area = 0.98, insulin area =122, SSPG =544. 
# Relative weight = 186, fasting plasma glucose = 184)

# Add individual to dataset
data <- rbind(data, c(0.98, 122, 544, 186, 184, 2.297))

# Scale data together
data <- as.data.frame(cbind(scale(data[,-6]), class=data[,6]))

# Individual we must predict moved to separate dataset
ind_data <- data[145,]

# Remaining data
data <- data[1:144,]

# Check the class balance

#histogram(data$class, breaks=seq(0,4, by=1), 
#          main="Class Distribution", 
#          xlab="Class Number", xaxt = "n")
#axis(1, at=seq(0,3, by=1))

# Class 3 is the most dominant class

###########################################################################################
#
# Produce pairwise scatterplots for all five variables, with different symbols or
# colors representing the three different classes. Do you see any evidence that
# the classes may have difference covariance matrices? That they may not be
# multivariate normal? 
#
###########################################################################################

x11()
pairs(data[0:5], col = data$class, oma=c(3,3,4,15))
par(xpd = TRUE)
legend("bottomright", fill = unique(data$class), legend = c(unique(data$class)))

# MVN - Multivariate Normatality Test

MVN <- mvn(data, subset = NULL, mvnTest = "mardia")
MVN$multivariateNormality # NOT MULTIVARIATE NORMAL

###########################################################################################
#
# Apply linear discriminant analysis (LDA) and quadratic discriminant analysis
# (QDA). How does the performance of QDA compare to that of LDA in this
# case?
#
###########################################################################################

#####################################
#
# Linear Discriminant Analysis (LDA)
#
#####################################

lda.fit <- lda(class~., data = data, CV = TRUE)
lda_tab <- table(data$class, lda.fit$class)
lda_tab
lda_error <- sum(lda_tab)-sum(lda_tab[1,1], lda_tab[2,2], lda_tab[3,3])
lda_error_rate <- lda_error/sum(lda_tab)
lda_error_rate # 0.1111111

#####################################
#
# Linear Discriminant Analysis (QDA)
#
#####################################

qda.fit <- qda(class~., data = data, CV = TRUE)
qda_tab <- table(data$class, qda.fit$class)
qda_tab
qda_error <- sum(qda_tab)-sum(qda_tab[1,1], qda_tab[2,2], qda_tab[3,3])
qda_error_rate <- qda_error/sum(qda_tab)
qda_error_rate # 0.09722222

###########################################################################################
#
# Suppose an individual has (glucose area = 0.98, insulin area =122, SSPG =544. 
# Relative weight = 186, fasting plasma glucose = 184). 
# To which class does LDA assign this individual? To which class does QDA?
#
###########################################################################################

lda.fit <- lda(class~., data = data, CV = FALSE)
predict(lda.fit, ind_data)$class # Class 1

qda.fit <- qda(class~., data = data, CV = FALSE)
predict(qda.fit, ind_data)$class # Class 1
