###########################################################################
## This code is to explore some data using different visualizations (EDA)
## Paul M Girdler
## Created: Sep 09, 2018
## 
###########################################################################

# NOTES from lecturer
# Rescale variables
# Identify possible outliers try not to remove if required
# Combine variables if required.

# Clear the memory
rm(list = ls())

# Set working directory

setwd("student")

#install packages if required
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("car")
library("ggplot2")
library("lattice")

load("~/student/cleaned_student_data.rdata")

nums <- unlist(lapply(g, is.numeric))
g_numeric <- subset(g[,nums])
corrg <- cor(g_numeric)
#png("Corrplot_Student.png")
M <- cor(corrg)
corrplot(M)
#dev.off

# Top 3 Numeric Pos Corr
# Studytime, Medu

# Top 3 Numeric Neg Corr
# Failures, Dalc, absences


###########################################################################
## "Kitchen Sink" Multi Linear Regression Model
## 
## 
## 
###########################################################################

#Residual standard error: 2.984 on 369 degrees of freedom
#Multiple R-squared:  0.2208,	Adjusted R-squared:  0.1996 
#F-statistic: 10.45 on 10 and 369 DF,  p-value: 1.349e-15
mG1xm1 <- lm(G1.x ~ studytime + Medu + failures.x + Dalc + absences.x + higher + school + paid + sex + address, data = g)
summary(mG1xm1) # summary of model


#Residual standard error: 2.112 on 369 degrees of freedom
#Multiple R-squared:  0.282,	Adjusted R-squared:  0.2626 
#F-statistic:  14.5 on 10 and 369 DF,  p-value: < 2.2e-16
mG1ym1 <- lm(G1.y ~ studytime + Medu + failures.y + Dalc + absences.y + higher + school + paid + sex + address, data = g)
summary(mG1ym1) # summary of model

###########################################################################
## Trimmed Multi Linear Regression Model
## 
## 
## 
###########################################################################

#Residual standard error: 2.978 on 373 degrees of freedom
#Multiple R-squared:  0.215,	Adjusted R-squared:  0.2024 
#F-statistic: 17.03 on 6 and 373 DF,  p-value: < 2.2e-16
tmG1xm1 <- lm(G1.x ~ studytime + Medu + failures.x + sex + higher + Dalc, data = g)
summary(tmG1xm1) # summary of model

#Residual standard error: 2.163 on 373 degrees of freedom
#Multiple R-squared:  0.2383,	Adjusted R-squared:  0.2261 
#F-statistic: 19.45 on 6 and 373 DF,  p-value: < 2.2e-16
tmG1ym1 <- lm(G1.y ~ studytime + Medu + failures.y + sex + higher + Dalc, data = g)
summary(tmG1ym1) # summary of model

###########################################################################
## Multi Linear Regression Model with Interactions
## 
## 
## 
###########################################################################

mG1xm2 <- lm(G1.x ~ studytime*Medu*failures.x*Dalc*absences.x*higher*school*paid*sex*address, data = g)
summary(mG1xm2) 

# Medu:failures.x *
# failures.x:absences.x:addressU *
# Medu:schoolMS:addressU *

mG1ym2 <- lm(G1.y ~ studytime*Medu*failures.y*Dalc*absences.y*higher*school*paid*sex*address, data = g)
summary(mG1ym2)

# Medu:addressU *
# Dalc:schoolMS:sexM **
# Medu:absences.y *

###########################################################################
## Observations:
## mG1xm2
## Medu:addressU *
## Dalc:schoolMS:sexM **
## Medu:absences.y *
## 
## mG1ym2
## Medu:addressU *
## Dalc:schoolMS:sexM **
## Medu:absences.y *
##
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
###########################################################################