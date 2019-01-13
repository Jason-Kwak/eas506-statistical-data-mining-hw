###########################################################################
## This code is to explore some data using different visualizations (EDA)
## Paul M Girdler
## Created: Sep 09, 2018
## 
###########################################################################

# Aim is to prepare data to build a Predictive model to forecast outcomes of 
# First Period Grades G1.x and G1.y


# NOTES from lecturer
# Rescale variables if required
# Identify possible outliers try not to remove if required
# Combine variables if required.

# Clear the memory
rm(list = ls())

# Set working directory

setwd("C:/Users/paul_/Documents/student/")

#install packages if required
#install.packages("ggplot2")
#install.packages("lattice")
#install.packages("car")
#install.packages("corrplot")
library(ggplot2)
library(lattice)
library(car)
library(corrplot)
library(data.table)

# Import Data
D1=read.table("student-mat.csv",sep=";",header=TRUE)
D2=read.table("student-por.csv",sep=";",header=TRUE)
student=merge(D1,D2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(student)) # 382 students
print(ncol(student)) # 53 variables
head(student)

# Looks like ther is some redundant data

###########################################################################
## Checking for redundant data by plotting a Correlation Plot
## 
## 
## 
###########################################################################


nums <- unlist(lapply(student, is.numeric))
student_numeric <- subset(student[,nums])
corrstudent <- cor(student_numeric)
#png("Corrplot_Student.png")
M <- cor(corrstudent)
corrplot(M)
#dev.off

###########################################################################
## duplicated columns(numeric): 
##
## traveltime.x/.y
## studytime.x/.y
## famrel.x/.y
## freetime.x/.y
## goout.x/gout.y
## Dalc.x/.y
## Walc.x
## health.x/.y
## 
## Also by inspection the following cat columns(categorical) are duplicated:
## guardian.x/.y
## schoolsup.x/.y
## famsup.x/.y
## paid.x/paid.y
## higher.x/.y
## romantic.x/.y
## activities.x/.y
##
###########################################################################

###########################################################################
## Create subset of student and remove repeated columns
## 
## 
## 
###########################################################################
## traveltime.x/.y
## studytime.x/.y
## famrel.x/.y
## freetime.x/.y
## goout.x/gout.y
## Dalc.x/.y
## Walc.x
## health.x/.y
## guardian.x/.y
## schoolsup.x/.y
## famsup.x/.y
## paid.x/paid.y
## higher.x/.y
## romantic.x/.y
## activities.x/.y

drop <- c("guardian.x","traveltime.x","studytime.x", "schoolsup.x", "famsup.x", "paid.x",
          "activities.x", "higher.x", "romantic.x", "famrel.x", "freetime.x", "goout.x",
          "Dalc.x", "Walc.x", "health.x")
d = student[,!(names(student) %in% drop)]

# Rename duplicated columns to remove .y
library(data.table)
setnames(d, 
old = c("guardian.y","traveltime.y","studytime.y", "schoolsup.y", "famsup.y", "paid.y",
        "activities.y", "higher.y", "romantic.y", "famrel.y", "freetime.y", "goout.y",
        "Dalc.y", "Walc.y", "health.y"),
new = c("guardian","traveltime","studytime", "schoolsup", "famsup", "paid",
        "activities", "higher", "romantic", "famrel", "freetime", "goout",
        "Dalc", "Walc", "health"))

# The data looks much nicer

print(nrow(d)) # 382 students
print(ncol(d)) # 38 variables down from 53
head(d)

# Create a vector pretty names to help with main and axis titles
pretty_names = c("School Attended", "Sex", "Age", "Urban / Rural", "Family Size", "Parent's Cohabitation",  
"Mother's Education", "Father's Education", "Mother's Occupation Type", "Father's Occupation Type", "Reason to Choose This School", "Attended Nursery School Y/N", 
"Internet at Home Y/N", "Number of Past Class Failures (Maths)", "Absences (Maths)", "Grade 1 (Maths)", "Grade 2 (Maths)", "Grade 3 (Maths)",    
"Guardian", "Travel Time to School", "Study Time", "Failures (Port)", "Extra Educational Support (Y/N)", "Family Educational Support (Y/N)",  
"Extra Paid Tutoring", "Extra-curricular activities (Yes/No)", "Higher Education Aspirations", "Romantic Relationship (Y/N)", "Quality of Family Relationships", "Free Time After School", 
"Going Out With Friends", "Workday Alcohol Consumption", " Weekend Alcohol Consumption", "Current Health Status", "Absences (Port)", "Grade 1 (Port)",     
"Grade 2 (Port)","Grade 3 (Port)")

# Test pretty names
pretty_names[which( colnames(d)=="Walc" )]

###########################################################################
## Looking at G1 data
##
##
###########################################################################
for (xvar in c("G1.x", "G1.y", "G2.x", "G2.y", "G3.x", "G3.y")){
  #xvar <- "G1.x" # X axis plotting variable
  ptype <- "Histogram" # Graph Type
  fname <- paste(ptype, "_", xvar, ".png", sep = "")
  png(fname)
  colnum <- which( colnames(d)==xvar)
  xname <- pretty_names[colnum]
  title <- paste("Histogram of", xname, sep = " ", collapse = NULL)
  hist(d[,colnum],
     main = (title),
     xlab = xname, 
     xlim=c(0,20),
     ylim=c(0,150))
  dev.off()
}

for (xvar in c("G1.x", "G1.y", "G2.x", "G2.y", "G3.x", "G3.y")){
  #xvar <- "G1.y" # X axis plotting variable
  ptype <- "Boxplot" # Graph Type
  fname <- paste(ptype, "_", xvar, ".png", sep = "")
  png(fname)
  colnum <- which( colnames(d)==xvar)
  xname <- pretty_names[colnum]
  title <- paste(ptype, "of", xname, sep = " ", collapse = NULL)
  boxplot(d[,colnum],
        data=d,
        main = title, 
        ylim=c(0, 20))
  dev.off()
}

#  G1.x and G1.y < 5 is an outlier

# Tested linear regression with all values.
# Will now try removing these values

e <- subset(d, d$G1.x > 5)
e <- subset(d, d$G1.y > 5)

# Removed 2 rows

###########################################################################
## Simple Scatterplot Matrices of Numeric Variables
##
##
###########################################################################

library(car)
scatterplotMatrix(~G1.x+Medu+Fedu+famrel, data=e,
                  main="Enhanced Scatterplot Matrix: Numeric Variables Related to Family Environment (Maths)")
scatterplotMatrix(~G1.x+traveltime+studytime+freetime+goout, data=e,
                  main="Enhanced Scatterplot Matrix: Numeric Variables Related to Time Management (Maths)")
scatterplotMatrix(~G1.x+Dalc+Walc+health, data=e,
                  main="Enhanced Scatterplot Matrix: Numeric Variables Related to Health (Maths)")
scatterplotMatrix(~G1.x+age+failures.x+absences.x, data=e,
                  main="Enhanced Scatterplot Matrix: Misc Numeric Variables (Maths)")

scatterplotMatrix(~G1.y+Medu+Fedu+famrel, data=e,
                  main="Enhanced Scatterplot Matrix: Numeric Variables Related to Family Environment (Port)")
scatterplotMatrix(~G1.y+traveltime+studytime+freetime+goout, data=e,
                  main="Enhanced Scatterplot Matrix: Numeric Variables Related to Time Management (Port)")
scatterplotMatrix(~G1.y+Dalc+Walc+health, data=e,
                  main="Enhanced Scatterplot Matrix: Numeric Variables Related to Health (Port)")
scatterplotMatrix(~G1.y+age+failures.y+absences.y, data=e,
                  main="Enhanced Scatterplot Matrix: Misc Numeric Variables (Port)")

###########################################################################
## Observations from Scatter Plot Matrix
##
##
###########################################################################

# G1 seems to be normally distributed around a mean.

# Obervations from Family Environment
###########################################################################
# Fedu and Medu seem to be related.
# Both Fedu and Medu seem to have a slight positive effect on G1
# Medu is a better predictor of G1
# Might come up with a combined variable ave(Fedu, Medu)
# Tested combined variable ave(Fedu, Medu) and it wasn't as effective as a predictor.
# DROP Fedu.

# famrel seems to have no effect on G1. 
# DROP famrel.

# Observations from Time Management
###########################################################################
# Study time has a slight positive effect on G1.
# Travel time has a slight negative effect on G1.
# Travel time has a very slight negative effect on study time. i.e. slightly related.
# freetime has a positive effect on goout
# freetime and gout don't have seem to have an effect on G1
# DROP freetime goout

# Observations from Health
###########################################################################
# Dalc, Walc and Health have a slight negative effect on G1
# Dalc and Walc are related.
# Tested a variable weighted average of Dalc and Walc. It wasn't any more effective.
# Surprisingly Dalc and Walc have no effect on health.
# DROP Walc
#
# Observations Misc
###########################################################################
# age has a negative effect on G1.
# failures has a negative effect on G1
# absences has a negative effect on G1

###########################################################################
## Drop insignifcant variables
##
##
###########################################################################

drop <- c("Fedu", "famrel","freetime", "goout","Walc")
f = e[,!(names(e) %in% drop)]
print(nrow(f)) # 382 students
print(ncol(f)) # 33 variables down from 53
head(d)

###########################################################################
## More Detailed Scatter Plots and Box Plots
##
##
###########################################################################

#attach(e)
#yvar <- G1.x
#xvar <- age
#colnum <- which(colnames(d)==paste(yvar, sep="")
#yname <- pretty_names[colnum]
#colnum <- which(colnames(d)==paste(xvar, sep="")
#xname <- pretty_names[colnum]
#title <- paste(yname, xname)
#scatterplot(xvar, yvar,
#            regLine=TRUE, legend=TRUE,
#            smooth=TRUE,
#            xlab=xname, ylab=yname)

###########################################################################
## Boxplots of Categorical Binary Data
##
###########################################################################

# Social Factors school, sex, address, nursery, romantic
append(e)
for (xvar in c("school", "sex", "address", "famsize", "Pstatus", "schoolsup", 
"famsup", "paid", "activities", "nursery", "higher", "internet", "romantic")){
  yvar <- "G1.x"
  colnum <- which(colnames(d)==yvar)
  yname <- pretty_names[colnum]
  colnum <- which(colnames(d)==xvar)
  xname <- pretty_names[colnum]
  ptype <- "Boxplot" # Graph Type
  fname <- paste(ptype, "_", "G1x", "v",xvar, ".png", sep = "")
  png(fname)
  title <- paste(ptype, "of", yname, " vs ", xname, sep = " ", collapse = NULL)
  boxplot(e[,yvar]~e[,xvar],
          data=e,
          main = title,
          xlab = xname,
          ylab = yname,
          ylim=c(0, 20))
  dev.off()
}


append(e)

for (xvar in c("school", "sex", "address", "famsize", "Pstatus", "schoolsup", 
               "famsup", "paid", "activities", "nursery", "higher", "internet", "romantic")){
  yvar <- "G1.y"
  colnum <- which(colnames(d)==yvar)
  yname <- pretty_names[colnum]
  colnum <- which(colnames(d)==xvar)
  xname <- pretty_names[colnum]
  ptype <- "Boxplot" # Graph Type
  fname <- paste(ptype, "_", "G1y", "v",xvar, ".png", sep = "")
  png(fname)
  title <- paste(ptype, "of", yname, " vs ", xname, sep = " ", collapse = NULL)
  boxplot(e[,yvar]~e[,xvar],
          data=e,
          main = title,
          xlab = xname,
          ylab = yname,
          ylim=c(0, 20))
  dev.off()
}

###########################################################################
## Drop insignifcant variables
##
##
###########################################################################

# See below for the variables dropped because of insignificance to the response

drop <- c("famsize", "Pstatus", "famsup", "nursery", "internet", "romantic")
g = f[,!(names(f) %in% drop)]
print(nrow(g)) # 382 students,
print(ncol(g)) # 27 variables down from 53

#save(g,file="cleaned_student_data.rdata")