# Clear the memory
rm(list = ls())

# Set working directory
setwd("C:/Users/paul_/Documents/boston/")

# Install packages if required
install.packages("ggplot2")
install.packages("MASS")
install.packages("corrplot")
library("MASS")
library("ggplot2")
 
# Description of data set
?Boston

boston <- Boston

# Looking at the data
head(boston)

###########################################################################
## Look at Correlation via Corrplot
## 
## 
## 
###########################################################################

library(corrplot)
M <- cor(boston)
corrplot(M, method="ellipse")

###########################################################################
## Look at Correlation via Enhanced Scatterplots
## 
## 
## 
###########################################################################

library(car)
scatterplotMatrix(~crim+medv+rm+age+tax, data=boston,
                  main="Enhanced Scatterplot Matrix: Comp Feature")
scatterplotMatrix(~crim+medv+zn+indus+dis+rad+ptratio+black+lstat, data=boston,
                  main="Enhanced Scatterplot Matrix: Neighbourhood Feature")

###########################################################################
## Look at Crime Rates by Town
## 
## 
## 
###########################################################################

png("Histogram of crim.png")
histogram(boston$crim,
          main="Histogram of per capita per town Crime Rate in Boston",
          xlab="per capita per town Crime Rate",
          ylab="Total Proportion (%)")
dev.off

# Anything over 20 seems top 10% in crime

top_crime <- subset(boston, boston$crim > 18.8)
top_crime <- top_crime[ -c(2:14) ]

# top_crime is the Top 20 crime suburbs in Boston

###########################################################################
## Look at Tax Rates by Town
## 
## 
## 
###########################################################################

png("Histogram of tax.png")
histogram(boston$tax,
          main="Histogram of Full-value property-tax rate per $10k",
          xlab="Full-value property-tax rate per $10k",
          ylab="Total Proportion (%)")
dev.off

# Bi-modal or tri-modal distribution

top_prop_tax <- subset(boston, boston$tax > 665)
top_prop_tax <- top_prop_tax[ -c(1:9,11:14) ]

###########################################################################
## Look at Pupil Teacher Ratios by Town
## 
## 
## 
###########################################################################

png("Histogram of ptration.png")
histogram(boston$ptratio,
          main="Histogram of Pupil-teacher ratio by town",
          xlab="Pupil-teacher ratio",
          ylab="Total Proportion (%)")
dev.off

# Skewed distribution

top_pup_teach <- subset(boston, boston$ptratio > 21)
top_pup_teach <- top_pup_teach[ -c(1:10,12:14) ]

# top_pup_teach is Top 20 suburbs with the worst pupil teacher ratio

###########################################################################
## How many average 7 or 8 rooms per dwelling?
## 
## 
## 
###########################################################################
png("Histogram of rm.png")
histogram(boston$rm,
          main="Histogram of Average Number of Rooms per Dwelling by Suburb",
          xlab="Ave No. of Rooms per Dwelling",
          ylab="Total Proportion (%)")
dev.off

# Looks like normal distribution around 5.5 - 6.5

# Average MORE than 7 = 64
ave_rm_7 <- subset(boston, boston$rm > 7)
print(nrow(ave_rm_7))

# Average MORE than 8 = 13
ave_rm_8 <- subset(boston, boston$rm > 8)
print(nrow(ave_rm_8))

