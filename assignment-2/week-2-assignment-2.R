setwd("D:/UvA/Year 1/Block 6/Introduction Data Science - Data Preprocessing/Assignments/Assignment 2")

rm(list = ls())

getwd()

Data=data.matrix(read.table("InterestRates.txt", header=FALSE))
head(Data)

#1 use day 1, 200, 400, 600 and 800

library(ggplot2)

d1 <- NULL
for (i in 1:ncol(Data)) {
  d1[i] <- Data[1,i]
}

d200 <- NULL
for (i in 1:ncol(Data)) {
  d200[i] <- Data[200,i]
}

d400<- NULL
for (i in 1:ncol(Data)) {
  d400[i] <- Data[400,i]
}

d600 <- NULL
for (i in 1:ncol(Data)) {
  d600[i] <- Data[600,i]
}

d800 <- NULL
for (i in 1:ncol(Data)) {
  d800[i] <- Data[800,i]
}

ggplot() + 
  geom_line(mapping = aes(x=1:51, y=d1), col = 1, size = 1) + 
  geom_line(mapping = aes(x=1:51, y=d200), col = 2, size = 1) + 
  geom_line(mapping = aes(x=1:51, y=d400), col = 3, size = 1) + 
  geom_line(mapping = aes(x=1:51, y=d600), col = 4, size = 1) +  
  geom_line(mapping = aes(x=1:51, y=d800), col = 5, size = 1) +
  labs(x="maturity level", y="yield") 

#2 use overnight and 5-year maturity

v1 <- NULL
for (i in 1:nrow(Data)) {
  v1[i] <- Data[i, 1]
}

##5 year maturity
v11 <- NULL
for (i in 1:nrow(Data)) {
  v11[i] <- Data[i, 11]
}

summary(v1); summary(v11)

v1[1]

ggplot() +
  geom_line(mapping = aes(x=1:nrow(Data), y=v1), col = "black") +
  geom_line(mapping = aes(x=1:nrow(Data), y=v11), col = "blue") +
  labs(x="maturity level", y="yield") +
  ggtitle("overnight maturity: black, 5-year maturity: blue")

#3 

Change <- Data[2:1264,] - Data[1:1263,]; head(Change)

pairs(~V3 + V11 + V21 + V31 + V41 + V51, data=Change, pch=20, col="blue", 
      main = "Dependencies between yield changes of 1-year, 5-year, 10-year,... 25-year maturity")

a1<- NULL
for (i in 3:51) {
  a1[i] <- cor(Change[,3], Change[,i])
}

a11<- NULL
for (i in 3:51) {
  a11[i] <- cor(Change[,11], Change[,i])
}

a21<- NULL
for (i in 3:51) {
  a21[i] <- cor(Change[,21], Change[,i])
}

a31<- NULL
for (i in 3:51) {
  a31[i] <- cor(Change[,31], Change[,i])
}

a41<- NULL
for (i in 3:51) {
  a41[i] <- cor(Change[,41], Change[,i])
}

a51<- NULL
for (i in 3:51) {
  a51[i] <- cor(Change[,51], Change[,i])
}

ggplot() + 
  geom_line(mapping = aes(x=1:51, y=a1), col = 1, size = 1) + 
  geom_line(mapping = aes(x=1:51, y=a11), col = 2, size = 1) + 
  geom_line(mapping = aes(x=1:51, y=a21), col = 3, size = 1) + 
  geom_line(mapping = aes(x=1:51, y=a31), col = 4, size = 1) +  
  geom_line(mapping = aes(x=1:51, y=a41), col = 5, size = 1) +
  geom_line(mapping = aes(x=1:51, y=a51), col = 6, size = 1) +
  ggtitle("Correlation between yield changes of 1-year, 5-year, 10-year, 15-year, 20-year and 25-year \nmaturity and every other maturities")
  
#PCA - TESTING
Change_3 <- cbind(Change[,3], Change[,11], Change[,21], Change[,31], Change[,41], Change[,51]); head(Change_3)

Change.PCA <- prcomp(Change_3)

library(ggfortify)
autoplot(Change.PCA, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5,
         col = "blue",
         main="PCA for yield changes for 1-year, 5-year and 10-year maturity")

#4 - use change_scaled data
##original dataset

Change.PCA <- prcomp(Change)

library(ggfortify)
autoplot(Change.PCA, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, col = "blue", 
         main = "PCA for changes in the yield curve (original data)")

summary(Change.PCA)

#keep 4 PC's to explain 96.31% of the variance 
## do we need more?

PC <- Change.PCA$rotation

round(PC[,1], digits = 3)

round(PC[,2], digits = 3)

round(PC[,3], digits = 3)

round(PC[,4], digits = 3)

## standardized dataset
Change_scaled <- scale(Change)

Change_scaled.PCA <- prcomp(Change_scaled)

autoplot(Change_scaled.PCA, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, col = "blue",
         main = "PCA for changes in the yield curve (standardized data)")

#5 

Data.PCA <- prcomp(Data)

autoplot(Data.PCA, loadings = TRUE, loadings.label = TRUE, loadings.label.size = 3, col = "blue", 
         main = "PCA (original data)")

summary(Data.PCA)

#keep 3 PC's to explain 97.07% of the variance

Data.PC <- Data.PCA$rotation

round(Data.PC[,1], digits = 3)

round(Data.PC[,2], digits = 3)

round(Data.PC[,3], digits = 3)

