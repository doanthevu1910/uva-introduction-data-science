setwd("D:/UvA/Year 1/Block 6/Introduction Data Science - Data Preprocessing/Week 1/Assignment 1")

rm(list = ls())

getwd()

library(ggplot2)

#1
##1
library(Pareto)

set.seed(100)

Data <- data.frame(x.n=rnorm(50000), x.p=rPareto(50000,t=1,alpha=2))

ggplot(data = Data) + 
  geom_boxplot(mapping = aes(x="", y=x.n))

ggplot(data = Data) + 
  geom_histogram(mapping = aes(x=x.n)) 

##2
attach(Data)

mean(x.n); sd(x.n)

##3

##4
mean(x.p); sd(x.p)

ggplot(data = Data) + 
  geom_boxplot(mapping = aes(x="", y=x.p)) 

ggplot(data = Data) + 
  geom_histogram(mapping = aes(x=x.p)) 

#2

##1
Data2 <- read.table("DataAssignment1.txt", sep=","); head(Data2)

ggplot(data = Data2) + 
  geom_boxplot(mapping = aes(x="", y=Data2[,1])) 

ggplot(data = Data2) + 
  geom_histogram(mapping = aes(x=Data2[,1]))

##2
sort(Data2[,1])

Data2_filtered <- Data2[-which(Data2 < 0 | Data2 > 4000), 1]
Data2_log <- log(Data2_filtered)

ggplot() + 
  geom_boxplot(mapping = aes(x="", y=Data2_log)) 

ggplot() + 
  geom_histogram(mapping = aes(x=Data2_log)) 

##3

summary(Data2)

summary(Data2_filtered)

##4

rolling_mean <- function(x) {
  cumsum(head(Data2_filtered, n = x))[x]/x
}

rolling_median <- function(x) {
  median(head(Data2_filtered, n = x))
}

rmean <- sapply(1:length(Data2_filtered), rolling_mean)

rmedian <- sapply(1:length(Data2_filtered), rolling_median)

plot(x=rmean, type = "l", col="red", ylim = c(0,10))
lines(x=rmedian, type = "l", col = "blue")

ggplot() + 
  geom_line(mapping = aes(x=1:length(Data2_filtered), y=rmean), col = "red") + 
  geom_line(mapping = aes(x=1:length(Data2_filtered), y=rmedian), col = "blue") +
  ylim(0, 10)