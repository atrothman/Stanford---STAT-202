rm(list=ls())

################
## Problem #3 ##
################
library(MASS)
library(dplyr)
library(psych)
set.seed(10815657)
df <- Boston

## explore the Boston dataset
dim(df)
head(df)
colnames(df)
?Boston
summary(df)

## specify the median of the crime variable
median_cr <- median(df$crim)

## specify the binary outcome
df$outcome <- 0
df$outcome[df$crim>median_cr] <- 1
table(df$outcome, exclude=NULL)

############
## Part A ##
############
summary(glm(outcome ~ chas + rm, family="binomial", data=df))

############
## Part B ##
############
boot.fn <- function(data, index){
  return(coef(glm(outcome ~ chas + rm, data=data, family="binomial", subset=index)))
}

boot.fn(df, 1:(dim(df)[1]))


############
## Part C ##
############
library(boot)
boot(df, boot.fn, 1000)







