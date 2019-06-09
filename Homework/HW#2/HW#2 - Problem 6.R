rm(list=ls())

################
## Problem #6 ##
################
library(ISLR)

df <- Carseats
head(df)
colnames(df)
dim(df)

## Part A
summary(lm(Sales ~ Price + Urban + US, data=df))
table(df$Price, exclude=NULL)
table(df$Urban, exclude=NULL)
table(df$US, exclude=NULL)

## Part E
summary(lm(Sales ~ Price + US, data=df))
