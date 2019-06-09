### load libraries
library(dplyr)
library(MASS)
library(Hmisc)
library(ggplot2)

## Part A
df <- Boston
head(df)
dim(df)

## Part B
pairs(dplyr::select(df, tax, nox, medv))

## Part C
summary(lm(crim ~ zn, data=df))
summary(lm(crim ~ indus, data=df))
summary(lm(crim ~ chas, data=df))
summary(lm(crim ~ nox, data=df))
summary(lm(crim ~ rm, data=df))
summary(lm(crim ~ age, data=df))
summary(lm(crim ~ dis, data=df))
summary(lm(crim ~ rad, data=df))
summary(lm(crim ~ tax, data=df))
summary(lm(crim ~ ptratio, data=df))
summary(lm(crim ~ black, data=df))
summary(lm(crim ~ lstat, data=df))
summary(lm(crim ~ medv, data=df))

## Part D
summary(df$crim)
boxplot(df$crim, xlab="per capita crime rate")

summary(df$tax)
boxplot(df$tax, xlab="full-value property-tax rate per $10,000")

summary(df$ptratio)
boxplot(df$ptratio, xlab="pupil-teacher ratio by town")

## Part E
table(df$chas, exclude=NULL)

## part F
summary(df$ptratio)

## part G
arrange(df, medv)

## Part H
arrange(df, desc(rm))
df_7 <- filter(df, rm>7)
df_8 <- filter(df, rm>8)
summary(df_8)
summary(df)

