rm(list=ls())

################
## Problem #2 ##
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
summary(glm(outcome ~ zn + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data=df, family="binomial"))

############
## Part B ##
############
k <- 4
df$CV_id <- sample(1:k, dim(df)[1], replace=TRUE)
table(df$CV_id, exclude=NULL)
CV_vector <- rep(NA, k)

df_train <- dplyr::filter(df, CV_id!=1)
df_test <- dplyr::filter(df, CV_id==1)

df_test$p_outcome <- predict(glm(outcome ~ zn + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data=df_train, family="binomial"), df_test, type="response")
df_test$p_outcome_class <- 0
df_test$p_outcome_class[df_test$p_outcome>0.5] <- 1

table(df_test$p_outcome_class, df_test$outcome)
CV_vector[1] <- (dim(dplyr::filter(df_test, outcome!=p_outcome_class))[1]) / (dim(df_test)[1])

rm(df_train, df_test)


############
## Part C ##
############

for(i in 2:k){
  df_train <- dplyr::filter(df, CV_id!=i)
  df_test <- dplyr::filter(df, CV_id==i)
  
  df_test$p_outcome <- predict(glm(outcome ~ zn + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data=df_train, family="binomial"), df_test, type="response")
  df_test$p_outcome_class <- 0
  df_test$p_outcome_class[df_test$p_outcome>0.5] <- 1
  
  table(df_test$p_outcome_class, df_test$outcome)
  CV_vector[i] <- (dim(dplyr::filter(df_test, outcome!=p_outcome_class))[1]) / (dim(df_test)[1])
  
  rm(df_train, df_test)
}


############
## Part D ##
############

CV_vector_partD <- rep(NA, k)
for(i in 1:k){
  df_train <- dplyr::filter(df, CV_id!=i)
  df_test <- dplyr::filter(df, CV_id==i)
  
  df_test$p_outcome <- predict(glm(outcome ~ zn + indus+ chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat + medv, data=df_train, family="binomial"), df_test, type="response")
  df_test$p_outcome_class <- 0
  df_test$p_outcome_class[df_test$p_outcome>0.5] <- 1
  
  table(df_test$p_outcome_class, df_test$outcome)
  CV_vector_partD[i] <- (dim(dplyr::filter(df_test, outcome!=p_outcome_class))[1]) / (dim(df_test)[1])
  
  rm(df_train, df_test)
}







