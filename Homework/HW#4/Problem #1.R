################
## Problem #1 ##
################
library(MASS)
library(dplyr)
library(psych)
set.seed(12345)
df <- Boston

## explore the Boston dataset
dim(df)
head(df)
colnames(df)
?Boston
summary(df)

## specify the median of the crime variable
median_cr <- median(df$crim)

## add in CV ineger varibal for later Cross Validation
k <- 5
df$CV_id <- sample(1:k, dim(df)[1], replace=TRUE)
table(df$CV_id, exclude=NULL)
CV_vector <- rep(NA, k)

## specify the binary outcome
df$outcome <- 0
df$outcome[df$crim>median_cr] <- 1
table(df$outcome, exclude=NULL)

for(i in 1:k){
  df_train <- dplyr::filter(df, CV_id!=i)
  df_test <- dplyr::filter(df, CV_id==i)
  
  df_test$p_outcome <- predict(glm(outcome ~ indus + age + tax, family=binomial, data=df_train), df_test, type="response")
  df_test$p_outcome_class <- 0
  df_test$p_outcome_class[df_test$p_outcome>0.5] <- 1
  
  CV_vector[i] <- (dim(dplyr::filter(df_test, outcome==p_outcome_class))[1]) / (dim(df_test)[1])
  
  rm(df_train, df_test)  
}

CV_performance <- mean(CV_vector)

CV_vector
CV_performance



