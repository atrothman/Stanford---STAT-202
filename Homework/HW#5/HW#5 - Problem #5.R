rm(list=ls())

################
## Problem #5 ##
################
library(leaps)
library(dplyr)
library(glmnet)
library(psych)
library(ggplot2)
library(ISLR)
library(pls)
set.seed(123456)

## load the "college" dataset
df <- College
head(df)
df$private_school <- 0
df$private_school[df$Private=='Yes'] <- 1
table(df$Private, df$private_school)
df <- dplyr::select(df, -Private)
head(df)

## specify the "CV_id"
k <- 10
df$CV_id <- sample(c(1:k), length(df$Apps), replace=TRUE)
table(df$CV_id)

CV_trial <- c(1:k)
MSE_df <- data.frame(CV_trial)
MSE_df$LASSO <- NA
MSE_df$Ridge <- NA
MSE_df$BestSub <- NA
MSE_df$ForwS <- NA
MSE_df$BackD <- NA
MSE_df$PCR <- NA

for(i in 1:k){
  print(i)
  df_train <- select(filter(df, CV_id!=i), -CV_id)
  df_test <- select(filter(df, CV_id==i), -CV_id)
  
  Y_train <- as.matrix(select(df_train, Apps))
  Y_test <- as.matrix(select(df_test, Apps))
  X_train <- as.matrix(select(df_train, -Apps))
  X_test <- as.matrix(select(df_test, -Apps))
  
  predicted_df <- data.frame(Y_test)
  
  ## LASSO model
  LASSO_CV_tuned <- cv.glmnet(X_train, Y_train, alpha=1)
  lambda_LASSO <- LASSO_CV_tuned$lambda.min
  LASSO <- glmnet(X_train, Y_train, alpha=1, lambda=lambda_LASSO)
  predicted_df$LASSO_p  <- predict(LASSO, s=lambda_LASSO, newx=X_test)
  rm(LASSO_CV_tuned, lambda_LASSO, LASSO)
  
  ## Ridge Model
  Ridge_CV_tuned <- cv.glmnet(X_train, Y_train, alpha=0)
  lambda_Ridge <- Ridge_CV_tuned$lambda.min
  Ridge <- glmnet(X_train, Y_train, alpha=0, lambda=lambda_Ridge)
  predicted_df$Ridge_p  <- predict(Ridge, s=lambda_Ridge, newx=X_test)
  rm(Ridge_CV_tuned, lambda_Ridge, Ridge)
  
  ## Best Subset
  bs <- summary(regsubsets(Apps~., data=df_train, nvmax=17))
  bs_vector <- bs$which[which.max(bs$adjr2),]
  bs_variables <- names(bs_vector[bs_vector==TRUE])[2:length(bs_vector[bs_vector==TRUE])]
  bs_train <- select(df_train, Apps, bs_variables)
  bs_model <- lm(Apps~., data=bs_train)
  predicted_df$bs_p  <- predict(bs_model, newdata=df_test)
  rm(bs, bs_vector, bs_variables, bs_train, bs_model)
  
  ## Forward Selection
  fs <- summary(regsubsets(Apps~., data=df_train, nvmax=17, method="forward"))
  fs_vector <- fs$which[which.max(fs$adjr2),]
  fs_variables <- names(fs_vector[fs_vector==TRUE])[2:length(fs_vector[fs_vector==TRUE])]
  fs_train <- select(df_train, Apps, fs_variables)
  fs_model <- lm(Apps~., data=fs_train)
  predicted_df$fs_p  <- predict(fs_model, newdata=df_test)
  rm(fs, fs_vector, fs_variables, fs_train, fs_model)
  
  ## Backward Selection
  bw <- summary(regsubsets(Apps~., data=df_train, nvmax=17, method="backward"))
  bw_vector <- bw$which[which.max(bw$adjr2),]
  bw_variables <- names(bw_vector[bw_vector==TRUE])[2:length(bw_vector[bw_vector==TRUE])]
  bw_train <- select(df_train, Apps, bw_variables)
  bw_model <- lm(Apps~., data=bw_train)
  predicted_df$bw_p  <- predict(bw_model, newdata=df_test)
  rm(bw, bw_vector, bw_variables, bw_train, bw_model)
  
  ## PCR
  PCR <- pcr(Apps~., data=df_train, scale=TRUE, validation="CV")
  predicted_df$pcr_p  <- predict(PCR, newdata=df_test, ncomp=which.min(PCR$validation$adj))
  rm(PCR)
  
  MSE_df$LASSO[i] <- (sum(predicted_df$Apps - predicted_df$LASSO_p)^2)/dim(predicted_df)[1]
  MSE_df$Ridge[i] <- (sum(predicted_df$Apps - predicted_df$Ridge_p)^2)/dim(predicted_df)[1]
  MSE_df$BestSub[i] <- (sum(predicted_df$Apps - predicted_df$bs_p)^2)/dim(predicted_df)[1]
  MSE_df$ForwS[i] <- (sum(predicted_df$Apps - predicted_df$fs_p)^2)/dim(predicted_df)[1]
  MSE_df$BackD[i] <- (sum(predicted_df$Apps - predicted_df$bw_p)^2)/dim(predicted_df)[1]
  MSE_df$PCR[i] <- (sum(predicted_df$Apps - predicted_df$pcr_p)^2)/dim(predicted_df)[1]
  
  rm(predicted_df)
}

describe(MSE_df)

Y <- as.matrix(select(df, Apps))
X <- as.matrix(select(df, -Apps, -CV_id))
Ridge_CV_tuned <- cv.glmnet(X, Y, alpha=0)
plot(Ridge_CV_tuned)
lambda_Ridge <- Ridge_CV_tuned$lambda.min
Ridge <- glmnet(X, Y, alpha=0, lambda=lambda_Ridge)


