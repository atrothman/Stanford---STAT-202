rm(list=ls())

################
## Problem #4 ##
################
library(leaps)
library(dplyr)
library(glmnet)
library(psych)
library(ggplot2)
set.seed(10815657)

############
## Part A ##
############
n=100
X <- rt(n, 15)
error <- rt(n, 15)
psych::describe(X)
psych::describe(error)


############
## Part B ##
############
Y <- 0.5 + (0.5*X) + (0.5*(X^2)) + (0.5&(X^3)) + error
psych::describe(Y)

############
## Part C ##
############
train_df <- data.frame(Y,X)
train_df$X2 <- X^2
train_df$X3 <- X^3
train_df$X4 <- X^4
train_df$X5 <- X^5
train_df$X6 <- X^6
train_df$X7 <- X^7
train_df$X8 <- X^8
train_df$X9 <- X^9
train_df$X10 <- X^10
head(train_df)

num_variables <- c(1:10)
best_subset <- summary(regsubsets(Y~., data=train_df, nvmax=10))
bs_df <- data.frame(num_variables)
bs_df$cp <- best_subset$cp
bs_df$bic <- best_subset$bic
bs_df$adjr2 <- best_subset$adjr2
bs_df
summary(lm(Y~X+X2, data=train_df))
summary(lm(Y~X+X2+X4+X6+X8, data=train_df))

ggplot(bs_df, aes(x=num_variables, y=cp)) + geom_point() + geom_line(color="red") + 
  ggtitle("Mallow's Cp on training data using Best Subset Selection") +  xlab("Number of predictors in the model") + ylab("Mallows's Cp") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(bs_df, aes(x=num_variables, y=bic)) + geom_point() + geom_line(color="blue") + 
  ggtitle("BIC on training data using Best Subset Selection") +  xlab("Number of predictors in the model") + ylab("Bayesian Information Criterion (BIC)") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(bs_df, aes(x=num_variables, y=adjr2)) + geom_point() + geom_line(color="green") + 
  ggtitle("Adj. R^2 on training data using Best Subset Selection") +  xlab("Number of predictors in the model") + ylab("Adjusted R^2") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)


############
## Part D ##
############
forward_selection <- summary(regsubsets(Y~., data=train_df, nvmax=10, method="forward"))
fs_df <- data.frame(num_variables)
fs_df$cp <- forward_selection$cp
fs_df$bic <- forward_selection$bic
fs_df$adjr2 <- forward_selection$adjr2
fs_df
summary(lm(Y~X+X2, data=train_df))
summary(lm(Y~X+X2+X4+X8+X10, data=train_df))

ggplot(fs_df, aes(x=num_variables, y=cp)) + geom_point() + geom_line(color="red") + 
  ggtitle("Mallow's Cp on training data using Forward Stepwise Selection") +  xlab("Number of predictors in the model") + ylab("Mallows's Cp") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(fs_df, aes(x=num_variables, y=bic)) + geom_point() + geom_line(color="blue") + 
  ggtitle("BIC on training data using Forward Stepwise Selection") +  xlab("Number of predictors in the model") + ylab("Bayesian Information Criterion (BIC)") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(fs_df, aes(x=num_variables, y=adjr2)) + geom_point() + geom_line(color="green") + 
  ggtitle("Adj. R^2 on training data using Forward Stepwise Selection") +  xlab("Number of predictors in the model") + ylab("Adjusted R^2") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

backward_selection <- summary(regsubsets(Y~., data=train_df, nvmax=10, method="backward"))
bs_df <- data.frame(num_variables)
bs_df$cp <- backward_selection$cp
bs_df$bic <- backward_selection$bic
bs_df$adjr2 <- backward_selection$adjr2
bs_df
summary(lm(Y~X2+X3+X4+X5+X6+X8, data=train_df))
summary(lm(Y~X2+X3+X8, data=train_df))
summary(lm(Y~X2+X3+X4+X5+X6+X7+X8, data=train_df))

ggplot(bs_df, aes(x=num_variables, y=cp)) + geom_point() + geom_line(color="red") + 
  ggtitle("Mallow's Cp on training data using Backward Stepwise Selection") +  xlab("Number of predictors in the model") + ylab("Mallows's Cp") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(bs_df, aes(x=num_variables, y=bic)) + geom_point() + geom_line(color="blue") + 
  ggtitle("BIC on training data using Backward Stepwise Selection") +  xlab("Number of predictors in the model") + ylab("Bayesian Information Criterion (BIC)") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(bs_df, aes(x=num_variables, y=adjr2)) + geom_point() + geom_line(color="green") + 
  ggtitle("Adj. R^2 on training data using Backward Stepwise Selection") +  xlab("Number of predictors in the model") + ylab("Adjusted R^2") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

############
## Part E ##
############
X_train <- as.matrix(dplyr::select(train_df, -Y))
LASSO_CV_tuned <- cv.glmnet(X_train, Y, alpha=1)
plot(LASSO_CV_tuned)
tuned_lambda <- LASSO_CV_tuned$lambda.min
LASSO <- glmnet(X_train, Y, alpha=1, lambda=tuned_lambda)
LASSO$a0
LASSO$beta

############
## Part F ##
############
Y_alt <- 0.5 + (0.5&(X^7)) + error

train_df_alt <- data.frame(Y_alt,X)
train_df_alt$X2 <- X^2
train_df_alt$X3 <- X^3
train_df_alt$X4 <- X^4
train_df_alt$X5 <- X^5
train_df_alt$X6 <- X^6
train_df_alt$X7 <- X^7
train_df_alt$X8 <- X^8
train_df_alt$X9 <- X^9
train_df_alt$X10 <- X^10
head(train_df_alt)

num_variables <- c(1:10)
best_subset <- summary(regsubsets(Y_alt~., data=train_df_alt, nvmax=10))
bs_df <- data.frame(num_variables)
bs_df$cp <- best_subset$cp
bs_df$bic <- best_subset$bic
bs_df$adjr2 <- best_subset$adjr2
bs_df

summary(lm(Y_alt~X8+X10, data=train_df_alt))
summary(lm(Y_alt~X2, data=train_df_alt))
summary(lm(Y_alt~X2+X4+X6+X8, data=train_df_alt))

ggplot(bs_df, aes(x=num_variables, y=cp)) + geom_point() + geom_line(color="red") + 
  ggtitle("Mallow's Cp on training data using Best Subset Selection") +  xlab("Number of predictors in the model") + ylab("Mallows's Cp") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(bs_df, aes(x=num_variables, y=bic)) + geom_point() + geom_line(color="blue") + 
  ggtitle("BIC on training data using Best Subset Selection") +  xlab("Number of predictors in the model") + ylab("Bayesian Information Criterion (BIC)") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

ggplot(bs_df, aes(x=num_variables, y=adjr2)) + geom_point() + geom_line(color="green") + 
  ggtitle("Adj. R^2 on training data using Best Subset Selection") +  xlab("Number of predictors in the model") + ylab("Adjusted R^2") +
  theme(plot.title = element_text(hjust = 0.5)) + scale_x_continuous(breaks=num_variables)

X_train <- as.matrix(dplyr::select(train_df_alt, -Y_alt))
LASSO_CV_tuned <- cv.glmnet(X_train, Y_alt, alpha=1)
plot(LASSO_CV_tuned)
tuned_lambda <- LASSO_CV_tuned$lambda.min
LASSO <- glmnet(X_train, Y_alt, alpha=1, lambda=tuned_lambda)
LASSO$a0
LASSO$beta





