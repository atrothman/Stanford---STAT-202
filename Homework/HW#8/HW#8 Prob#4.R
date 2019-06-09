rm(list=ls())

################
## Problem #4 ##
################
library(ISLR)
library(dplyr)
library(ggplot2)
library(e1071)

############
## Part A ##
############
set.seed(1234)
df <- OJ
train_id <- sample(dim(df)[1], 800)
train_df <- df[train_id,]
test_df <- df[-train_id,]

############
## Part B ##
############
svc_linear <- svm(Purchase~., data=train_df, kernel="linear", cost=0.01)
summary(svc_linear)

############
## Part C ##
############
train_df$svc_linear_p <- predict(svc_linear, train_df)
table(train_df$Purchase, train_df$svc_linear_p)

test_df$svc_linear_p <- predict(svc_linear, test_df)
table(test_df$Purchase, test_df$svc_linear_p)

############
## Part D ##
############
set.seed(1234)
tune_cost_svc_linear <- tune(svm, Purchase~., data=select(train_df, -svc_linear_p), kernel="linear", ranges=list(cost=seq(0.01,10,0.1)))
summary(tune_cost_svc_linear)

############
## Part E ##
############
svc_linear_tuned <- svm(Purchase~., data=select(train_df,-svc_linear_p), kernel="linear", cost=tune_cost_svc_linear$best.parameters$cost)
train_df$svc_linear_tuned_p <- predict(svc_linear_tuned, select(train_df,-svc_linear_p))
table(train_df$Purchase, train_df$svc_linear_tuned_p)

test_df$svc_linear_tuned_p <- predict(svc_linear_tuned, select(test_df,-svc_linear_p))
table(test_df$Purchase, test_df$svc_linear_tuned_p)

############
## Part F ##
############
set.seed(1)
svm_radial <- svm(Purchase~., data=train_df, kernel="radial", cost=0.01)
summary(svm_radial)

train_df$svm_radial_p <- predict(svm_radial, train_df)
table(train_df$Purchase, train_df$svm_radial_p)

test_df$svm_radial_p <- predict(svm_radial, test_df)
table(test_df$Purchase, test_df$svm_radial_p)

set.seed(1234)
tune_cost_svm_radial <- tune(svm, Purchase~., data=select(train_df, -svm_radial_p), kernel="radial", ranges=list(cost=seq(0.01,10,0.1)))
summary(tune_cost_svm_radial)

svm_kernel_tuned <- svm(Purchase~., data=select(train_df,-svm_radial_p), kernel="radial", cost=tune_cost_svm_radial$best.parameters$cost)
train_df$svm_kernel_tuned_p <- predict(svm_kernel_tuned, select(train_df,-svm_radial_p))
table(train_df$Purchase, train_df$svm_kernel_tuned_p)

test_df$svm_kernel_tuned_p <- predict(svm_kernel_tuned, select(test_df,-svm_radial_p))
table(test_df$Purchase, test_df$svm_kernel_tuned_p)

############
## Part G ##
############
set.seed(1)
svm_poly <- svm(Purchase~., data=train_df, kernel="polynomial", degree=2, cost=0.01)
summary(svm_poly)

train_df$svm_poly_p <- predict(svm_poly, train_df)
table(train_df$Purchase, train_df$svm_poly_p)

test_df$svm_poly_p <- predict(svm_poly, test_df)
table(test_df$Purchase, test_df$svm_poly_p)

set.seed(1234)
tune_cost_svm_poly <- tune(svm, Purchase~., data=select(train_df, -svm_poly_p), kernel="polynomial", degree=2, ranges=list(cost=seq(0.01,10,0.1)))
summary(tune_cost_svm_poly)

svm_poly_tuned <- svm(Purchase~., data=select(train_df,-svm_poly_p), kernel="polynomial", degree=2, cost=tune_cost_svm_poly$best.parameters$cost)
train_df$svm_poly_tuned_p <- predict(svm_poly_tuned, select(train_df,-svm_poly_p))
table(train_df$Purchase, train_df$svm_poly_tuned_p)

test_df$svm_poly_tuned_p <- predict(svm_poly_tuned, select(test_df,-svm_poly_p))
table(test_df$Purchase, test_df$svm_poly_tuned_p)









