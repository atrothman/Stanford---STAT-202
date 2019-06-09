rm(list=ls())

################
## Problem #3 ##
################
library(dplyr)
library(ggplot2)
set.seed(1234)

############
## Part A ##
############
id <- c(1:500)
df <- data.frame(id)
df$x1 <- rnorm(500)
df$error <- rnorm(500)
df$x2 <- 5*((df$x1)^2) + df$error
df$x2[df$id<=250] <- df$x2[df$id<=250]+5
df$x2[df$id>250] <- df$x2[df$id>250]-5
df$y <- 0
df$y[df$id>250] <- 1
df$y <- as.factor(df$y)

############
## Part B ##
############
ggplot(df, aes(x=x1, y=x2, color=y)) + geom_point()

############
## Part C ##
############
logit_model <- glm(y~., data=select(df, y, x1, x2), family="binomial")
summary(logit_model)

############
## Part D ##
############
df$logit_p <- predict(logit_model, df, type="response")
df$logit_p_class <- 0
df$logit_p_class[df$logit_p>=0.5] <- 1
df$logit_p_class <- as.factor(df$logit_p_class)
ggplot(df, aes(x=x1, y=x2, color=logit_p_class)) + geom_point()

############
## Part E ##
############
df$x1_squared <- df$x1*df$x1
df$x2_squared <- df$x2*df$x2
df$x1_x2 <- df$x1*df$x2

logit_model_nl <- glm(y~., data=select(df, y, x1_squared, x2_squared, x1_x2), family="binomial")
summary(logit_model_nl)

############
## Part F ##
############
df$logit_nl_p <- predict(logit_model_nl, df, type="response")
df$logit_nl_p_class <- 0
df$logit_nl_p_class[df$logit_nl_p>=0.5] <- 1
df$logit_nl_p_class <- as.factor(df$logit_nl_p_class)
ggplot(df, aes(x=x1, y=x2, color=logit_nl_p_class)) + geom_point()

############
## Part G ##
############
library(e1071)
svc_linear <- svm(y~., data=select(df, y, x1, x2), kernel="linear", cost=10)
df$svc_linear_p <- predict(svc_linear, select(df, y, x1, x2))
df$svc_linear_p <- as.factor(df$svc_linear_p)
ggplot(df, aes(x=x1, y=x2, color=svc_linear_p)) + geom_point()

############
## Part H ##
############
set.seed(1)
svm_radial <- svm(y~., data=select(df, y, x1, x2), kernel="radial", gamma=1, cost=10)
df$svm_radial_p <- predict(svm_radial, select(df, y, x1, x2))
df$svm_radial_p <- as.factor(df$svm_radial_p)
ggplot(df, aes(x=x1, y=x2, color=svm_radial_p)) + geom_point()







