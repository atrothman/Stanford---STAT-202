rm(list=ls())

################
## Problem #2 ##
################
library(dplyr)
library(ggplot2)
set.seed(1234)
id <- c(1:100)
df <- data.frame(id)
df$x1 <- rnorm(100)
df$error <- rnorm(100)
df$x2 <- 5*((df$x1)^2) + df$error
df$x2[df$id<=50] <- df$x2[df$id<=50]+5
df$x2[df$id>50] <- df$x2[df$id>50]-5
df$y <- 0
df$y[df$id>50] <- 1
df$y <- as.factor(df$y)

##########################
## Plot the data points ##
##########################
ggplot(df, aes(x=x1, y=x2, color=y)) + geom_point()

######################################
## create training and testing sets ##
######################################
train_df <- filter(df, (id>=1 & id<=25)|(id>50 & id<=75))
test_df <- filter(df, (id>25 & id<=50)|(id>75))

###############################
## Suuport Vector Classifier ##
###############################
library(e1071)
svc_linear <- svm(y~., data=select(train_df, y, x1, x2), kernel="linear", cost=10)

## training set
plot(svc_linear, select(train_df, y, x1, x2))
train_df$svc_linear_p <- predict(svc_linear, select(train_df, y, x1, x2))
table(train_df$y, train_df$svc_linear_p)

## testing set
plot(svc_linear, select(test_df, y, x1, x2))
test_df$svc_linear_p <- predict(svc_linear, select(test_df, y, x1, x2))
table(test_df$y, test_df$svc_linear_p)


###########################
## SVM polynomial kernel ##
###########################
set.seed(1)
svm_poly <- svm(y~., data=select(train_df, y, x1, x2), kernel="polynomial", cost=10)

## training set
plot(svm_poly, select(train_df, y, x1, x2))
train_df$svm_poly_p <- predict(svm_poly, select(train_df, y, x1, x2))
table(train_df$y, train_df$svm_poly_p)

## testing set
plot(svm_poly, select(test_df, y, x1, x2))
test_df$svm_poly_p <- predict(svm_poly, select(test_df, y, x1, x2))
table(test_df$y, test_df$svm_poly_p)


#######################
## SVM radial kernel ##
#######################
set.seed(1)
svm_radial <- svm(y~., data=select(train_df, y, x1, x2), kernel="radial", gamma=1, cost=10)

## training set
plot(svm_radial, select(train_df, y, x1, x2))
train_df$svm_radial_p <- predict(svm_radial, select(train_df, y, x1, x2))
table(train_df$y, train_df$svm_radial_p)

## testing set
plot(svm_radial, select(test_df, y, x1, x2))
test_df$svm_radial_p <- predict(svm_radial, select(test_df, y, x1, x2))
table(test_df$y, test_df$svm_radial_p)




