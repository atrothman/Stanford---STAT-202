rm(list=ls())


library(ISLR)
library(dplyr)
library(class)
data(Weekly)
df <- Weekly

df$Dir_num <- 0
df$Dir_num[df$Direction=='Up'] <- 1

cov_sub <- c("Lag1", "Lag2", "Lag3")


df_train <- dplyr::select(dplyr::filter(df, Year<=2008), Dir_num, cov_sub)
df_test <- dplyr::select(dplyr::filter(df, Year>2008), Dir_num, cov_sub)

## fit the KNN and output prediction
Y_train <- as.matrix(df_train$Dir_num)
X_train <- as.matrix(dplyr::select(df_train, cov_sub))    
X_test <- as.matrix(dplyr::select(df_test, cov_sub))
## for k=1
df_test$pred_KNN_10 <- knn(X_train, X_test, Y_train, k=2)
cm_KNN_10 <- table(df_test$pred_KNN_10, df_test$Dir_num)
cm_KNN_10
dim(dplyr::filter(df_test,Dir_num==pred_KNN_10))[1] / dim(df_test)[1]

