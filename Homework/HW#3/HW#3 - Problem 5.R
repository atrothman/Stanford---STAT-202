rm(list=ls())

################
## Problem #5 ##
################
library(ISLR)
data(Weekly)
df <- Weekly
??Weekly

## Part A
library(psych)

dim(df)
head(df)
colnames(df)

summary(df)
describe(df)
table(df$Year, exclude=NULL)
table(df$Direction, exclude=NULL)

write.csv(describe(df), "D:\\classes\\Stanford\\STATS 202 (Fall 2018)\\Homework\\HW#3\\descrip_tableA.csv")

pairs(~Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume + Today, data=df, main="Simple Scatterplot Matrix")


## Part B
df$Dir_num <- 0
df$Dir_num[df$Direction=='Up'] <- 1
table(df$Direction, exclude=NULL)
table(df$Dir_num, exclude=NULL)
table(df$Direction, df$Dir_num, exclude=NULL)

summary(glm(Dir_num ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial, data=df))

## Part C
df$Dir_probability <- predict(glm(Dir_num ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, family=binomial, data=df), type="response")
df$Dir_pred_class <- 0
df$Dir_pred_class[df$Dir_probability > 0.5] <- 1

table(df$Dir_pred_class, df$Direction)


## Part D
library(dplyr)

head(df)
df_train <- dplyr::filter(dplyr::select(df, -Dir_probability, -Dir_pred_class), Year<=2008)
df_test <- dplyr::filter(dplyr::select(df, -Dir_probability, -Dir_pred_class), Year>2008)

df_test$logistic_p <- predict(glm(Dir_num ~ Lag2, family=binomial, data=df_train), df_test, type="response")
df_test$logistic_class <- 0
df_test$logistic_class[df_test$logistic_p > 0.5] <- 1

table(df_test$logistic_class, df_test$Direction)

## Part E
library(MASS)
df_test$lda_class <- predict(lda(Dir_num ~ Lag2, data=df_train), df_test)$class
table(df_test$lda_class, df_test$Direction)

## Part F
df_test$qda_class <- predict(qda(Dir_num ~ Lag2, data=df_train), df_test)$class
table(df_test$qda_class, df_test$Direction)


## Part G
library(class)
set.seed(1234)
Y_train <- as.matrix(df_train$Dir_num)
X_train <- as.matrix(df_train$Lag2)
X_test <- as.matrix(df_test$Lag2)

df_test$KNN_class <- knn(X_train, X_test, Y_train, k=1)
table(df_test$KNN_class, df_test$Direction)




## Part I
CV_results <- function(df, comb_matrix){
  
  num_variables <- rep(dim(comb_matrix)[1],dim(comb_matrix)[2])
  
  results_df <- data.frame(num_variables)
  rm(num_variables)
  results_df$var_string <- NA
  results_df$CV_logit_frac <- NA
  results_df$CV_LDA_frac <- NA
  results_df$CV_QDA_frac <- NA
  results_df$CV_KNN_1_frac <- NA
  results_df$CV_KNN_5_frac <- NA
  results_df$CV_KNN_10_frac <- NA
  results_df$CV_KNN_20_frac <- NA
  results_df$CV_KNN_50_frac <- NA
  
  for(i in 1:(dim(comb_matrix)[2])){
    print(i)
    
    ## take the "ith" column of covariates from the "covariate matrix"
    cov_sub <- comb_matrix[,i]
    results_df$var_string[i] <- paste(cov_sub, collapse=" + ")
    
    ## create training and testing sets:
    df_train <- dplyr::select(dplyr::filter(df, Year<=2008), Dir_num, cov_sub)
    df_test <- dplyr::select(dplyr::filter(df, Year>2008), Dir_num, cov_sub)
    
    ## Fit the logistic regression:
    df_test$pred_logit <- predict(glm(Dir_num ~ ., family=binomial, data=df_train), df_test, type="response")
    df_test$pred_logit_class <- 0
    df_test$pred_logit_class[df_test$pred_logit > 0.5] <- 1
    cm_logit <- table(df_test$pred_logit_class, df_test$Dir_num)
    results_df$CV_logit_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_logit_class))[1] / dim(df_test)[1]
    rm(cm_logit)
    
    ## Fit the LDA:
    df_test$pred_LDA <- predict(lda(Dir_num ~ ., data=df_train), df_test)$class
    cm_LDA <- table(df_test$pred_LDA, df_test$Dir_num)
    #results_df$CV_LDA_frac[i] <- (cm_LDA[1,1]+cm_LDA[2,2])/sum(cm_LDA)
    results_df$CV_LDA_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_LDA))[1] / dim(df_test)[1]
    rm(cm_LDA)
    
    ## Fit the QDA:
    df_test$pred_QDA <- predict(qda(Dir_num ~ ., data=df_train), df_test)$class
    cm_QDA <- table(df_test$pred_QDA, df_test$Dir_num)
    #results_df$CV_QDA_frac[i] <- (cm_QDA[1,1]+cm_QDA[2,2])/sum(cm_QDA)
    results_df$CV_QDA_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_QDA))[1] / dim(df_test)[1]
    rm(cm_QDA)
    
    ## fit the KNN and output prediction
    Y_train <- as.matrix(df_train$Dir_num)
    X_train <- as.matrix(dplyr::select(df_train, cov_sub))    
    X_test <- as.matrix(dplyr::select(df_test, cov_sub))
    ## for k=1
    df_test$pred_KNN_1 <- knn(X_train, X_test, Y_train, k=1)
    cm_KNN_1 <- table(df_test$pred_KNN_1, df_test$Dir_num)
    results_df$CV_KNN_1_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_KNN_1))[1] / dim(df_test)[1]
    rm(cm_KNN_1)
    ## for k=5
    df_test$pred_KNN_5 <- knn(X_train, X_test, Y_train, k=5)
    cm_KNN_5 <- table(df_test$pred_KNN_5, df_test$Dir_num)
    results_df$CV_KNN_5_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_KNN_5))[1] / dim(df_test)[1]
    rm(cm_KNN_5)
    ## for k=10
    df_test$pred_KNN_10 <- knn(X_train, X_test, Y_train, k=10)
    cm_KNN_10 <- table(df_test$pred_KNN_10, df_test$Dir_num)
    results_df$CV_KNN_10_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_KNN_10))[1] / dim(df_test)[1]
    rm(cm_KNN_10)
    ## for k=20
    df_test$pred_KNN_20 <- knn(X_train, X_test, Y_train, k=20)
    cm_KNN_20 <- table(df_test$pred_KNN_20, df_test$Dir_num)
    results_df$CV_KNN_20_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_KNN_20))[1] / dim(df_test)[1]
    rm(cm_KNN_20)
    ## for k=50
    df_test$pred_KNN_50 <- knn(X_train, X_test, Y_train, k=50)
    cm_KNN_50 <- table(df_test$pred_KNN_50, df_test$Dir_num)
    results_df$CV_KNN_50_frac[i] <- dim(dplyr::filter(df_test,Dir_num==pred_KNN_50))[1] / dim(df_test)[1]
    rm(cm_KNN_50)
    
  }
  
  return(results_df)
}


df$Lag1_cubed <- df$Lag1^3
df$Lag2_cubed <- df$Lag2^3
df$Lag3_cubed <- df$Lag3^3
df$Lag4_cubed <- df$Lag4^3
df$Lag5_cubed <- df$Lag5^3
df$Volume_cubed <- df$Volume^3

df$Lag1_2 <- df$Lag1 * df$Lag2
df$Lag1_3 <- df$Lag1 * df$Lag3
df$Lag1_4 <- df$Lag1 * df$Lag4
df$Lag1_5 <- df$Lag1 * df$Lag5
df$Lag1_Volume <- df$Lag1 * df$Volume

df$Lag2_3 <- df$Lag2 * df$Lag3
df$Lag2_4 <- df$Lag2 * df$Lag4
df$Lag2_5 <- df$Lag2 * df$Lag5
df$Lag2_Volume <- df$Lag2 * df$Volume

df$Lag3_4 <- df$Lag3 * df$Lag4
df$Lag3_5 <- df$Lag3 * df$Lag5
df$Lag3_Volume <- df$Lag3 * df$Volume

df$Lag4_5 <- df$Lag4 * df$Lag5
df$Lag4_Volume <- df$Lag4 * df$Volume

df$Lag5_Volume <- df$Lag5 * df$Volume


covariate_labels <- c("Lag1", "Lag2", "Lag3", "Lag4", "Lag5", "Volume")
                      


results_1 <- CV_results(df, combn(covariate_labels, 1)) 
results_2 <- CV_results(df, combn(covariate_labels, 2))
results_3 <- CV_results(df, combn(covariate_labels, 3))
results_4 <- CV_results(df, combn(covariate_labels, 4))
results_5 <- CV_results(df, combn(covariate_labels, 5))
results_6 <- CV_results(df, combn(covariate_labels, 6))

results_df <- rbind(results_1, results_2, results_3, results_4, results_5, results_6)



covariate_labels_sub <- c("Lag1", "Lag2", "Lag3",
                      "Lag1_cubed", "Lag2_cubed", "Lag3_cubed",
                      "Lag1_2", "Lag1_3", "Lag2_3")

results_1_sub <- CV_results(df, combn(covariate_labels_sub, 1)) 
results_2_sub <- CV_results(df, combn(covariate_labels_sub, 2))
results_3_sub <- CV_results(df, combn(covariate_labels_sub, 3))
results_4_sub <- CV_results(df, combn(covariate_labels_sub, 4))
results_5_sub <- CV_results(df, combn(covariate_labels_sub, 5))
results_6_sub <- CV_results(df, combn(covariate_labels_sub, 6))
results_7_sub <- CV_results(df, combn(covariate_labels_sub, 7))
results_8_sub <- CV_results(df, combn(covariate_labels_sub, 8))
results_9_sub <- CV_results(df, combn(covariate_labels_sub, 9))


results_df_sub <- rbind(results_1_sub, results_2_sub, results_3_sub, results_4_sub, results_5_sub, results_6_sub, results_7_sub, results_8_sub, results_9_sub)



cov_sub <- c("Lag1", "Lag2_cubed", "Lag2_3")


df_train <- dplyr::select(dplyr::filter(df, Year<=2008), Dir_num, cov_sub)
df_test <- dplyr::select(dplyr::filter(df, Year>2008), Dir_num, cov_sub)

## fit the KNN and output prediction
Y_train <- as.matrix(df_train$Dir_num)
X_train <- as.matrix(dplyr::select(df_train, cov_sub))    
X_test <- as.matrix(dplyr::select(df_test, cov_sub))
## for k=1
df_test$pred_KNN_10 <- knn(X_train, X_test, Y_train, k=10)
cm_KNN_10 <- table(df_test$pred_KNN_10, df_test$Dir_num)
cm_KNN_10
dim(dplyr::filter(df_test,Dir_num==pred_KNN_10))[1] / dim(df_test)[1]
rm(cm_KNN_1)








"Lag1_cubed", "Lag2_cubed", "Lag3_cubed", "Lag4_cubed", "Lag5_cubed", "Volume_cubed",
"Lag1_2", "Lag1_3", "Lag1_4", "Lag1_5", "Lag1_Volume",
"Lag2_3", "Lag2_4", "Lag2_5", "Lag2_Volume",
"Lag3_4", "Lag3_5", "Lag3_Volume",
"Lag4_5", "Lag4_Volume", "Lag5_Volume")
