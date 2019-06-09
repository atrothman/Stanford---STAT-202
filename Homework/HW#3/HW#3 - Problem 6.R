rm(list=ls())

CV_results <- function(df, comb_matrix){
  
  num_variables <- rep(dim(comb_matrix)[1],dim(comb_matrix)[2])
  
  results_df <- data.frame(num_variables)
  rm(num_variables)
  results_df$var_string <- NA
  results_df$CV_logit_frac <- NA
  results_df$CV_LDA_frac <- NA
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
    
    ## create "sub" dataframe with the appropriate covariates and the outcome
    df_sub <- dplyr::select(df, outcome, cov_sub)
    
    ## create the for loop for 5-fold CV:
    
    logit_frac <- rep(NA, 5)
    LDA_frac <- rep(NA, 5)
    KNN_1_frac <- rep(NA, 5)
    KNN_5_frac <- rep(NA, 5)
    KNN_10_frac <- rep(NA, 5)
    KNN_20_frac <- rep(NA, 5)
    KNN_50_frac <- rep(NA, 5)
    
    for(j in 1:5){
      df_train <- dplyr::select(dplyr::filter(df, CV_id!=j), outcome, cov_sub)
      df_test <- dplyr::select(dplyr::filter(df, CV_id==j), outcome, cov_sub)
      
      ## fit the logistic model and output prediction
      df_test$pred_logit <- predict(glm(outcome ~ ., family=binomial, data=df_train), df_test, type="response")
      df_test$pred_logit_class <- 0
      df_test$pred_logit_class[df_test$pred_logit > 0.5] <- 1
      cm_logit <- table(df_test$pred_logit_class, df_test$outcome)
      logit_frac[j] <- (cm_logit[1,1]+cm_logit[2,2])/sum(cm_logit)
      rm(cm_logit)
      
      ## fit the LDA and output prediction
      df_test$pred_LDA <- predict(lda(outcome ~ ., data=df_train), df_test)$class
      cm_LDA <- table(df_test$pred_LDA, df_test$outcome)
      LDA_frac[j] <- (cm_LDA[1,1]+cm_LDA[2,2])/sum(cm_LDA)
      rm(cm_LDA)
      
      ## fit the KNN and output prediction
      Y_train <- as.matrix(df_train$outcome)
      X_train <- as.matrix(dplyr::select(df_train, cov_sub))    
      X_test <- as.matrix(dplyr::select(df_test, cov_sub))
      ## for k=1
      df_test$pred_KNN_1 <- knn(X_train, X_test, Y_train, k=1)
      cm_KNN_1 <- table(df_test$pred_KNN_1, df_test$outcome)
      KNN_1_frac[j] <- (cm_KNN_1[1,1]+cm_KNN_1[2,2])/sum(cm_KNN_1)
      rm(cm_KNN_1)
      ## for k=5
      df_test$pred_KNN_5 <- knn(X_train, X_test, Y_train, k=5)
      cm_KNN_5 <- table(df_test$pred_KNN_5, df_test$outcome)
      KNN_5_frac[j] <- (cm_KNN_5[1,1]+cm_KNN_5[2,2])/sum(cm_KNN_5)
      rm(cm_KNN_5)
      ## for k=10
      df_test$pred_KNN_10 <- knn(X_train, X_test, Y_train, k=10)
      cm_KNN_10 <- table(df_test$pred_KNN_10, df_test$outcome)
      KNN_10_frac[j] <- (cm_KNN_10[1,1]+cm_KNN_10[2,2])/sum(cm_KNN_10)
      rm(cm_KNN_10)
      ## for k=20
      df_test$pred_KNN_20 <- knn(X_train, X_test, Y_train, k=20)
      cm_KNN_20 <- table(df_test$pred_KNN_20, df_test$outcome)
      KNN_20_frac[j] <- (cm_KNN_20[1,1]+cm_KNN_20[2,2])/sum(cm_KNN_20)
      rm(cm_KNN_20)
      ## for k=50
      df_test$pred_KNN_50 <- knn(X_train, X_test, Y_train, k=50)
      cm_KNN_50 <- table(df_test$pred_KNN_50, df_test$outcome)
      KNN_50_frac[j] <- (cm_KNN_50[1,1]+cm_KNN_50[2,2])/sum(cm_KNN_50)
      rm(cm_KNN_50)
      rm(Y_train, X_train, X_test)
      
      rm(df_train, df_test)
    }
    
    results_df$CV_logit_frac[i] <- mean(logit_frac)
    results_df$CV_LDA_frac[i] <- mean(LDA_frac)
    results_df$CV_KNN_1_frac[i] <- mean(KNN_1_frac)
    results_df$CV_KNN_5_frac[i] <- mean(KNN_5_frac)
    results_df$CV_KNN_10_frac[i] <- mean(KNN_10_frac)
    results_df$CV_KNN_20_frac[i] <- mean(KNN_20_frac)
    results_df$CV_KNN_50_frac[i] <- mean(KNN_50_frac)
    
    rm(logit_frac, LDA_frac, KNN_1_frac, KNN_5_frac, KNN_10_frac, KNN_20_frac, KNN_50_frac)
  }
  
  return(results_df)
}

################
## Problem #6 ##
################
library(MASS)
library(dplyr)
library(psych)
set.seed(1234)
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
df$CV_id <- sample(1:5, dim(df)[1], replace=TRUE)
table(df$CV_id, exclude=NULL)

## specify the binary outcome
df$outcome <- 0
df$outcome[df$crim>median_cr] <- 1
table(df$outcome, exclude=NULL)

covariate_labels <- c("zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "black", "lstat", "medv")

################################################################
################################################################
## for the 13 possible covariates                             ##
## take all possible linear combinations of those covariates  ##  
## and perorm:                                                ## 
## Logistic Regression                                        ##
## LDA                                                        ##
## KNN with k=1,5,10,20,50                                    ##
## using 5-fold cross-validation                              ##
################################################################
################################################################

results_1 <- CV_results(df, combn(covariate_labels, 1))  ## 13 possible combiations
results_2 <- CV_results(df, combn(covariate_labels, 2))  ## 78 possible combiations
results_3 <- CV_results(df, combn(covariate_labels, 3))  ## 286 possible combiations
results_4 <- CV_results(df, combn(covariate_labels, 4))  ## 715 possible combiations
results_5 <- CV_results(df, combn(covariate_labels, 5))  ## 1287 possible combiations
results_6 <- CV_results(df, combn(covariate_labels, 6))  ## 1716 possible combiations
results_7 <- CV_results(df, combn(covariate_labels, 7))  ## 1716 possible combiations
results_8 <- CV_results(df, combn(covariate_labels, 8))  ## 1287 possible combiations
results_9 <- CV_results(df, combn(covariate_labels, 9))  ## 715 possible combiations
results_10 <- CV_results(df, combn(covariate_labels, 10))  ## 286 possible combiations
results_11 <- CV_results(df, combn(covariate_labels, 11))  ## 78 possible combiations
results_12 <- CV_results(df, combn(covariate_labels, 12))  ## 13 possible combiations
results_13 <- CV_results(df, combn(covariate_labels, 13))  ## 1 possible combiations

results_df <- rbind(results_1, results_2, results_3, results_4, results_5, results_6, results_7, results_8, results_9, results_10, results_11, results_12, results_13)

head(dplyr::arrange(results_df, desc(CV_logit_frac)))
head(dplyr::arrange(results_df, desc(CV_LDA_frac)))
head(dplyr::arrange(results_df, desc(CV_KNN_1_frac)))
head(dplyr::arrange(results_df, desc(CV_KNN_5_frac)))
head(dplyr::arrange(results_df, desc(CV_KNN_10_frac)))
head(dplyr::arrange(results_df, desc(CV_KNN_20_frac)))
head(dplyr::arrange(results_df, desc(CV_KNN_50_frac)))

results_aggregated <- results_df %>% group_by(num_variables) %>% summarise(max_logit = max(CV_logit_frac), max_LDA = max(CV_LDA_frac), max_KNN1 = max(CV_KNN_1_frac), max_KNN5 = max(CV_KNN_5_frac), max_KNN10 = max(CV_KNN_10_frac), max_KNN20 = max(CV_KNN_20_frac), max_KNN50 = max(CV_KNN_50_frac))
write.csv(results_aggregated, "D:\\classes\\Stanford\\STATS 202 (Fall 2018)\\Homework\\HW#3\\results_aggregated.csv")

















