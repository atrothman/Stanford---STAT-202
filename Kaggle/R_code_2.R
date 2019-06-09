rm(list=ls())

############
## Kaggle ##
############
library(dplyr)
library(glmnet)
library(randomForest)

#####################################
## load in traing and testing sets ##
#####################################
setwd("D:\\classes\\Stanford\\STATS 202 (Fall 2018)\\Kaggle\\")
df_train_id <- read.csv("train_data.csv")
df_train <- select(df_train_id, -Id)
df_train$Status <- as.factor(df_train$Status)
df_test <- read.csv("test_data.csv")
summary(df_train)
summary(df_test)

df_train$BP_2 <- (df_train$BP)^2
df_train$smoking_2 <- (df_train$smoking)^2
df_train$cholesterol_2 <- (df_train$cholesterol)^2
df_train$behavior_2 <- (df_train$behavior)^2
df_train$BMI_2 <- (df_train$BMI)^2
df_train$alcohol_2 <- (df_train$alcohol)^2
df_train$age_2 <- (df_train$age)^2
df_train$old_assay_2 <- (df_train$old_assay)^2
df_train$gold_standard_2 <- (df_train$gold_standard)^2
df_train$assay_2 <- (df_train$assay)^2

df_test$BP_2 <- (df_test$BP)^2
df_test$smoking_2 <- (df_test$smoking)^2
df_test$cholesterol_2 <- (df_test$cholesterol)^2
df_test$behavior_2 <- (df_test$behavior)^2
df_test$BMI_2 <- (df_test$BMI)^2
df_test$alcohol_2 <- (df_test$alcohol)^2
df_test$age_2 <- (df_test$age)^2
df_test$old_assay_2 <- (df_test$old_assay)^2
df_test$gold_standard_2 <- (df_test$gold_standard)^2
df_test$assay_2 <- (df_test$assay)^2

## BP
df_train$BP_smoking <- df_train$BP*df_train$smoking
df_train$BP_cholesterol <- df_train$BP*df_train$cholesterol
df_train$BP_behavior <- df_train$BP*df_train$behavior
df_train$BP_BMI <- df_train$BP*df_train$BMI
df_train$BP_alcohol <- df_train$BP*df_train$alcohol
df_train$BP_age <- df_train$BP*df_train$age
df_train$BP_old_assay <- df_train$BP*df_train$old_assay
df_train$BP_gold_standard <- df_train$BP*df_train$gold_standard
df_train$BP_assay <- df_train$BP*df_train$assay

## cholesterol
df_train$cholesterol_behavior <- df_train$cholesterol*df_train$behavior
df_train$cholesterol_BMI <- df_train$cholesterol*df_train$BMI
df_train$cholesterol_alcohol <- df_train$cholesterol*df_train$alcohol
df_train$cholesterol_age <- df_train$cholesterol*df_train$age
df_train$cholesterol_old_assay <- df_train$cholesterol*df_train$old_assay
df_train$cholesterol_gold_standard <- df_train$cholesterol*df_train$gold_standard
df_train$cholesterol_assay <- df_train$cholesterol*df_train$assay

## behavior
df_train$behavior_BMI <- df_train$behavior*df_train$BMI
df_train$behavior_alcohol <- df_train$behavior*df_train$alcohol
df_train$behavior_age <- df_train$behavior*df_train$age
df_train$behavior_old_assay <- df_train$behavior*df_train$old_assay
df_train$behavior_gold_standard <- df_train$behavior*df_train$gold_standard
df_train$behavior_assay <- df_train$behavior*df_train$assay

## BMI
df_train$BMI_alcohol <- df_train$BMI*df_train$alcohol
df_train$BMI_age <- df_train$BMI*df_train$age
df_train$BMI_old_assay <- df_train$BMI*df_train$old_assay
df_train$BMI_gold_standard <- df_train$BMI*df_train$gold_standard
df_train$BMI_assay <- df_train$BMI*df_train$assay

## alcohol
df_train$alcohol_age <- df_train$alcohol*df_train$age
df_train$alcohol_old_assay <- df_train$alcohol*df_train$old_assay
df_train$alcohol_gold_standard <- df_train$alcohol*df_train$gold_standard
df_train$alcohol_assay <- df_train$alcohol*df_train$assay

## age
df_train$age_old_assay <- df_train$age*df_train$old_assay
df_train$age_gold_standard <- df_train$age*df_train$gold_standard
df_train$age_assay <- df_train$age*df_train$assay

## old_assay
df_train$old_assay_gold_standard <- df_train$old_assay*df_train$gold_standard
df_train$old_assay_assay <- df_train$old_assay*df_train$assay

## gold_standard
df_train$gold_standard_assay <- df_train$gold_standard*df_train$assay



## BP
df_test$BP_smoking <- df_test$BP*df_test$smoking
df_test$BP_cholesterol <- df_test$BP*df_test$cholesterol
df_test$BP_behavior <- df_test$BP*df_test$behavior
df_test$BP_BMI <- df_test$BP*df_test$BMI
df_test$BP_alcohol <- df_test$BP*df_test$alcohol
df_test$BP_age <- df_test$BP*df_test$age
df_test$BP_old_assay <- df_test$BP*df_test$old_assay
df_test$BP_gold_standard <- df_test$BP*df_test$gold_standard
df_test$BP_assay <- df_test$BP*df_test$assay

## cholesterol
df_test$cholesterol_behavior <- df_test$cholesterol*df_test$behavior
df_test$cholesterol_BMI <- df_test$cholesterol*df_test$BMI
df_test$cholesterol_alcohol <- df_test$cholesterol*df_test$alcohol
df_test$cholesterol_age <- df_test$cholesterol*df_test$age
df_test$cholesterol_old_assay <- df_test$cholesterol*df_test$old_assay
df_test$cholesterol_gold_standard <- df_test$cholesterol*df_test$gold_standard
df_test$cholesterol_assay <- df_test$cholesterol*df_test$assay

## behavior
df_test$behavior_BMI <- df_test$behavior*df_test$BMI
df_test$behavior_alcohol <- df_test$behavior*df_test$alcohol
df_test$behavior_age <- df_test$behavior*df_test$age
df_test$behavior_old_assay <- df_test$behavior*df_test$old_assay
df_test$behavior_gold_standard <- df_test$behavior*df_test$gold_standard
df_test$behavior_assay <- df_test$behavior*df_test$assay

## BMI
df_test$BMI_alcohol <- df_test$BMI*df_test$alcohol
df_test$BMI_age <- df_test$BMI*df_test$age
df_test$BMI_old_assay <- df_test$BMI*df_test$old_assay
df_test$BMI_gold_standard <- df_test$BMI*df_test$gold_standard
df_test$BMI_assay <- df_test$BMI*df_test$assay

## alcohol
df_test$alcohol_age <- df_test$alcohol*df_test$age
df_test$alcohol_old_assay <- df_test$alcohol*df_test$old_assay
df_test$alcohol_gold_standard <- df_test$alcohol*df_test$gold_standard
df_test$alcohol_assay <- df_test$alcohol*df_test$assay

## age
df_test$age_old_assay <- df_test$age*df_test$old_assay
df_test$age_gold_standard <- df_test$age*df_test$gold_standard
df_test$age_assay <- df_test$age*df_test$assay

## old_assay
df_test$old_assay_gold_standard <- df_test$old_assay*df_test$gold_standard
df_test$old_assay_assay <- df_test$old_assay*df_test$assay

## gold_standard
df_test$gold_standard_assay <- df_test$gold_standard*df_test$assay


cutoff <- seq(0, 1, 0.01)
df_varrying_cutoff <- data.frame(cutoff)
df_varrying_cutoff$logistic <- NA
df_varrying_cutoff$LASSO <- NA
df_varrying_cutoff$Ridge <- NA
df_varrying_cutoff$EN <- NA
rm(cutoff)
count=1
for(j in seq(0, 1, 0.01)){
  print(j)
  ######################
  ## Cross Validation ##
  ######################
  k=10
  logit_CV_results <- rep(NA, k)
  LASSO_CV_results <- rep(NA, k)
  Ridge_CV_results <- rep(NA, k)
  EN_CV_results <- rep(NA, k)
  for(i in 1:k){
    CV_train <- select(filter(df_train, CV_Id!=i), -CV_Id) ## specify CV training set
    CV_test <- select(filter(df_train, CV_Id==i), -CV_Id)
    Y_train <- as.matrix(select(CV_train, Status))
    Y_test <- as.matrix(select(CV_test, Status))
    X_train <- as.matrix(select(CV_train, -Status))
    X_test <- as.matrix(select(CV_test, -Status))
    
    #########################
    ## logistic regression ##
    #########################
    CV_test$logit_num <- predict(glm(Status~., family="binomial", data=CV_train), CV_test, type="response")
    CV_test$logit_class <- 0
    CV_test$logit_class[CV_test$logit_num>=j] <- 1
    logit_CV_results[i] <- (dim(dplyr::filter(CV_test, logit_class==Status))[1])/(dim(CV_test)[1])
    
    ###########
    ## LASSO ##
    ###########
    LASSO_CV_tuned <- cv.glmnet(X_train, Y_train, alpha=1)
    lambda_LASSO <- LASSO_CV_tuned$lambda.min
    LASSO <- glmnet(X_train, Y_train, alpha=1, lambda=lambda_LASSO)
    CV_test$LASSO_num  <- as.numeric(predict(LASSO, s=lambda_LASSO, newx=X_test))
    CV_test$LASSO_class <- 0
    CV_test$LASSO_class[CV_test$LASSO_num>=j] <- 1
    LASSO_CV_results[i] <- (dim(dplyr::filter(CV_test, LASSO_class==Status))[1])/(dim(CV_test)[1])
    rm(LASSO_CV_tuned, lambda_LASSO, LASSO)
    
    ###########
    ## Ridge ##
    ###########
    Ridge_CV_tuned <- cv.glmnet(X_train, Y_train, alpha=0)
    lambda_Ridge <- Ridge_CV_tuned$lambda.min
    Ridge <- glmnet(X_train, Y_train, alpha=0, lambda=lambda_Ridge)
    CV_test$Ridge_num  <- as.numeric(predict(Ridge, s=lambda_Ridge, newx=X_test))
    CV_test$Ridge_class <- 0
    CV_test$Ridge_class[CV_test$Ridge_num>=j] <- 1
    Ridge_CV_results[i] <- (dim(dplyr::filter(CV_test, Ridge_class==Status))[1])/(dim(CV_test)[1])
    rm(Ridge_CV_tuned, lambda_Ridge, Ridge)
    
    #################
    ## Elastic Net ##
    #################
    EN_CV_tuned <- cv.glmnet(X_train, Y_train, alpha=0.5)
    lambda_EN <- EN_CV_tuned$lambda.min
    EN <- glmnet(X_train, Y_train, alpha=0.5, lambda=lambda_EN)
    CV_test$EN_num  <- as.numeric(predict(EN, s=lambda_EN, newx=X_test))
    CV_test$EN_class <- 0
    CV_test$EN_class[CV_test$EN_num>=j] <- 1
    EN_CV_results[i] <- (dim(dplyr::filter(CV_test, EN_class==Status))[1])/(dim(CV_test)[1])
    rm(EN_CV_tuned, lambda_EN, EN)
    
    rm(CV_train, CV_test, Y_train, Y_test, X_train, X_test)
    
    ###################
    ## Random Forest ##
    ###################
    rf <- randomForest(Status~., data=select(df_train, -CV_Id), mtry=25, ntree=1000)
    yhat_rf <- predict(rf, newdata=df_test)
    
    
    
  }
  
  df_varrying_cutoff$logistic[count] <- mean(logit_CV_results)
  df_varrying_cutoff$LASSO[count] <- mean(LASSO_CV_results)
  df_varrying_cutoff$Ridge[count] <- mean(Ridge_CV_results)
  df_varrying_cutoff$EN[count] <- mean(EN_CV_results)
  
  count=count+1
  
  rm(logit_CV_results, LASSO_CV_results, Ridge_CV_results, EN_CV_results)
}


#########################
## Logistic Regression ##
#########################
logit <- glm(Status~., family="binomial", data=select(df_train, -CV_Id))
summary(logit)
df_test$logit_num <- predict(logit, df_test, type="response")
df_test$logit_class <- FALSE
df_test$logit_class[df_test$logit_num>=0.5] <- TRUE
table(df_test$logit_class)

###########
## LASSO ##
###########
Y_train <- as.matrix(select(df_train, Status))
X_train <- as.matrix(select(df_train, -Status))
X_test <- as.matrix(select(df_test, -logit_num, -logit_class, -LASSO_class))

LASSO_CV_tuned <- cv.glmnet(X_train, Y_train, alpha=1)
lambda_LASSO <- LASSO_CV_tuned$lambda.min
LASSO <- glmnet(X_train, Y_train, alpha=1, lambda=lambda_LASSO)
df_test$LASSO_num  <- as.numeric(predict(LASSO, s=lambda_LASSO, newx=X_test))
df_test$LASSO_class <- FALSE
df_test$LASSO_class[df_test$LASSO_num>=0.46] <- TRUE


##########################
## create final dataset ##
##########################
final_df <- select(df_test, Id, LASSO_class)
colnames(final_df)[2] <- "Category"
head(final_df)

write.csv(final_df, "ATR_submission.csv", row.names=FALSE)









