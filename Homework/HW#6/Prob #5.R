rm(list=ls())

################
## Problem #5 ##
################
library(dplyr)
set.seed(123456)

############
## Part A ##
############
n=100
Y = rnorm(n, 10, 1)
X1 = Y + rnorm(n, 0, 0.2)
X2 = Y^2 + rnorm(n, 0, 1.1)
df <- data.frame(Y, X1, X2)
summary(df)

summary(lm(Y ~ ., data=df))


###########
## model ##
###########
predictor_list <- colnames(df)[2:length(colnames(df))]

for(j in 1:length(predictor_list)){
  
  
  df_sub <- select(df, -c(Y, predictor_list[j]))
  Y_sub <- df$Y - select(df, predictor_list[j])
  colnames(Y_sub) <- c("Y_sub")
  df_sub$Y_sub <- as.matrix(Y_sub)
  
  lin_reg_sub <- lm(Y_sub~., data=df_sub)
}







