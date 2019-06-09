rm(list=ls())

################
## Problem #4 ##
################
library(MASS)
library(dplyr)
library(psych)
set.seed(10815657)
df <- Boston

## explore the Boston dataset
dim(df)
head(df)
colnames(df)
?Boston
summary(df)

############
## Part A ##
############
mean(df$crim)

############
## Part B ##
############
se_estimate <- sd(df$crim) / sqrt(dim(df)[1])
se_estimate

############
## Part C ##
############
boot.part_c <- function(data, index){
  x <- data$crim[index]
  return(mean(x))
}

library(boot)
part_c_results <- boot(df, boot.part_c, 1000)
part_c_results

############
## Part D ##
############
t.test(df$crim)


############
## Part E ##
############
median(df$crim)

############
## Part F ##
############
boot.part_f <- function(data, index){
  x <- data$crim[index]
  return(median(x))
}

part_f_results <- boot(df, boot.part_f, 1000)
part_f_results


############
## Part G ##
############
quantile(df$crim, probs = 0.1)


############
## Part H ##
############
boot.part_h <- function(data, index){
  x <- data$crim[index]
  return(quantile(x, probs = 0.1))
}

part_h_results <- boot(df, boot.part_h, 1000)
part_h_results








