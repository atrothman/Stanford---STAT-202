rm(list=ls())

################
## Problem #5 ##
################
library(MASS)
library(dplyr)
library(psych)
set.seed(10815657)
df <- USArrests
head(df)

############
## Part 1 ##
############
pve_vector1 <- rep(NA, 1000)
pve_vector2 <- rep(NA, 1000)
pve_vector1_and_2 <- rep(NA, 1000)

for(i in 1:1000){
  df_matrix <- as.matrix(df)
  bs_matrix <- df_matrix[(sample(dim(df)[1], dim(df)[1], replace=T)),]
  
  PR <- prcomp(bs_matrix, scale=T)
  pve <- ((PR$sdev)^2) / sum((PR$sdev)^2)
  
  pve_vector1[i] <-  pve[1] 
  pve_vector2[i] <-  pve[2] 
  pve_vector1_and_2[i] <- pve[1] + pve[2] 
  
  rm(df_matrix, bs_matrix, PR, pve)
}

hist(pve_vector1, breaks=25)
hist(pve_vector2, breaks=25)
hist(pve_vector1_and_2, breaks=25)


############
## Part 2 ##
############
PR <- prcomp(df, scale=T)
pve <- ((PR$sdev)^2) / sum((PR$sdev)^2)

se_1 <- sd(pve_vector1)
se_2 <- sd(pve_vector2)
se_1_and_2 <- sd(pve_vector1_and_2)

## 95% CI for pve_vector1
pve[1]
pve[1] - (2*se_1)
pve[1] + (2*se_1)


## 95% CI for pve_vector2
pve[2]
pve[2] - (2*se_2)
pve[2] + (2*se_2)

## 95% CI for pve_vector1_and_2
pve[1] + pve[2]
(pve[1]+pve[2]) - (2*se_1_and_2)
(pve[1]+pve[2]) + (2*se_1_and_2)



############
## Part 4 ##
############
part_4 <- function(df){
  PC1_matrix <- matrix(NA, dim(df)[2], 1000)
  
  for(i in 1:1000){
    df_matrix <- as.matrix(df)
    bs_matrix <- df_matrix[(sample(dim(df)[1], dim(df)[1], replace=T)),]
    PR <- prcomp(bs_matrix, scale=T)
    PC1 <- PR$rotation[,1]
    largest_value <- max(PC1)
    if(largest_value < 0){
      PC1 <- PC1*-1
    }
    PC1_matrix[,i] <- PC1
    
    rm(df_matrix, bs_matrix, PR, PC1, largest_value)
  }
  return(PC1_matrix)
}

PC1_matrix <- part_4(df)


############
## Part 5 ##
############
sd(PC1_matrix[1,])
sd(PC1_matrix[2,])
sd(PC1_matrix[3,])
sd(PC1_matrix[4,])

PR <- prcomp(df, scale=T)
PR$rotation






