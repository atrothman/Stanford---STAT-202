rm(list=ls())

################
## Problem #4 ##
################
set.seed(1234)
library(dplyr)
library(ggfortify)
library(psych)

## Part A
## initialize array
data <- array(NA, dim=c(60,50))
column_labels <- array(NA, dim=c(50,1))

class_label <- array(NA, dim=c(60,1))
class_label[1:20,1] <- 1
class_label[21:40,1] <- 2
class_label[41:60,1] <- 3

for(i in 1:50){
  column_labels[i,1] <- paste("var_",i,sep='')
  mean <- runif(1,-1,1)
  sd = runif(1,0.5,2)
  
  data[1:20, i] <- rnorm(20, mean+2, sd)
  data[21:40, i] <- rnorm(20, mean-1, sd)
  data[41:60, i] <- rnorm(20, mean-3, sd)
}
colnames(data) <- column_labels
data_df <- data.frame(data)
data_df$class_label <- as.factor(class_label)
head(data_df)
describe(data_df)

## Part B
autoplot(prcomp(select(data_df, -class_label), scale=FALSE), scale=0, data=data_df, colour = 'class_label', loadings = TRUE, loadings.colour = 'black', loadings.label = TRUE, loadings.label.colour = 'black', loadings.label.size = 3)

## Part C
Kmeans_k3 <- kmeans(select(data_df, -class_label), 3, nstart=20)
data_df$k3 <- Kmeans_k3$cluster
table(data_df$k3, data_df$class_label)

## Part D
Kmeans_k2 <- kmeans(select(data_df, -class_label), 2, nstart=20)
data_df$k2 <- Kmeans_k2$cluster
table(data_df$k2, data_df$class_label)

## Part E
Kmeans_k4 <- kmeans(select(data_df, -class_label), 4, nstart=20)
data_df$k4 <- Kmeans_k4$cluster
table(data_df$k4, data_df$class_label)

## Part F
first_2_PCs <- prcomp(data)$rotation[,1:2]
Kmeans_k3_2PC <- kmeans((data %*% first_2_PCs), 3, nstart=20)
data_df$k3_2PC <- Kmeans_k3_2PC$cluster
table(data_df$k3_2PC, data_df$class_label)

## Part G
data_scaled <- data.frame(scale(data, center = TRUE, scale = TRUE))
Kmeans_k3_scaled <- kmeans(data_scaled, 3, nstart=20)
data_df$k3_scaled <- Kmeans_k3_scaled$cluster
table(data_df$k3_scaled, data_df$class_label)








