rm(list=ls())

################
## Problem #2 ##
################
library(ggplot2)
library(dplyr)
set.seed(10817645)

obs <- c(1:6)
X1 <- c(1,1,0,5,6,4)
X2 <- c(4,3,4,1,2,0)
df <- data.frame(obs, X1, X2)
rm(obs, X1, X2)

n <- dim(df)[1]
p <- (dim(df)[2])-1
K=2

## part A
ggplot(df, aes(x=X1, y=X2)) + geom_point(size=7)

## part B
df$cluster <- sample(1:K, n, replace=T)
df

## part C
cluster <- c(1:K)
df_cent <- data.frame(cluster)
rm(cluster)
df_cent$X1_cord <- NA
df_cent$X2_cord <- NA

for(i in 1:K){
  df_sub <- filter(df, cluster==i)
  df_cent$X1_cord[i] <- mean(df_sub$X1)
  df_cent$X2_cord[i] <- mean(df_sub$X2)
  rm(df_sub)
}
df_cent

## part D
df$cluster_update <- NA
for(i in 1:n){
  df_cent_sub <- df_cent
  df_cent_sub$distance <- NA
  
  for(j in 1:dim(df_cent_sub)[1]){
    df_cent_sub$distance[j] <- sqrt(((df$X1[i] - df_cent_sub$X1_cord[j])^2) + ((df$X2[i] - df_cent_sub$X2_cord[j])^2))
  }
  df$cluster_update[i] <- df_cent_sub$cluster[which.min(df_cent_sub$distance)[1]]
  rm(df_cent_sub)
}
df


## part E
set.seed(10817645)

cluster_update <- function(df, K, n){
  cluster <- c(1:K)
  df_cent <- data.frame(cluster)
  rm(cluster)
  df_cent$X1_cord <- NA
  df_cent$X2_cord <- NA
  for(i in 1:K){
    df_sub <- filter(df, cluster==i)
    df_cent$X1_cord[i] <- mean(df_sub$X1)
    df_cent$X2_cord[i] <- mean(df_sub$X2)
    rm(df_sub)
  }
  df$cluster_update <- NA
  for(i in 1:n){
    df_cent_sub <- df_cent
    df_cent_sub$distance <- NA
    for(j in 1:dim(df_cent_sub)[1]){
      df_cent_sub$distance[j] <- sqrt(((df$X1[i] - df_cent_sub$X1_cord[j])^2) + ((df$X2[i] - df_cent_sub$X2_cord[j])^2))
    }
    df$cluster_update[i] <- df_cent_sub$cluster[which.min(df_cent_sub$distance)[1]]
    rm(df_cent_sub)
  }
  return(df)
}

K_means <- function(df, K, n){
  df$cluster <- sample(1:K, n, replace=T)
  converged <- 0
  while(converged==0){
    df <- cluster_update(df, K, n)
    if(all(df$cluster==df$cluster_update)){
      converged <- 1
    }
    df$cluster <- df$cluster_update
    df <- dplyr::select(df, -cluster_update)
  }
  return(df)
}

obs <- c(1:6)
X1 <- c(1,1,0,5,6,4)
X2 <- c(4,3,4,1,2,0)
df <- data.frame(obs, X1, X2)
rm(obs, X1, X2)

n <- dim(df)[1]
p <- (dim(df)[2])-1
K=2

df <- K_means(df, K, n)
df

## Part F
df$cluster <- as.factor(df$cluster)
ggplot(df, aes(x=X1, y=X2, shape=cluster, color=cluster)) + geom_point(size=7)



