rm(list=ls())

################
## Problem #2 ##
################
library(ggplot2)

X = seq(-2, 2, 0.1)
Y = rep(NA, length(X))

for(i in 1:length(X)){
  if(X[i]<1){
    Y[i] = 1+X[i]
  }
  if(X[i]>=1){
    Y[i] = 1+X[i]-(2*((X[i]-1)^2))
  }
}

df <- data.frame(Y,X)

## include connected line
ggplot(df, aes(x=X, y=Y)) + geom_line(color="red")



