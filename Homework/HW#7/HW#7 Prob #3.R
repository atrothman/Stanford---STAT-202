rm(list=ls())

################
## Problem #3 ##
################
library(randomForest)
library(MASS)
data(Boston)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
boston.test <- Boston [-train ,"medv"]
df <- matrix(NA, nrow=476, ncol=13)

for(num_var in 1:13){
  for(num_tree in 25:500){
    print(num_tree)
    rf <- randomForest(medv~., data=Boston, subset=train, mtry=num_var, ntree=num_tree)
    yhat <- predict(rf, newdata=Boston[-train,])
    df[num_tree-24, num_var] <- mean((yhat - boston.test)^2)
    rm(rf, yhat)
  }
}

df <- data.frame(df)
colnames(df) <- c("v1", "v2", "v3", "v4", "v5", "v6", "v7", "v8", "v9", "v10", "v11", "v12", "v13")
df$num_trees <- c(25:500)
summary(df)

plot(df$num_trees, df$v2, type='l', col="red", lwd=1, xlab="number of trees", ylab="Test Mean Squared Error", ylim=c(9,17))
lines(df$num_trees, df$v4, type='l', col="green", lwd=1)
lines(df$num_trees, df$v6, type='l', col="blue", lwd=1)
lines(df$num_trees, df$v8, type='l', col="orange", lwd=1)
lines(df$num_trees, df$v10, type='l', col="yellow", lwd=1)
lines(df$num_trees, df$v12, type='l', col="purple", lwd=1)
legend(x=450,y=11, c("mtry=2", "mtry=4", "mtry=6", "mtry=8", "mtry=10", "mtry=12"), lty=c(1,1,1,1,1,1), col=c("red","green","blue","orange","yellow","purple"))



plot(df$num_trees, df$v1, type='l', col="red", lwd=1, xlab="number of trees", ylab="Mean Squared Error", ylim=c(9,26))
lines(df$num_trees, df$v2, type='l', col="green", lwd=1)
lines(df$num_trees, df$v3, type='l', col="blue", lwd=1)
lines(df$num_trees, df$v4, type='l', col="orange", lwd=1)
lines(df$num_trees, df$v5, type='l', col="yellow", lwd=1)
lines(df$num_trees, df$v6, type='l', col="purple", lwd=1)
lines(df$num_trees, df$v7, type='l', col="brown", lwd=1)
legend(x=450,y=20.5, c("mtry=1", "mtry=2", "mtry=3", "mtry=4", "mtry=5", "mtry=6", "mtry=7"), lty=c(1,1,1,1,1,1,1), col=c("red","green","blue","orange","yellow","purple","brown"))


plot(df$num_trees, df$v8, type='l', col="darkgreen", lwd=1, xlab="number of trees", ylab="Mean Squared Error", ylim=c(9,17))
lines(df$num_trees, df$v9, type='l', col="darkred", lwd=1)
lines(df$num_trees, df$v10, type='l', col="deeppink", lwd=1)
lines(df$num_trees, df$v11, type='l', col="greenyellow", lwd=1)
lines(df$num_trees, df$v12, type='l', col="lightslateblue", lwd=1)
lines(df$num_trees, df$v13, type='l', col="paleturquoise4", lwd=1)
legend(x=450,y=17, c("mtry=8", "mtry=9", "mtry=10", "mtry=11", "mtry=12", "mtry=13"), lty=c(1,1,1,1,1,1), col=c("darkgreen","darkred","deeppink","greenyellow","lightslateblue","paleturquoise4"))



