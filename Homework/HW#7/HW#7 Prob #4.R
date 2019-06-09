rm(list=ls())

################
## Problem #4 ##
################
set.seed(1)
library(tree)
library(randomForest)
library(ISLR)
data(Carseats)
summary(Carseats)

############
## Part A ##
############
train_id <- sample(1:nrow(Carseats), nrow(Carseats)/2)
train_df <- Carseats[train_id,]
test_df <- Carseats[-train_id,]

############
## Part B ##
############
tree.carseats <- tree(Sales~., data=train_df)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0.95)

yhat <- predict(tree.carseats, newdata=test_df)
MSE <- mean((yhat - test_df$Sales)^2)

############
## Part C ##
############
cv.carseats <- cv.tree(tree.carseats)
cv.carseats
plot(cv.carseats$size, cv.carseats$dev, type="b")
#plot(cv.carseats$k, cv.carseats$dev, type="b")

prune.carseats <- prune.tree(tree.carseats, best=8)
plot(prune.carseats)
text(prune.carseats, pretty=0)

yhat_prune <- predict(prune.carseats, newdata=test_df)
MSE_prune <- mean((yhat_prune - test_df$Sales)^2)

############
## Part D ##
############
bag <- randomForest(Sales~., data=train_df, mtry=10, ntree=1000)
importance(bag)
yhat_bag <- predict(bag, newdata=test_df)
MSE_bag <- mean((yhat_bag - test_df$Sales)^2)

############
## Part E ##
############
MSE_vector <- rep(NA, 10)

for(i in 1:10){
  rf <- randomForest(Sales~., data=train_df, mtry=i, ntree=1000)
  
  if(i==8){
    print(importance(rf))  
  }
  yhat_rf <- predict(rf, newdata=test_df)
  MSE_vector[i] <- mean((yhat_rf - test_df$Sales)^2)
  rm(rf, yhat_rf)
}
MSE_vector
mtry <- c(1:10)

plot(x=mtry, y=MSE_vector, xlab="'mtry' parameter", ylab="test MSE")


