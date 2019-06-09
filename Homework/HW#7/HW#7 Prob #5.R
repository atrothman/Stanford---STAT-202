rm(list=ls())

################
## Problem #5 ##
################
set.seed(1)
library(tree)
library(randomForest)
library(ISLR)
data(OJ)
summary(OJ)
OJ$Purchase <- as.factor(OJ$Purchase)

############
## Part A ##
############
train_id <- sample(1:nrow(OJ), 800)
train_df <- OJ[train_id,]
test_df <- OJ[-train_id,]

############
## Part B ##
############
tree.OJ <- tree(Purchase~., data=train_df)
summary(tree.OJ)
yhat_train <- predict(tree.OJ, newdata=train_df, type="class")
table(yhat_train, train_df$Purchase)

############
## Part C ##
############
tree.OJ

############
## Part D ##
############
plot(tree.OJ)
text(tree.OJ, pretty=0)

############
## Part E ##
############
yhat_test <- predict(tree.OJ, newdata=test_df, type="class")
table(yhat_test, test_df$Purchase)

############
## Part F ##
############
cv.OJ <- cv.tree(tree.OJ, FUN=prune.misclass)
cv.OJ

############
## Part G ##
############
plot(cv.OJ$size, cv.OJ$dev, type="b")

############
## Part I ##
############
prune.OJ <- prune.tree(tree.OJ, best=2)
plot(prune.OJ)
text(prune.OJ, pretty=0)

############
## Part J ##
############
yhat_full_training <- predict(tree.OJ, newdata=train_df, type="class")
table(yhat_full_training, train_df$Purchase)

yhat_pruned_training <- predict(prune.OJ, newdata=train_df, type="class")
table(yhat_pruned_training, train_df$Purchase)

############
## Part K ##
############
yhat_full_test <- predict(tree.OJ, newdata=test_df, type="class")
table(yhat_full_test, test_df$Purchase)
yhat_pruned_test <- predict(prune.OJ, newdata=test_df, type="class")
table(yhat_pruned_test, test_df$Purchase)












