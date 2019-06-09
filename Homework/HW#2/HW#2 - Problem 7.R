rm(list=ls())

################
## Problem #7 ##
################

## Part A
set.seed(1)
x1 = runif(100)
x2 = 0.5*x1 + rnorm(100)/10
y = 2 + 2*x1 + 0.3*x2 + rnorm(100)

df <- data.frame(y, x1, x2)
head(df)

## Part B
cor(df$x1, df$x2)
plot(df$x1, df$x2, main="Scatterplot of x1 vs x2", xlab="x1", ylab="x2")

## Part C
summary(lm(y ~ x1 + x2, data=df))

## Part D
summary(lm(y ~ x1, data=df))

## Part E
summary(lm(y ~ x2, data=df))

## Part G
x1 <- c(x1, 0.1)
x2 <- c(x2, 0.8)
y <- c(y, 6)
df_add <- data.frame(y, x1, x2)

summary(lm(y ~ x1 + x2, data=df))
summary(lm(y ~ x1 + x2, data=df_add))
summary(lm(y ~ x1, data=df))
summary(lm(y ~ x1, data=df_add))
summary(lm(y ~ x2, data=df))
summary(lm(y ~ x2, data=df_add))


