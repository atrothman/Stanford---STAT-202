rm(list=ls())

################
## Problem #5 ##
################

############
## Part A ##
############
set.seed(123456)
X1 <- rnorm(100, 0, 1)
X2 <- rnorm(100, 0, 1)
beta_0 <- -4.2
beta_1 <- 1.5
beta_2 <- -2.7
eps <- rnorm(100, 0, 1)
Y <- beta_0 + beta_1*X1 + beta_2*X2 + eps
plot(Y)

############
## Part B ##
############
bhat_1 <- 1

############
## Part C ##
############
a <- Y - bhat_1*X1
summary(lm(a~X2))
bhat_2 <- lm(a~X2)$coef[2]

############
## Part D ##
############
a <- Y - bhat_2*X2
summary(lm(a~X1))
bhat_1 <- lm(a~X1)$coef[2]

############
## Part E ##
############
b0_hat <- rep(NA, 1001)
b1_hat <- rep(NA, 1001)
b2_hat <- rep(NA, 1001)

b0_hat[1] <- 1
b1_hat[1] <- 1
b2_hat[1] <- 1

for(i in 2:1001){
  ##estimate b0
  a <- Y - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2)
  b0_hat[i] <- lm(a~1)$coef[1]
  rm(a)
  
  ##estimate b1
  a <- Y - b0_hat[i-1] - (b2_hat[i-1]*X2)
  b1_hat[i] <- lm(a~X1-1)$coef[1]
  
  ##estimate b2
  a <- Y - b0_hat[i-1] - (b1_hat[i-1]*X1)
  b2_hat[i] <- lm(a~X2-1)$coef[1]
}
b0_hat <- b0_hat[2:1001]
b1_hat <- b1_hat[2:1001]
b2_hat <- b2_hat[2:1001]

plot(b0_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-5,3))
lines(b1_hat, type='l', col="green", lwd=2)
lines(b2_hat, type='l', col="blue", lwd=2)
legend(x=600,y=0.5, c("b0_hat", "b1_hat", "b2_hat"), lty=c(1,1,1), col=c("red","green","blue"))

############
## Part F ##
############
summary(lm(Y~X1+X2))
lm_fit <- lm(Y~X1+X2)

plot(b0_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-5,3))
lines(b1_hat, type='l', col="green", lwd=2)
lines(b2_hat, type='l', col="blue", lwd=2)
abline(h=coef(lm_fit)[1], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[2], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[3], lty="dashed", lwd=3)
legend(x=500,y=0.5, c("b0_hat", "b1_hat", "b2_hat", "lm() estimates"), lty=c(1,1,1,2), col=c("red","green","blue", "black"))







