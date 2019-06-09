rm(list=ls())

################
## Problem #4 ##
################

############
## Part A ##
############
library(MASS)
data(Boston)
set.seed(1)
fit.03 <- lm(nox~poly(dis,3), data=Boston)
dislims <- range(Boston$dis)
dis.grid <- seq(dislims[1], dislims[2], 0.1)
preds <- predict(fit.03, newdata=list(dis=dis.grid), se=TRUE)
se.bands <- preds$fit + cbind(2*preds$se.fit, -2*preds$se.fit)
par(mfrow=c(1,1), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0))
plot(Boston$dis, Boston$nox, xlim=dislims, cex=0.5, col="darkgrey")
title("Degree 3 Polynomial Fit")
lines(dis.grid, preds$fit, lwd=2, col="blue")
matlines(dis.grid, se.bands, lwd=1, col="blue", lty=3)
summary(fit.03)

############
## Part B ##
############
rss.error <- rep(0,10)
for (i in 1:10) {
  lm.fit <- lm(nox~poly(dis,i), data=Boston)
  rss.error[i] <- sum(lm.fit$residuals^2)
}
rss.error

plot(rss.error, type="b") 

############
## Part C ##
############
require(boot)
set.seed(1)
cv.error <- rep(0,10)
for (i in 1:10) {
  glm.fit <- glm(nox~poly(dis,i), data=Boston)
  cv.error[i] <- cv.glm(Boston, glm.fit, K=10)$delta[1]  # [1]:std, [2]:bias-corrected
}
cv.error
plot(cv.error, type="b")

############
## Part D ##
############
require(splines)
fit.sp <- lm(nox~bs(dis, df=7), data=Boston)
pred <- predict(fit.sp, newdata=list(dis=dis.grid), se=T)
plot(Boston$dis, Boston$nox, col="gray")
lines(dis.grid, pred$fit, lwd=2)
lines(dis.grid, pred$fit+2*pred$se, lty="dashed")
lines(dis.grid, pred$fit-2*pred$se, lty="dashed")

# set df to select knots at uniform quantiles of `dis`
attr(bs(Boston$dis,df=7),"knots")  

############
## Part E ##
############
require(splines)
set.seed(1)
rss.error <- rep(0,7)
for (i in 4:10) {
  fit.sp <- lm(nox~bs(dis, df=i), data=Boston)
  rss.error[i-3] <- sum(fit.sp$residuals^2)
}
rss.error
plot(4:10, rss.error, type="b")

############
## Part F ##
############
require(splines)
require(boot)
set.seed(1)
cv.error <- rep(0,7)
for (i in 4:10) {
  glm.fit <- glm(nox~bs(dis, df=i), data=Boston)
  cv.error[i-3] <- cv.glm(Boston, glm.fit, K=10)$delta[1]
}
cv.error
plot(4:10, cv.error, type="b") 


