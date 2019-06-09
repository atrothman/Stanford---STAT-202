rm(list=ls())

################
## Problem #4 ##
################
library(ISLR)
data(Wage)

plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

require(gam)
gam.fit1 <- gam(wage~ns(age,5), data=Wage)
gam.fit2.1 <- gam(wage~ns(age,5)+maritl, data=Wage)
gam.fit2.2 <- gam(wage~ns(age,5)+jobclass, data=Wage)
gam.fit3 <- gam(wage~ns(age,5)+maritl+jobclass, data=Wage)
anova(gam.fit1, gam.fit2.1, gam.fit3)
anova(gam.fit1, gam.fit2.2, gam.fit3)

par(mfrow=c(1,3))
plot(gam.fit3, se=TRUE, col="blue")