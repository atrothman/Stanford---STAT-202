rm(list=ls())

################
## Problem #6 ##
################
library(dplyr)

set.seed(123456)
X0 <- rep(1, 100)
X1 <-  rnorm(100, 0, 1)
X2 <- rnorm(100, 0, 1)
X3 <- rnorm(100, 0, 1)
X4 <- rnorm(100, 0, 1)
X5 <- rnorm(100, 0, 1)
X6 <- rnorm(100, 0, 1)
X7 <- rnorm(100, 0, 1)
X8 <- rnorm(100, 0, 1)
X9 <- rnorm(100, 0, 1)
X10 <- rnorm(100, 0, 1)
X11 <- rnorm(100, 0, 1)
X12 <- rnorm(100, 0, 1)
X13 <- rnorm(100, 0, 1)
X14 <- rnorm(100, 0, 1)
X15 <- rnorm(100, 0, 1)
X16 <- rnorm(100, 0, 1)
X17 <- rnorm(100, 0, 1)
X18 <- rnorm(100, 0, 1)
X19 <- rnorm(100, 0, 1)
X20 <- rnorm(100, 0, 1)
X21 <- rnorm(100, 0, 1)
X22 <- rnorm(100, 0, 1)
X23 <- rnorm(100, 0, 1)
X24 <- rnorm(100, 0, 1)
X25 <- rnorm(100, 0, 1)
X26 <- rnorm(100, 0, 1)
X27 <- rnorm(100, 0, 1)
X28 <- rnorm(100, 0, 1)
X29 <- rnorm(100, 0, 1)
X30 <- rnorm(100, 0, 1)
X31 <- rnorm(100, 0, 1)
X32 <- rnorm(100, 0, 1)
X33 <- rnorm(100, 0, 1)
X34 <- rnorm(100, 0, 1)
X35 <- rnorm(100, 0, 1)
X36 <- rnorm(100, 0, 1)
X37 <- rnorm(100, 0, 1)
X38 <- rnorm(100, 0, 1)
X39 <- rnorm(100, 0, 1)
X40 <- rnorm(100, 0, 1)
X41 <- rnorm(100, 0, 1)
X42 <- rnorm(100, 0, 1)
X43 <- rnorm(100, 0, 1)
X44 <- rnorm(100, 0, 1)
X45 <- rnorm(100, 0, 1)
X46 <- rnorm(100, 0, 1)
X47 <- rnorm(100, 0, 1)
X48 <- rnorm(100, 0, 1)
X49 <- rnorm(100, 0, 1)
X50 <- rnorm(100, 0, 1)
eps <- rnorm(100, 0, 1)

df <- data.frame(X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, 
                 X10, X11, X12, X13, X14, X15, X16, X17, X18, X19, 
                 X20, X21, X22, X23, X24, X25, X26, X27, X28, X29, 
                 X30, X31, X32, X33, X34, X35, X36, X37, X38, X39, 
                 X40, X41, X42, X43, X44, X45, X46, X47, X48, X49, X50)

Betas <- runif(51, min=-4, max=4)
Y <- as.matrix(df)%*%Betas + eps

b0_hat <- rep(NA, 1001)
b1_hat <- rep(NA, 1001)
b2_hat <- rep(NA, 1001)
b3_hat <- rep(NA, 1001)
b4_hat <- rep(NA, 1001)
b5_hat <- rep(NA, 1001)
b6_hat <- rep(NA, 1001)
b7_hat <- rep(NA, 1001)
b8_hat <- rep(NA, 1001)
b9_hat <- rep(NA, 1001)
b10_hat <- rep(NA, 1001)
b11_hat <- rep(NA, 1001)
b12_hat <- rep(NA, 1001)
b13_hat <- rep(NA, 1001)
b14_hat <- rep(NA, 1001)
b15_hat <- rep(NA, 1001)
b16_hat <- rep(NA, 1001)
b17_hat <- rep(NA, 1001)
b18_hat <- rep(NA, 1001)
b19_hat <- rep(NA, 1001)
b20_hat <- rep(NA, 1001)
b21_hat <- rep(NA, 1001)
b22_hat <- rep(NA, 1001)
b23_hat <- rep(NA, 1001)
b24_hat <- rep(NA, 1001)
b25_hat <- rep(NA, 1001)
b26_hat <- rep(NA, 1001)
b27_hat <- rep(NA, 1001)
b28_hat <- rep(NA, 1001)
b29_hat <- rep(NA, 1001)
b30_hat <- rep(NA, 1001)
b31_hat <- rep(NA, 1001)
b32_hat <- rep(NA, 1001)
b33_hat <- rep(NA, 1001)
b34_hat <- rep(NA, 1001)
b35_hat <- rep(NA, 1001)
b36_hat <- rep(NA, 1001)
b37_hat <- rep(NA, 1001)
b38_hat <- rep(NA, 1001)
b39_hat <- rep(NA, 1001)
b40_hat <- rep(NA, 1001)
b41_hat <- rep(NA, 1001)
b42_hat <- rep(NA, 1001)
b43_hat <- rep(NA, 1001)
b44_hat <- rep(NA, 1001)
b45_hat <- rep(NA, 1001)
b46_hat <- rep(NA, 1001)
b47_hat <- rep(NA, 1001)
b48_hat <- rep(NA, 1001)
b49_hat <- rep(NA, 1001)
b50_hat <- rep(NA, 1001)

b0_hat[1] <- b1_hat[1] <- b2_hat[1] <- b3_hat[1] <- b4_hat[1] <- b5_hat[1] <- b6_hat[1] <- b7_hat[1] <- b8_hat[1] <- b9_hat[1] <- 1
b10_hat[1] <- b11_hat[1] <- b12_hat[1] <- b13_hat[1] <- b14_hat[1] <- b15_hat[1] <- b16_hat[1] <- b17_hat[1] <- b18_hat[1] <- b19_hat[1] <- 1
b20_hat[1] <- b21_hat[1] <- b22_hat[1] <- b23_hat[1] <- b24_hat[1] <- b25_hat[1] <- b26_hat[1] <- b27_hat[1] <- b28_hat[1] <- b29_hat[1] <- 1
b30_hat[1] <- b31_hat[1] <- b32_hat[1] <- b33_hat[1] <- b34_hat[1] <- b35_hat[1] <- b36_hat[1] <- b37_hat[1] <- b38_hat[1] <- b39_hat[1] <- 1
b40_hat[1] <- b41_hat[1] <- b42_hat[1] <- b43_hat[1] <- b44_hat[1] <- b45_hat[1] <- b46_hat[1] <- b47_hat[1] <- b48_hat[1] <- b49_hat[1] <- b50_hat[1] <- 1

for(i in 2:1001){
  print(i)
  
  ##estimate b0
  a <- Y - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b0_hat[i] <- lm(a~1)$coef[1]
  rm(a)
  
  ##estimate b1
  a <- Y - b0_hat[i] - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b1_hat[i] <- lm(a~X1-1)$coef[1]
  rm(a)
  
  ##estimate b2
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b2_hat[i] <- lm(a~X2-1)$coef[1]
  rm(a)
  
  ##estimate b3
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b3_hat[i] <- lm(a~X3-1)$coef[1]
  rm(a)
  
  ##estimate b4
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b4_hat[i] <- lm(a~X4-1)$coef[1]
  rm(a)
  
  ##estimate b5
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b5_hat[i] <- lm(a~X5-1)$coef[1]
  rm(a)
  
  ##estimate b6
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b6_hat[i] <- lm(a~X6-1)$coef[1]
  rm(a)
  
  ##estimate b7
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b7_hat[i] <- lm(a~X7-1)$coef[1]
  rm(a)
  
  ##estimate b8
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b8_hat[i] <- lm(a~X8-1)$coef[1]
  rm(a)
  
  ##estimate b9
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b9_hat[i] <- lm(a~X9-1)$coef[1]
  rm(a)
  
  ##estimate b10
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) 
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b10_hat[i] <- lm(a~X10-1)$coef[1]
  rm(a)
  
  ##estimate b11
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b11_hat[i] <- lm(a~X11-1)$coef[1]
  rm(a)
  
  ##estimate b12
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b12_hat[i] <- lm(a~X12-1)$coef[1]
  rm(a)
  
  ##estimate b13
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b13_hat[i] <- lm(a~X13-1)$coef[1]
  rm(a)
  
  ##estimate b14
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b14_hat[i] <- lm(a~X14-1)$coef[1]
  rm(a)
  
  ##estimate b15
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b15_hat[i] <- lm(a~X15-1)$coef[1]
  rm(a)
  
  ##estimate b16
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b16_hat[i] <- lm(a~X16-1)$coef[1]
  rm(a)
  
  ##estimate b17
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b17_hat[i] <- lm(a~X17-1)$coef[1]
  rm(a)
  
  ##estimate b18
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b18_hat[i] <- lm(a~X18-1)$coef[1]
  rm(a)
  
  ##estimate b19
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b19_hat[i] <- lm(a~X19-1)$coef[1]
  rm(a)
  
  ##estimate b20
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) 
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b20_hat[i] <- lm(a~X20-1)$coef[1]
  rm(a)
  
  ##estimate b21
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b21_hat[i] <- lm(a~X21-1)$coef[1]
  rm(a)
  
  ##estimate b22
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b22_hat[i] <- lm(a~X22-1)$coef[1]
  rm(a)
  
  ##estimate b23
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b23_hat[i] <- lm(a~X23-1)$coef[1]
  rm(a)
  
  ##estimate b24
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b24_hat[i] <- lm(a~X24-1)$coef[1]
  rm(a)
  
  ##estimate b25
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b25_hat[i] <- lm(a~X25-1)$coef[1]
  rm(a)
  
  ##estimate b26
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b26_hat[i] <- lm(a~X26-1)$coef[1]
  rm(a)
  
  ##estimate b27
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b27_hat[i] <- lm(a~X27-1)$coef[1]
  rm(a)
  
  ##estimate b28
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b28_hat[i] <- lm(a~X28-1)$coef[1]
  rm(a)
  
  ##estimate b29
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b29_hat[i] <- lm(a~X29-1)$coef[1]
  rm(a)
  
  ##estimate b30
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) 
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b30_hat[i] <- lm(a~X30-1)$coef[1]
  rm(a)
  
  ##estimate b31
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b31_hat[i] <- lm(a~X31-1)$coef[1]
  rm(a)
  
  ##estimate b32
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b32_hat[i] <- lm(a~X32-1)$coef[1]
  rm(a)
  
  ##estimate b33
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b33_hat[i] <- lm(a~X33-1)$coef[1]
  rm(a)
  
  ##estimate b34
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b34_hat[i] <- lm(a~X34-1)$coef[1]
  rm(a)
  
  ##estimate b35
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b35_hat[i] <- lm(a~X35-1)$coef[1]
  rm(a)
  
  ##estimate b36
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b36_hat[i] <- lm(a~X36-1)$coef[1]
  rm(a)
  
  ##estimate b37
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b37_hat[i] <- lm(a~X37-1)$coef[1]
  rm(a)
  
  ##estimate b38
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b38_hat[i] <- lm(a~X38-1)$coef[1]
  rm(a)
  
  ##estimate b39
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b39_hat[i] <- lm(a~X39-1)$coef[1]
  rm(a)
  
  ##estimate b40
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) 
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b40_hat[i] <- lm(a~X40-1)$coef[1]
  rm(a)
  
  ##estimate b41
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b41_hat[i] <- lm(a~X41-1)$coef[1]
  rm(a)
  
  ##estimate b42
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b42_hat[i] <- lm(a~X42-1)$coef[1]
  rm(a)
  
  ##estimate b43
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b43_hat[i] <- lm(a~X43-1)$coef[1]
  rm(a)
  
  ##estimate b44
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b44_hat[i] <- lm(a~X44-1)$coef[1]
  rm(a)
  
  ##estimate b45
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b45_hat[i] <- lm(a~X45-1)$coef[1]
  rm(a)
  
  ##estimate b46
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b46_hat[i] <- lm(a~X46-1)$coef[1]
  rm(a)
  
  ##estimate b47
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b47_hat[i] <- lm(a~X47-1)$coef[1]
  rm(a)
  
  ##estimate b48
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b49_hat[i-1]*X49) - (b50_hat[i-1]*X50)
  b48_hat[i] <- lm(a~X48-1)$coef[1]
  rm(a)
  
  ##estimate b49
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b50_hat[i-1]*X50)
  b49_hat[i] <- lm(a~X49-1)$coef[1]
  rm(a)
  
  ##estimate b50
  a <- Y - b0_hat[i] - (b1_hat[i-1]*X1) - (b2_hat[i-1]*X2) - (b3_hat[i-1]*X3) - (b4_hat[i-1]*X4) - (b5_hat[i-1]*X5) - (b6_hat[i-1]*X6) - (b7_hat[i-1]*X7) - (b8_hat[i-1]*X8) - (b9_hat[i-1]*X9) - (b10_hat[i-1]*X10)
  - (b11_hat[i-1]*X11) - (b12_hat[i-1]*X12) - (b13_hat[i-1]*X13) - (b14_hat[i-1]*X14) - (b15_hat[i-1]*X15) - (b16_hat[i-1]*X16) - (b17_hat[i-1]*X17) - (b18_hat[i-1]*X18) - (b19_hat[i-1]*X19) - (b20_hat[i-1]*X20)
  - (b21_hat[i-1]*X21) - (b22_hat[i-1]*X22) - (b23_hat[i-1]*X23) - (b24_hat[i-1]*X24) - (b25_hat[i-1]*X25) - (b26_hat[i-1]*X26) - (b27_hat[i-1]*X27) - (b28_hat[i-1]*X28) - (b29_hat[i-1]*X29) - (b30_hat[i-1]*X30)
  - (b31_hat[i-1]*X31) - (b32_hat[i-1]*X32) - (b33_hat[i-1]*X33) - (b34_hat[i-1]*X34) - (b35_hat[i-1]*X35) - (b36_hat[i-1]*X36) - (b37_hat[i-1]*X37) - (b38_hat[i-1]*X38) - (b39_hat[i-1]*X39) - (b40_hat[i-1]*X40)
  - (b41_hat[i-1]*X41) - (b42_hat[i-1]*X42) - (b43_hat[i-1]*X43) - (b44_hat[i-1]*X44) - (b45_hat[i-1]*X45) - (b46_hat[i-1]*X46) - (b47_hat[i-1]*X47) - (b48_hat[i-1]*X48) - (b49_hat[i-1]*X49) 
  b50_hat[i] <- lm(a~X50-1)$coef[1]
  rm(a)
  
}
b0_hat <- b0_hat[2:1001]
b1_hat <- b1_hat[2:1001]
b2_hat <- b2_hat[2:1001]
b3_hat <- b3_hat[2:1001]
b4_hat <- b4_hat[2:1001]
b5_hat <- b5_hat[2:1001]
b6_hat <- b6_hat[2:1001]
b7_hat <- b7_hat[2:1001]
b8_hat <- b8_hat[2:1001]
b9_hat <- b9_hat[2:1001]
b10_hat <- b10_hat[2:1001]
b11_hat <- b11_hat[2:1001]
b12_hat <- b12_hat[2:1001]
b13_hat <- b13_hat[2:1001]
b14_hat <- b14_hat[2:1001]
b15_hat <- b15_hat[2:1001]
b16_hat <- b16_hat[2:1001]
b17_hat <- b17_hat[2:1001]
b18_hat <- b18_hat[2:1001]
b19_hat <- b19_hat[2:1001]
b20_hat <- b20_hat[2:1001]
b21_hat <- b21_hat[2:1001]
b22_hat <- b22_hat[2:1001]
b23_hat <- b23_hat[2:1001]
b24_hat <- b24_hat[2:1001]
b25_hat <- b25_hat[2:1001]
b26_hat <- b26_hat[2:1001]
b27_hat <- b27_hat[2:1001]
b28_hat <- b28_hat[2:1001]
b29_hat <- b29_hat[2:1001]
b30_hat <- b30_hat[2:1001]
b31_hat <- b31_hat[2:1001]
b32_hat <- b32_hat[2:1001]
b33_hat <- b33_hat[2:1001]
b34_hat <- b34_hat[2:1001]
b35_hat <- b35_hat[2:1001]
b36_hat <- b36_hat[2:1001]
b37_hat <- b37_hat[2:1001]
b38_hat <- b38_hat[2:1001]
b39_hat <- b39_hat[2:1001]
b40_hat <- b40_hat[2:1001]
b41_hat <- b41_hat[2:1001]
b42_hat <- b42_hat[2:1001]
b43_hat <- b43_hat[2:1001]
b44_hat <- b44_hat[2:1001]
b45_hat <- b45_hat[2:1001]
b46_hat <- b46_hat[2:1001]
b47_hat <- b47_hat[2:1001]
b48_hat <- b48_hat[2:1001]
b49_hat <- b49_hat[2:1001]
b50_hat <- b50_hat[2:1001]

df_new <- select(df, -X0)
df_new$Y <- Y
lm_fit <- lm(Y~., data=df_new)

plot(b0_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b1_hat, type='l', col="green", lwd=2)
lines(b2_hat, type='l', col="blue", lwd=2)
lines(b3_hat, type='l', col="orange", lwd=2)
lines(b4_hat, type='l', col="yellow", lwd=2)
lines(b5_hat, type='l', col="purple", lwd=2)
lines(b6_hat, type='l', col="brown", lwd=2)
lines(b7_hat, type='l', col="darkgreen", lwd=2)
lines(b8_hat, type='l', col="darkred", lwd=2)
lines(b9_hat, type='l', col="deeppink", lwd=2)
lines(b10_hat, type='l', col="greenyellow", lwd=2)
abline(h=coef(lm_fit)[1], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[2], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[3], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[4], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[5], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[6], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[7], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[8], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[9], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[10], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[11], lty="dashed", lwd=3)
legend(x=600,y=5, c("b0_hat", "b1_hat", "b2_hat", "b3_hat", "b4_hat", "b5_hat", "b6_hat", "b7_hat", "b8_hat", "b9_hat", "b10_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,1,2), col=c("red","green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))


plot(b11_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b12_hat, type='l', col="blue", lwd=2)
lines(b13_hat, type='l', col="orange", lwd=2)
lines(b14_hat, type='l', col="yellow", lwd=2)
lines(b15_hat, type='l', col="purple", lwd=2)
lines(b16_hat, type='l', col="brown", lwd=2)
lines(b17_hat, type='l', col="darkgreen", lwd=2)
lines(b18_hat, type='l', col="darkred", lwd=2)
lines(b19_hat, type='l', col="deeppink", lwd=2)
lines(b20_hat, type='l', col="greenyellow", lwd=2)
abline(h=coef(lm_fit)[12], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[13], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[14], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[15], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[16], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[17], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[18], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[19], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[20], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[21], lty="dashed", lwd=3)
legend(x=600,y=6, c("b11_hat", "b12_hat", "b13_hat", "b14_hat", "b15_hat", "b16_hat", "b17_hat", "b18_hat", "b19_hat", "b20_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))


plot(b21_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b22_hat, type='l', col="blue", lwd=2)
lines(b23_hat, type='l', col="orange", lwd=2)
lines(b24_hat, type='l', col="yellow", lwd=2)
lines(b25_hat, type='l', col="purple", lwd=2)
lines(b26_hat, type='l', col="brown", lwd=2)
lines(b27_hat, type='l', col="darkgreen", lwd=2)
lines(b28_hat, type='l', col="darkred", lwd=2)
lines(b29_hat, type='l', col="deeppink", lwd=2)
lines(b30_hat, type='l', col="greenyellow", lwd=2)
abline(h=coef(lm_fit)[22], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[23], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[24], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[25], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[26], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[27], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[28], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[29], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[30], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[31], lty="dashed", lwd=3)
legend(x=600,y=6, c("b21_hat", "b22_hat", "b23_hat", "b24_hat", "b25_hat", "b26_hat", "b27_hat", "b28_hat", "b29_hat", "b30_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))


plot(b31_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b32_hat, type='l', col="blue", lwd=2)
lines(b33_hat, type='l', col="orange", lwd=2)
lines(b34_hat, type='l', col="yellow", lwd=2)
lines(b35_hat, type='l', col="purple", lwd=2)
lines(b36_hat, type='l', col="brown", lwd=2)
lines(b37_hat, type='l', col="darkgreen", lwd=2)
lines(b38_hat, type='l', col="darkred", lwd=2)
lines(b39_hat, type='l', col="deeppink", lwd=2)
lines(b40_hat, type='l', col="greenyellow", lwd=2)
abline(h=coef(lm_fit)[32], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[33], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[34], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[35], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[36], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[37], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[38], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[39], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[40], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[41], lty="dashed", lwd=3)
legend(x=600,y=6, c("b31_hat", "b32_hat", "b33_hat", "b34_hat", "b35_hat", "b36_hat", "b37_hat", "b38_hat", "b39_hat", "b40_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))


plot(b41_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b42_hat, type='l', col="blue", lwd=2)
lines(b43_hat, type='l', col="orange", lwd=2)
lines(b44_hat, type='l', col="yellow", lwd=2)
lines(b45_hat, type='l', col="purple", lwd=2)
lines(b46_hat, type='l', col="brown", lwd=2)
lines(b47_hat, type='l', col="darkgreen", lwd=2)
lines(b48_hat, type='l', col="darkred", lwd=2)
lines(b49_hat, type='l', col="deeppink", lwd=2)
lines(b50_hat, type='l', col="greenyellow", lwd=2)
abline(h=coef(lm_fit)[42], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[43], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[44], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[45], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[46], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[47], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[48], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[49], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[50], lty="dashed", lwd=3)
abline(h=coef(lm_fit)[51], lty="dashed", lwd=3)
legend(x=600,y=6, c("b41_hat", "b42_hat", "b43_hat", "b44_hat", "b45_hat", "b46_hat", "b47_hat", "b48_hat", "b49_hat", "b50_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))










############################
############################


plot(b0_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b1_hat, type='l', col="green", lwd=2)
lines(b2_hat, type='l', col="blue", lwd=2)
lines(b3_hat, type='l', col="orange", lwd=2)
lines(b4_hat, type='l', col="yellow", lwd=2)
lines(b5_hat, type='l', col="purple", lwd=2)
lines(b6_hat, type='l', col="brown", lwd=2)
lines(b7_hat, type='l', col="darkgreen", lwd=2)
lines(b8_hat, type='l', col="darkred", lwd=2)
lines(b9_hat, type='l', col="deeppink", lwd=2)
lines(b10_hat, type='l', col="greenyellow", lwd=2)
abline(h=b0_hat[1000], lty="dashed", lwd=3)
abline(h=b1_hat[1000], lty="dashed", lwd=3)
abline(h=b2_hat[1000], lty="dashed", lwd=3)
abline(h=b3_hat[1000], lty="dashed", lwd=3)
abline(h=b4_hat[1000], lty="dashed", lwd=3)
abline(h=b5_hat[1000], lty="dashed", lwd=3)
abline(h=b6_hat[1000], lty="dashed", lwd=3)
abline(h=b7_hat[1000], lty="dashed", lwd=3)
abline(h=b8_hat[1000], lty="dashed", lwd=3)
abline(h=b9_hat[1000], lty="dashed", lwd=3)
abline(h=b10_hat[1000], lty="dashed", lwd=3)
legend(x=600,y=6, c("b0_hat", "b1_hat", "b2_hat", "b3_hat", "b4_hat", "b5_hat", "b6_hat", "b7_hat", "b8_hat", "b9_hat", "b10_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,1,2), col=c("red","green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))



plot(b11_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b12_hat, type='l', col="blue", lwd=2)
lines(b13_hat, type='l', col="orange", lwd=2)
lines(b14_hat, type='l', col="yellow", lwd=2)
lines(b15_hat, type='l', col="purple", lwd=2)
lines(b16_hat, type='l', col="brown", lwd=2)
lines(b17_hat, type='l', col="darkgreen", lwd=2)
lines(b18_hat, type='l', col="darkred", lwd=2)
lines(b19_hat, type='l', col="deeppink", lwd=2)
lines(b20_hat, type='l', col="greenyellow", lwd=2)
abline(h=b11_hat[1000], lty="dashed", lwd=3)
abline(h=b12_hat[1000], lty="dashed", lwd=3)
abline(h=b13_hat[1000], lty="dashed", lwd=3)
abline(h=b14_hat[1000], lty="dashed", lwd=3)
abline(h=b15_hat[1000], lty="dashed", lwd=3)
abline(h=b16_hat[1000], lty="dashed", lwd=3)
abline(h=b17_hat[1000], lty="dashed", lwd=3)
abline(h=b18_hat[1000], lty="dashed", lwd=3)
abline(h=b19_hat[1000], lty="dashed", lwd=3)
abline(h=b20_hat[1000], lty="dashed", lwd=3)
legend(x=600,y=6, c("b11_hat", "b12_hat", "b13_hat", "b14_hat", "b15_hat", "b16_hat", "b17_hat", "b18_hat", "b19_hat", "b20_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))


plot(b21_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b22_hat, type='l', col="blue", lwd=2)
lines(b23_hat, type='l', col="orange", lwd=2)
lines(b24_hat, type='l', col="yellow", lwd=2)
lines(b25_hat, type='l', col="purple", lwd=2)
lines(b26_hat, type='l', col="brown", lwd=2)
lines(b27_hat, type='l', col="darkgreen", lwd=2)
lines(b28_hat, type='l', col="darkred", lwd=2)
lines(b29_hat, type='l', col="deeppink", lwd=2)
lines(b30_hat, type='l', col="greenyellow", lwd=2)
abline(h=b21_hat[1000], lty="dashed", lwd=3)
abline(h=b22_hat[1000], lty="dashed", lwd=3)
abline(h=b23_hat[1000], lty="dashed", lwd=3)
abline(h=b24_hat[1000], lty="dashed", lwd=3)
abline(h=b25_hat[1000], lty="dashed", lwd=3)
abline(h=b26_hat[1000], lty="dashed", lwd=3)
abline(h=b27_hat[1000], lty="dashed", lwd=3)
abline(h=b28_hat[1000], lty="dashed", lwd=3)
abline(h=b29_hat[1000], lty="dashed", lwd=3)
abline(h=b30_hat[1000], lty="dashed", lwd=3)
legend(x=600,y=6, c("b21_hat", "b22_hat", "b23_hat", "b24_hat", "b25_hat", "b26_hat", "b27_hat", "b28_hat", "b29_hat", "b30_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))


plot(b31_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b32_hat, type='l', col="blue", lwd=2)
lines(b33_hat, type='l', col="orange", lwd=2)
lines(b34_hat, type='l', col="yellow", lwd=2)
lines(b35_hat, type='l', col="purple", lwd=2)
lines(b36_hat, type='l', col="brown", lwd=2)
lines(b37_hat, type='l', col="darkgreen", lwd=2)
lines(b38_hat, type='l', col="darkred", lwd=2)
lines(b39_hat, type='l', col="deeppink", lwd=2)
lines(b40_hat, type='l', col="greenyellow", lwd=2)
abline(h=b31_hat[1000], lty="dashed", lwd=3)
abline(h=b32_hat[1000], lty="dashed", lwd=3)
abline(h=b33_hat[1000], lty="dashed", lwd=3)
abline(h=b34_hat[1000], lty="dashed", lwd=3)
abline(h=b35_hat[1000], lty="dashed", lwd=3)
abline(h=b36_hat[1000], lty="dashed", lwd=3)
abline(h=b37_hat[1000], lty="dashed", lwd=3)
abline(h=b38_hat[1000], lty="dashed", lwd=3)
abline(h=b39_hat[1000], lty="dashed", lwd=3)
abline(h=b40_hat[1000], lty="dashed", lwd=3)
legend(x=600,y=6, c("b31_hat", "b32_hat", "b33_hat", "b34_hat", "b35_hat", "b36_hat", "b37_hat", "b38_hat", "b39_hat", "b40_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))


plot(b41_hat, type='l', col="red", lwd=2, xlab="Iterations", ylab="beta estimates", ylim=c(-7,7))
lines(b42_hat, type='l', col="blue", lwd=2)
lines(b43_hat, type='l', col="orange", lwd=2)
lines(b44_hat, type='l', col="yellow", lwd=2)
lines(b45_hat, type='l', col="purple", lwd=2)
lines(b46_hat, type='l', col="brown", lwd=2)
lines(b47_hat, type='l', col="darkgreen", lwd=2)
lines(b48_hat, type='l', col="darkred", lwd=2)
lines(b49_hat, type='l', col="deeppink", lwd=2)
lines(b50_hat, type='l', col="greenyellow", lwd=2)
abline(h=b41_hat[1000], lty="dashed", lwd=3)
abline(h=b42_hat[1000], lty="dashed", lwd=3)
abline(h=b43_hat[1000], lty="dashed", lwd=3)
abline(h=b44_hat[1000], lty="dashed", lwd=3)
abline(h=b45_hat[1000], lty="dashed", lwd=3)
abline(h=b46_hat[1000], lty="dashed", lwd=3)
abline(h=b47_hat[1000], lty="dashed", lwd=3)
abline(h=b48_hat[1000], lty="dashed", lwd=3)
abline(h=b49_hat[1000], lty="dashed", lwd=3)
abline(h=b50_hat[1000], lty="dashed", lwd=3)
legend(x=600,y=6, c("b41_hat", "b42_hat", "b43_hat", "b44_hat", "b45_hat", "b46_hat", "b47_hat", "b48_hat", "b49_hat", "b50_hat", "lm() estimates"), lty=c(1,1,1,1,1,1,1,1,1,1,2), col=c("green","blue", "orange","yellow","purple", "brown","darkgreen","darkred", "deeppink", "greenyellow", "black"))









