rm(list=ls())

################
## Problem #3 ##
################
library(dplyr)

df <- USArrests
head(df)

## Part A
hc_complete <- hclust(dist(df), method='complete')
plot(hc_complete, main="Complete linkage via Euclidean distance", cex=0.6)

## Part B
cutree(hc_complete , 3)

## Part C
df_scaled <- df
df_scaled$Murder_scaled <- df_scaled$Murder / sd(df_scaled$Murder)
df_scaled$Assault_scaled <- df_scaled$Assault / sd(df_scaled$Assault)
df_scaled$UrbanPop_scaled <- df_scaled$UrbanPop / sd(df_scaled$UrbanPop)
df_scaled$Rape_scaled <- df_scaled$Rape / sd(df_scaled$Rape)
df_scaled <- select(df_scaled, Murder_scaled, Assault_scaled, UrbanPop_scaled, Rape_scaled)
head(df_scaled)

sd(df_scaled$Murder_scaled)
sd(df_scaled$Assault_scaled)
sd(df_scaled$UrbanPop_scaled)
sd(df_scaled$Rape_scaled)

hc_complete_scaled <- hclust(dist(df_scaled), method='complete')
plot(hc_complete_scaled, main="Complete linkage via Euclidean distance (data scaled to sd=1)", cex=0.6)
