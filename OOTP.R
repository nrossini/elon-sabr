rm(list=ls())

require(dplyr)

batting <- read.csv("/users/nickrossini/downloads/Batting2020.csv")

lm1 <- lm(wOBA ~ CON + GAP + POW + EYE + K.s , data = batting)
summary(lm1)

lm2 <- lm(HR ~ CON + GAP + POW + EYE + K.s , data = batting)
summary(lm2)

