rm(list = ls())

library(MASS)

data("mammals")

dim(mammals)

names(mammals)

plot(mammals$body~mammals$brain, col = "red")
plot(mammals$body, mammals$brain, col = "red")

shapiro.test(mammals$body)
shapiro.test(mammals$brain)

qqnorm(mammals$body)
qqline(mammals$body)

qqnorm(mammals$brain)
qqline(mammals$brain)

var.test(mammals$body, mammals$brain)

cor(mammals$body, mammals$brain, method = "spearman")

cor.test(mammals$body, mammals$brain, method = "spearman")
