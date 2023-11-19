rm(list = ls())

bd <- read.csv("./datos/bank.csv", sep = ";")

plot(bd$age~bd$balance, col = "red")
plot(bd$age,bd$balance, col = "red")

library(nortest)
lillie.test(bd$age)
lillie.test(bd$balance)

qqnorm(bd$age)
qqline(bd$age)

qqnorm(bd$balance)
qqline(bd$balance)

var.test(bd$age, bd$balance)

cor(bd$age, bd$balance, method = "spearman")

cor.test(bd$age, bd$balance, method = "spearman")
