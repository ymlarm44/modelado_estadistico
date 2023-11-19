rm(list = ls())
peso <- c(74, 92, 63, 72, 58, 76, 85, 78, 67, 91, 85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 183, 169, 190, 172, 188, 186, 176, 170, 176, 179)

qqnorm(peso)
qqline(peso)

qqnorm(altura)
qqline(altura)

shapiro.test(peso)
shapiro.test(altura)

library(MASS)
plot(peso~altura)
cor.test(peso, altura)

modelo = lm(peso~altura)
summary(modelo)
abline(modelo)

altura.new <- data.frame(altura = 177)

peso.new <- predict(object = modelo, newdata = altura.new)

points(peso.new~altura.new$altura, pch = 17, col = "green")
