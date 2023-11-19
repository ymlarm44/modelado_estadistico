rm(list = ls())
datos <- read.table(file = "./datos/datos.txt", header = TRUE)

qqnorm(datos$Peso)
qqline(datos$Peso)

qqnorm(datos$Cintura)
qqline(datos$Cintura)

shapiro.test(datos$Peso)
shapiro.test(datos$Cintura)

plot(datos$Peso~datos$Cintura)
cor.test(datos$Peso, datos$Cintura)

modelo = lm(datos$Cintura~datos$Peso)
summary(modelo)

abline(modelo)
