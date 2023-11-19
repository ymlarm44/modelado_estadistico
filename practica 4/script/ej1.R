install.packages("corplot")
library(ppcor, corplot)
datos <- read.csv(file = "C:/Users/Usuario/Downloads/Práctica 3-20230404/Datos/supermercados.csv", sep = ";")
pcor(x = datos[,2:5])

modelo <- lm(ventas ~ TV + radios + diarios ,data = datos)
summary(modelo)

modelo2 <- lm(ventas ~ TV + radios, data = datos)
summary(modelo2)

nuevos <- data.frame(TV = c(125.55, 234.00), radios = c(23.40, 85.50), diarios = c(89.70, 65.00))

nuevo_ventas <- predict(object = modelo2, newdata = nuevos)

nuevo_ventas
