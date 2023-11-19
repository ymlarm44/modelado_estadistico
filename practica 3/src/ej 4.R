rm(list = ls())
datos <- read.csv(file = "C:/Users/Usuario/Downloads/Práctica 3-20230404/Datos/supermercados.csv", sep = ";")

plot(ventas~TV, datos)
plot(ventas~radios, datos)
plot(ventas~diarios, datos)

cor.test(datos$ventas, datos$TV)
cor.test(datos$ventas, datos$radios)
cor.test(datos$ventas, datos$diarios)

modelo_tv <- lm(ventas~TV, data = datos)
modelo_radios <- lm(ventas~radios, data = datos)
modelo_diarios <- lm(ventas~diarios, data = datos)

summary(modelo_TV)
summary(modelo_radios)
summary(modelo_diarios)

nuevos <- data.frame(TV = c(125.55, 234.00), radios = c(23.40, 85.50), diarios = c(89.70, 65.00))

nuevo_ventas <- predict(object = modelo_tv, newdata = nuevos)
abline(modelo_tv)
points(nuevo_ventas~nuevos$TV, pch = 17, col = "green")
