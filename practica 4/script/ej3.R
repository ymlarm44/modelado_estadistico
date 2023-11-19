rm(list = ls())
library(ppcor, corrplot)
datos <- read.csv(file = "C:/Users/Usuario/OneDrive/Facultad/materias/modelado estadistico/practica 4/data/produccion.csv", sep = ";")

pcor(datos[,1:10])

modelo_may <- lm(produccion ~ premay + tempmay, data = datos)

modelo_jun <- lm(produccion ~ prejun + tempjun, data = datos)

modelo_jul <- lm(produccion ~ prejul + tempjul, data = datos)

modelo_ago <- lm(produccion ~ preago + tempago, data = datos)

summary(modelo_may)
summary(modelo_jun)
summary(modelo_jul)
summary(modelo_ago)

modelo_todo <- lm(produccion ~ premay + tempmay + prejun + tempjun + prejul + tempjul + preago + tempago, data = datos)
summary(modelo_todo)
