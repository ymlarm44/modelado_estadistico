rm(list = ls())
library(ppcor, corplot)
datos <- read.csv(file = "C:/Users/Usuario/OneDrive/Facultad/materias/modelado estadistico/practica 4/data/tortugas.csv", sep = ";")

pcor(datos[,1:6])

modelo <- lm(Endemics ~ Elevation + Area + Adjacent, data = datos)
summary(modelo)
