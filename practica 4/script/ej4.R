rm(list = ls())
library(ppcor, corrplot)
data(state)
datos <- data.frame(state.x77)

colnames(datos) <- c("habitantes", "ingresos", "analfabetismo", "esp_vida", "asesinatos", "universitarios", "heladas", "area")

pcor(datos[,1:8])

modelo <- lm(esp_vida ~ habitantes + asesinatos + universitarios + heladas, data = datos)
summary(modelo)
