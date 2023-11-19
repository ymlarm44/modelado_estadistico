rm(list = ls())
data(faithful)

plot(eruptions~waiting,faithful)
cor.test(faithful$eruptions, faithful$waiting)

modelo = lm(eruptions~waiting,data = faithful)
summary(modelo)

abline(modelo)

datos_nuevos <- data.frame(waiting = c(58, 67.2, 75, 92))

nuevos_eruptions <- predict(object = modelo, newdata = datos_nuevos)

points(nuevos_eruptions~datos_nuevos$waiting, pch = 17, col = "green")
