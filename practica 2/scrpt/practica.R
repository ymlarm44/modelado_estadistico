peso <- c(74, 92, 63, 72, 58, 76, 85, 78, 67, 91, 85, 73, 62, 80, 72)
altura <- c(168, 196, 170, 175, 162, 183, 169, 190, 172, 188, 186, 176, 170, 176, 179)
bd <- data.frame(Peso_kg = peso, altura_cm = altura)
bd

plot(altura~peso, col = "red")
plot(peso,altura, col = "red")

shapiro.test(peso)
shapiro.test(altura)

qqnorm(peso)
qqline(peso)

qqnorm(altura)
qqline(altura)

var.test(peso, altura)

cor(peso, altura)

cor.test(peso, altura)

