#CARGA DE DATOS
rm(list=ls())
data <- read.table("./data/auto-mpg.data")

#LIBRERIAS
library(dplyr)
library(nortest)
library(ppcor)
library(tidyr)

#EXPLORACION DE DATOS
dim(data)
names(data)
head(data)
summary(data)
ftable(data$V7)
ftable(data$V8)
origin1 <- filter(data, V8 == 1)
head(origin1)
origin2 <- filter(data, V8 == 2)
head(origin2$V9)
origin3 <- filter(data, V8 == 3)
head(origin3$V9)

#ACONDICIONAMIENTO
nombreVariables <- c('mpg', 'cyl', 'displacement', 'hp', 'weight', 'acceleration', 'model_year', 'origin', 'car_name')
colnames(data) <- nombreVariables
data$hp <- as.numeric(data$hp)
#data$cyl <- as.factor(data$cyl)
#data$model_year <- as.factor(data$model_year)
#data$origin <- as.factor(data$origin)

#BUSQUEDA/CORRECCION DE NA
apply(X = is.na(data), MARGIN = 2, FUN = sum) #(331, 127, 33, 337, 355, 375)
dim(data) #TAMAÑO ANTES DE ELIMINAR NA
data <- data %>% drop_na(hp)
dim(data) #TAMAÑO DESPUES DE ELIMINAR NA
summary(data)

#BUSQUEDA DE FILAS DUPLICADAS
data %>% duplicated() %>% ftable()

#DIVISION DE DATOS TRAIN/TEST     
#library(caTools)
#set.seed(1234)
#split <- sample.split(data$mpg, SplitRatio = 0.80)
#data.train <- subset(data, split == TRUE)
#data.test <- subset(data, split == FALSE)


#ESTUDIO DE NORMALIDAD
lillie.test(data$mpg)
lillie.test(data$displacement)
lillie.test(data$hp)
lillie.test(data$weight)
lillie.test(data$acceleration)

qqnorm(data$mpg)
qqline(data$mpg)
#--------------------------
qqnorm(data$displacement)
qqline(data$displacement)         
#--------------------------
qqnorm(data$hp)
qqline(data$hp)
#--------------------------
qqnorm(data$weight)
qqline(data$weight)
#--------------------------
qqnorm(data$acceleration)
qqline(data$acceleration)

#ESTUDIO DE CORRELACION LINEAL
names(data)
pcor(x = data[,1:8], method = "spearman")


#PRIMER MODELO (REGRESION LINEAL MULTIPLE)
modelo1 <- lm(mpg ~cyl+displacement+hp+weight+acceleration+model_year+origin, data = data)
summary(modelo) #Adjusted R-squared = 0.8182

#AJUSTAMIENTO DEL MODELO
pairs(mpg ~ cyl+displacement+hp+weight+acceleration+model_year+origin, data)

#ENCUENTRO UN PATRON EN LAS VARIABLES DISPLACEMENT, HP, WEIGHT RESPECTO A MPG
pairs(mpg ~ displacement+hp+weight,
      data=data,
      main= 'Auto-mpg',
      cex.main = 0.9,
      pch = 21,
      col = c('red', 'blue'),
      bg = c('red', 'blue')
)

#REALIZO TRANSFORMACIONES

#DISPLACEMENT
y <- data$mpg
x <- data$displacement
plot(x, y)
cor.test(x, y)

#CASO EXPONENCIAL   <- cor = -0.8536
plot(x, log(y))
cor.test(x, log(y))

#CASO POTENCIAL   <- cor = -0.8600 (MEJOR)
plot(log(x), log(y))
cor.test(log(x), log(y))

#------------------------------------------------
#HP
x2<- data$hp
plot(x2, y)
cor.test(x2, y)

#CASO EXPONENCIAL  <- cor = -0.8301
plot(x2, log(y))
cor.test(x2, log(y))

#CASO POTENCIAL  <- cor = -0.8501 (MEJOR)
plot(log(x2), log(y))
cor.test(log(x2), log(y))

#------------------------------------------------
#WEIGHT
x3 <- data$weight
plot(x3, y)
cor.test(x3, y)

#CASO EXPONENCIAL   <- rho= -0.8756 (MEJOR)
plot(x3, log(y))
cor.test(x3, log(y))

#CASO POTENCIAL   <- rho= -0.8745
plot(log(x3), log(y))
cor.test(log(x3), log(y))

#MODELO 2 (REGRESION LINEAL MULTIPLE)
modelo2 <- lm(log(mpg) ~cyl+log(displacement)+log(hp)+weight+acceleration+model_year+origin, data = data)
summary(modelo2) #Adjusted R-squared = 0.8844
