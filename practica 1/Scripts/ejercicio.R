data = read.csv("./Datos/bank.csv", sep = ";")
head(data)
names(data)

summary(data$age)

parametros = function(x) {
  print(mean(x))
  print(median(x))
  print(sd(x))
}

parametros(data$age)
parametros(data$balance)
parametros(data$duration)

sum(is.na(data))

data2 = data.frame(age = data$age, education = data$education)