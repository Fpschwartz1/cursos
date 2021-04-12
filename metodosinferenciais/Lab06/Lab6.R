#####################
# Lab 6 - exercicio 1
#####################

library(datasets)
data(cars)

? cars

fit <- lm(cars$dist ~ cars$speed)

fit$coefficients

plot(cars$speed, cars$dist)
abline(fit)

hist(fit$residuals)

plot(cars$speed, fit$residuals)
lines(c(3,26),c(0,0))

fit

fit$coefficients[1] + fit$coefficients[2] * c(5, 10, 14, 23, 30)

source("lmIC.R")
lmIC(cars$speed, cars$dist, vx = c(5, 10, 14, 23, 30))

summary(fit)



#####################
# Lab 6 - exercicio 2
#####################
data(airquality)

? airquality

# remover NAs
colSums(is.na(airquality))

airquality2 <- airquality[rowSums(is.na(airquality)) == 0, ]

# concentracao de Ozonio em funcao da radiacao solar
fit <- lm(Ozone ~ Solar.R, data = airquality2)

fit$coefficients

plot(airquality2$Solar.R, airquality2$Ozone)
abline(fit)

hist(fit$residuals)

plot(airquality2$Solar.R, fit$residuals)
lines(c(-10,500),c(0,0))

fit$coefficients[1] + fit$coefficients[2] * c(10, 100, 200, 300)

source("lmIC.R")
lmIC(airquality2$Solar.R, airquality2$Ozone, vx = c(10, 100, 200, 300))

summary(fit)

# concentracao de Ozonio em funcao da velocidade do vento
fit <- lm(Ozone ~ Wind, data = airquality2)

fit$coefficients

plot(airquality2$Wind, airquality2$Ozone)
abline(fit)

hist(fit$residuals)

plot(airquality2$Wind, fit$residuals)
lines(c(-10,500),c(0,0))

fit$coefficients[1] + fit$coefficients[2] * c(1.5, 3.7, 10, 15.3, 22)

source("lmIC.R")
lmIC(airquality2$Wind, airquality2$Ozone, vx = c(1.5, 3.7, 10, 15.3, 22))

summary(fit)


#####################
# Lab 6 - exercicio 3
#####################
df <- read.csv2("Eleicoes2014.csv")

df$Receitas.em.2014 <- as.numeric(as.character(df$Receitas.em.2014))

fit <- lm(df$Nominais ~ df$Receitas.em.2014)
fit$coefficients

plot(df$Receitas.em.2014, df$Nominais)
abline(fit)

hist(fit$residuals)

plot(df$Receitas.em.2014, fit$residuals)
lines(c(-10,9e6),c(0,0), col="red")

fit$coefficients[1] + fit$coefficients[2] * c(1000, 50e3, 1e6, 5e6, 8e6)

source("lmIC.R")
lmIC(df$Receitas.em.2014, df$Nominais, vx = c(1000, 50e3, 1e6, 5e6, 8e6))

summary(fit)
