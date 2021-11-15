rm(list = ls(all = TRUE))

data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~ . , data = swiss)
plot(fit)
par(mfrow = c(1, 1))


# normalidade - Shapiro-Wilk (p > 0.05)
shapiro.test(fit$residuals)

# homocedasticidade - Breusch-Pagan (p > 0.05)
library(lmtest)
? bptest
bptest(fit)

## generate a regressor
x <- rep(c(-1,1), 50)
## generate heteroskedastic and homoskedastic disturbances
err1 <- rnorm(100, sd=rep(c(1,2), 50))
err2 <- rnorm(100)
## generate a linear relationship
y1 <- 1 + x + err1
y2 <- 1 + x + err2
## perform Breusch-Pagan test
bptest(y1 ~ x)
bptest(y2 ~ x)


# independencia dos residuos: Durbin_Watson
# um dos pressupostos que os modelos de regressao precisam
# atender eh a ausencia de correlacao entre os erros,
# isto eh, os erros sao independentes sob a condicao de
# normalidade
? dwtest
dwtest(Fertility ~ ., data = swiss, alternative = "two.sided")


# exemplo de modelo com premissas atendidas
library(car)
fit <- lm(prestige ~ women + education + type, data = Prestige)
shapiro.test(fit$residuals) # distribuicao normal
bptest(fit) # variancia constante
dwtest(fit) # independencia dos residuos






