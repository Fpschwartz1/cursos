# A omissao de variaveis resulta 
# no enviesamento dos coeficientes

# uma variavel explicativa
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
plot(fit1$model$Agriculture, fit1$model$Fertility,
     xlab = "Agricultura (%)", 
     ylab = "Índice de Fertilidade", cex = 2)
grid()
abline(fit1, col = "blue")
summary(fit1)$coefficients

# uma variavel explicativa
fit2 <- lm(Fertility ~ Examination, data = swiss)
plot(fit2$model$Examination, fit2$model$Fertility,
     xlab = "Recrutas com nota máxima no exame do exército (%)", 
     ylab = "Índice de Fertilidade", cex = 2)
grid()
abline(fit2, col = "blue")
summary(fit2)$coefficients

pairs(swiss)

# duas variaveis explicativas 
fit3 <- lm(Fertility ~ Agriculture + Examination, data = swiss)
dat <- data.frame(Agricultura = fit3$model$Agriculture, 
                  Fertilidade = fit3$model$Fertility,
                  Exame = fit3$model$Examination)
dat <- dat[order(dat$Exame),]
dat$nivel <- cut(dat$Exame, breaks = 10)
co.pal <- hcl.colors(length(levels(dat$nivel)),
                     palette="Grays") # hcl.pals()
plot(fit3$model$Agriculture, fit3$model$Fertility,
     xlab = "Agricultura (%)", 
     ylab = "Índice de Fertilidade", cex = 2)
abline(fit1); grid()
points(dat$Agricultura, dat$Fertilidade,
       pch = 21, col = "black", cex = 2,
       bg = co.pal[as.integer(dat$nivel)])
abline(fit3, col = "orange")

summary(fit1)$coefficients
summary(fit3)$coefficients


# A inclusao de variaveis que nao deveriam ser incluidas
# faz crescer o erro padrao dos coeficientes dos demais
# regressores.

# BLOCO 1: regressores correlacionados
set.seed(52341)
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- x1/sqrt(2) + rnorm(n) /sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95^2); 
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5) 
cor(x1,x2)
cor(x1,x3)
cor(x2,x3)


# BLOCO 2: regressores nao correlacionados
set.seed(52341)
n <- 100; nosim <- 1000
x1 <- rnorm(n); x2 <- rnorm(n); x3 <- rnorm(n); 
betas <- sapply(1 : nosim, function(i){
  y <- x1 + rnorm(n, sd = .3)
  c(coef(lm(y ~ x1))[2], 
    coef(lm(y ~ x1 + x2))[2], 
    coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)


# BLOCO 3: inflacao na base de dados swiss
data(swiss) 
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit2 <- lm(Fertility ~ Agriculture + Examination, data = swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data = swiss)
c(summary(fit2)$cov.unscaled[2,2], summary(fit3)$cov.unscaled[2,2]) / 
  summary(fit1)$cov.unscaled[2,2] # normalização

# funcao vif
library(car)
fit <- lm(Fertility ~ . , data = swiss)
vif(fit)

fit <- lm(Fertility ~ . - Examination , data = swiss)
vif(fit)



# R2 cresce monotonicamente na medida em que novos
# regressores são incluídos, independentemente da 
# importância do regressor.
n <- 100
plot(c(1, n), 0 : 1, type = "n", 
     xlab = "Qtd. preditores",
     ylab = expression("R"^2),
     main = expression("Variação de R"^2))
r <- sapply(1 : n, function(p) {
        y <- rnorm(n); x <- matrix(rnorm(n * p), n, p)
        summary(lm(y ~ x))$r.squared 
      }
     )
grid()
lines(1 : n, r, lwd = 2)


# O modelo tende ao ajustamento perfeito quando a 
# quantidade de regressores nao redundantes se 
# aproxima do tamanho da amostra.
set.seed(67834)
n <- 14
x <- rnorm(n)
Y <- x + rnorm(n)

df <- data.frame(Y)
SEQ <- NULL
for(i in 1:n){
  df <- cbind(df, x = x^i)
  names(df)[i+1] <- paste0("x",i)

  fit <- lm(Y ~ ., data = df)
  df2 <- data.frame(x, fit = fit$fitted.values)
  df2 <- df2[order(x),]
  
  SEQ <- c(SEQ, round(sum(fit$residuals^2),2))
  plot(x, Y, xlab = "x", ylab = "Y",
       main = paste0("Qtd. preditores: ",i, 
                     "  -  SEQ = ", SEQ[i]))
  text(8, 11.5, "Overfitting", cex=1.5, pos = 4)
  grid()
  
  lines(df2$x, df2$fit, col = "red", lwd = 2)
  
  Sys.sleep(1)
}


# A soma dos erros quadráticos decresce 
# monotonocamente na medida em que novos 
# regressores são incluídos.
plot(SEQ, type = "n",
     main = "SEQ decresce com novos preditores",
     xlab = "Qtd. Preditores")
grid()
points(1:n, SEQ, type = "l")


# ANOVA - resudual sum of squares
pairs(swiss)
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit2 <- lm(Fertility ~ Agriculture + Examination, data = swiss)
fit3 <- lm(Fertility ~ Agriculture + Examination + Education, data = swiss)
fit4 <- lm(Fertility ~ Agriculture + Examination + Education + Catholic, data = swiss)
fit5 <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality, data = swiss)
anova(fit1, fit2, fit3, fit4, fit5)

vif(fit2)
vif(fit3)
vif(fit4)
vif(fit5)

