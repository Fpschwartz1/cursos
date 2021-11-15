rm(list = ls(all = TRUE))

set.seed(1234)
n <- 100;
x2 <- runif(n)
x1 <- runif(n)
beta0 <- 0; beta1 <- 2; beta2 <- 4 ; sigma <- .01

Y <- beta0 + x1 * beta1 + beta2 * x2 + rnorm(n, sd = sigma)

cor.test(x1, x2)

fit2 <- lm(Y ~ x1 + x2)
summary(fit2)$coef

plot(x1, Y, type = "n", frame = FALSE,
     main="Regressores Independentes")

fit1 <- lm(Y ~ x1)
abline(fit1, lwd = 2)
summary(fit1)$coef

co.pal <- heat.colors(n)
points(x1, Y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

# 3D
library(rgl)
plot3d(x1, x2, Y)

# Praticamente nao altera as relacoes marginais
summary(lm(Y ~ x1))$coefficients
summary(lm(Y ~ x2))$coefficients
summary(lm(Y ~ x1 + x2))$coefficients


fit1 <- lm(Y ~ x1)
fit2 <- lm(Y ~ x1 + x2)
anova(fit1,  fit2)

vif(fit2)
