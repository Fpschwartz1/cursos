set.seed(1234)
n <- 100;
x2 <- runif(n)
x1 <- runif(n)
beta0 <- 0; beta1 <- 2; beta2 <- 4 ; sigma <- .01

y <- beta0 + x1 * beta1 + beta2 * x2 + rnorm(n, sd = sigma)

plot(x1, y, type = "n", frame = FALSE,
     main="Regressores Independentes")
abline(lm(y ~ x1), lwd = 2)
co.pal <- heat.colors(n)
points(x1, y, pch = 21, col = "black", bg = co.pal[round((n - 1) * x2 + 1)], cex = 2)

# 3D
library(rgl)
plot3d(x1, x2, y)

# Praticamente nao altera as relacoes marginais
summary(lm(y ~ x1))$coefficients
summary(lm(y ~ x2))$coefficients
summary(lm(y ~ x1 + x2))$coefficients

