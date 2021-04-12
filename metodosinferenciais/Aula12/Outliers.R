set.seed(1234)
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE,
     xlab = "X", ylab = "Y", main = "Outliers")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)

# superior-esquerda
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE,
     xlab = "X", ylab = "Y", main = "Outliers")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
abline(lm( c(y,5) ~ c(x,0)), lwd = 2, col = "darkorange")

# superior-direita
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE,
     xlab = "X", ylab = "Y", main = "Outliers")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(5, 5, cex = 2, bg = "darkorange", col = "black", pch = 21)
abline(lm( c(y,5) ~ c(x,5)), lwd = 2, col = "darkorange")

# inferior-direita
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE,
     xlab = "X", ylab = "Y", main = "Outliers")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(5, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
abline(lm( c(y,0) ~ c(x,5)), lwd = 2, col = "darkorange")

# inferior-esquerda
plot(c(-3, 6), c(-3, 6), type = "n", frame = FALSE,
     xlab = "X", ylab = "Y", main = "Outliers")
abline(lm(y ~ x), lwd = 2)
points(x, y, cex = 2, bg = "lightblue", col = "black", pch = 21)
points(0, 0, cex = 2, bg = "darkorange", col = "black", pch = 21)
abline(lm( c(y,0) ~ c(x,0)), lwd = 2, col = "darkorange")


#########################
# Diagnostico de outliers
#########################

########
# CASO 1
########
set.seed(48392)
n <- 100; x <- rnorm(n); y <- rnorm(n)
plot(x, y, frame = FALSE, cex = 2, pch = 21,
     main="Caso 1", bg = "lightblue", col = "black")
summary(lm(y ~ x))$coefficients

x <- c(10, x); y <- c(10, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21,
     main="Caso 1", bg = "lightblue", col = "black")
points(10, 10, cex=2, pch=21 , bg="orange")
fit <- lm(y ~ x)
summary(fit)$coefficients
abline(lm(y ~ x))

# alavancagem
round(hatvalues(fit)[1 : 10], 3)

# influencia
round(dfbetas(fit)[1 : 10, 2], 3)


########
# CASO 2
########
set.seed(48392)
n <- 100; x <- rnorm(n); y <- x + rnorm(n, sd = .3)

plot(x, y, frame = FALSE, cex = 2, pch = 21,
     main="Caso 1", bg = "lightblue", col = "black")
fit1 <- lm(y ~ x)
summary(fit1)$coefficients
abline(fit1)

x <- c(5, x); y <- c(5, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21,
     main="Caso 2", bg = "lightblue", col = "black")
points(5, 5, cex=2, pch=21 , bg="orange")
abline(fit1, lwd=2)

fit2 <- lm(y ~ x)
abline(fit2, col = "orange", lwd=2)
summary(fit)$coefficients


# alavancagem
round(hatvalues(fit2)[1 : 10], 3)

# influencia
round(dfbetas(fit2)[1 : 100, 2], 3)


########
# CASO 3
########
dat <- read.table('orly_owl_Lin_4p_5_flat.txt', header = FALSE)
head(dat,4)

# regressao multivariada
fit <- lm(V1 ~ . -1, data = dat)
summary(fit)$coef

# valores ajustados versus residuos
plot(predict(fit), resid(fit), pch = '.')
