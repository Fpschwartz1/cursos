set.seed(12346)
n <- 10; x <- rnorm(n); y <- x + rnorm(n, sd = .3)

# SITUACAO 1
plot(c(-3, 6), c(-3, 10), type = "n", frame = FALSE,
     xlab = "x", ylab = "Y", main = "Outliers")
fit1 <- lm(y ~ x)
points(x, y, cex = 1, bg = "lightblue", col = "black", pch = 21)
abline(fit1, lwd = 1, col = "blue")

# forcas imaginarias
forca_i <- function(x, y, e){
  for(i in 1:length(x)){
    lines(c(x[i],x[i]), c(y[i],y[i]+e[i]), col="red")
    pch = 24
    if(e[i] < 0) pch = 25
    points(x[i],y[i]+e[i], pch=pch, col="red", cex=0.6)
  }
}

forca_i(x, y, fit1$residuals)

# centro de massa
points(mean(x), mean(y), cex = 0.7, bg = "red", col = "black", pch = 21)
points(mean(x), mean(y)-0.7, pch=17, col="red", cex=3)


# ponto distante
points(mean(x), 5, cex = 1, bg = "darkorange", col = "black", pch = 21)
eout <- 5 - (coef(fit1)[1] + coef(fit1)[2]*mean(x))

forca_i(mean(x), 5, eout)

# ponto onde a forca atua
lines(c(mean(x),mean(x)), c(sum(coef(fit1)*c(1,mean(x))),5), lty="dotted", col = "orange")
points(mean(x), mean(y), cex = 0.7, bg = "red", col = "black", pch = 21)

# modelo com o outlier
x2 <- c(x,mean(x)); y2 <- c(y,5)
fit2 <- lm(y2 ~ x2)
abline(fit2, lwd = 2, col = "darkorange")


# SITUACAO 2
plot(c(-3, 6), c(-3, 10), type = "n", frame = FALSE,
     xlab = "x", ylab = "Y", main = "Outliers")
fit1 <- lm(y ~ x)
points(x, y, cex = 1, bg = "lightblue", col = "black", pch = 21)
abline(fit1, lwd = 1, col = "blue")

# ponto distante
points(4, 1, cex = 1, bg = "darkorange", col = "black", pch = 21)
eout <- (coef(fit1)[1] + coef(fit1)[2]*mean(x)) - 1

# ponto onde a forca atua
lines(c(4,4), c(sum(coef(fit1)*c(1,4)),1), lty="dotted", col = "orange")
forca_i(4, 1, eout)

# centro de massa
points(mean(x), mean(y), cex = 0.7, bg = "red", col = "black", pch = 21)
points(mean(x), mean(y)-0.7, pch=17, col="red", cex=3)

# modelo com o outlier
x2 <- c(x,4); y2 <- c(y,1)
fit2 <- lm(y2 ~ x2)
abline(fit2, lwd = 2, col = "darkorange")

# distancia entre a coordenada x da observacao
# e a media de x
shape::Arrows(mean(x),-2,4,-2,code=3,arr.type = "T",col="red")

# o deslocamento em relacao ao centro de massa
# associado ao potencial de alavancagem 
# define a INFLUENCIA da observacao no conjunto de dados

plot(c(-3, 6), c(-3, 10), type = "n", frame = FALSE,
     xlab = "x", ylab = "Y", main = "Outliers")
abline(fit1, lwd = 1, col = "blue")
abline(fit2, lwd = 1, col = "darkorange")

# pontos ajustados
points(x2,fit2$fitted.values, cex = 1.1, bg = "orange", col = "darkorange", pch = 21)
points(x,fit1$fitted.values, cex = 1.1, bg = "blue", col = "black", pch = 21)

# Cook's Distance - Ralph Dennis Cook
cooks.distance(fit2)
library(olsrr)
ols_plot_cooksd_chart(fit2)
4/length(x2) # threshold 4/n

# DFBETA
coef(fit1)
coef(fit2)
round(coef(fit2) - coef(fit1),5)

round(dfbeta(fit2),5)

ols_plot_dfbetas(fit2, print_plot = FALSE)
2/sqrt(length(x2)) # threshold 2/sqrt(n)

# alavancagem
ols_leverage(fit2)


# SITUACAO 3
set.seed(48392)
n <- 100; x <- rnorm(n); y <- rnorm(n)
plot(x, y, frame = FALSE, cex = 2, pch = 21,
     main="Caso 1", bg = "lightblue", col = "black")
summary(lm(y ~ x))$coefficients
abline(lm(y ~ x))

x <- c(10, x); y <- c(10, y)
plot(x, y, frame = FALSE, cex = 2, pch = 21,
     main="Caso 1", bg = "lightblue", col = "black")
points(10, 10, cex=2, pch=21 , bg="orange")
fit <- lm(y ~ x)
summary(fit)$coefficients
abline(lm(y ~ x))

# cook's distance
ols_plot_cooksd_chart(fit)

# dfbeta
ols_plot_dfbetas(fit, print_plot = FALSE)



# SITUACAO 4
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

# cook's distance
ols_plot_cooksd_chart(fit)

# dfbeta
ols_plot_dfbetas(fit, print_plot = FALSE)

ols_leverage(fit)[1:10]



# SITUACAO 5 
dat <- read.table('orly_owl_Lin_4p_5_flat.txt', header = FALSE)
head(dat,4)

# regressao multivariada
fit <- lm(V1 ~ . -1, data = dat)
summary(fit)$coef

# cook's distance
ols_plot_cooksd_chart(fit)

# dfbeta
ols_plot_dfbetas(fit, print_plot = FALSE)

# valores ajustados versus residuos
plot(predict(fit), resid(fit), pch = '.')

