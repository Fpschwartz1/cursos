library(UsingR)
data(diamond)

par(mfrow=c(1,1))
x <- diamond$carat
y <- diamond$price

plot(x, y,
     xlab = "Massa (carats)", ylab = "Preço (SIN $)",
     main = "Preço do diamante em função da massa",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)

fit <- lm(y ~ x)
abline(fit, lwd = 2)


x <- c( 0.31, 0.22, 0.24 )
fit$coefficients[1] + fit$coefficients[2]*x

summary(fit)





B0 <- fit$coefficients[1]
B1 <- fit$coefficients[2]

Y <- B0 + B1*x
Y

# deslocamento de x
mu <- mean(x)
mu

Y <- B0 + B1*x + B1*mu - B1*mu

Y <- (B0 +B1*mu) + B1*(x - mu)
(B0 +B1*mu)

lines(c(mu, mu), c(0,max(Y)), col="red", lwd=2)

fit

## deslocamento de x com lm
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond)
fit2


# escalonamento de x
Y <- B0 + (B1/0.2)*(x*0.2)
(B1/0.2)

plot(x*0.2, y,
     xlab = "Massa (g)", ylab = "Preço (SIN $)",
     main = "Preço do diamante em função da massa",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)

## escalonamento de x com lm
fit <- lm(price ~ I(carat * 0.2), data = diamond)
coef(fit)


# combinando deslocamento com escalonamento
fit <- lm(price ~ I((carat - mean(carat)) * 0.2), data = diamond)
coef(fit)

