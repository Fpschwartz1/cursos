library(UsingR); data(diamond)
x <- diamond$carat
y <- diamond$price
par(mfrow=c(1,2))
par(cex.axis=0.8, cex.main=0.9)
## regressão linear price ~ carat com indicação dos resíduos
plot(x, y, main="\n\n(a)",
     xlab = "Massa (quilates)", ylab = "Preço (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)
fit <- lm(y ~ x)
abline(fit, lwd=1.5)
# linhas dos resíduos
for(i in 1:length(x)){
  lines(c(x[i],x[i]),
        c(y[i],fit$fitted.values[i]),
        col="red")
}
## gráfico dos resíduos versus a massa (x)
plot(x,fit$residuals, main="\n\n(b)",
     xlab = "Massa (quilates)", ylab = "Resíduo")
lines(c(0,1),c(0,0))
# linhas dos resíduos
for(i in 1:length(x)){
  lines(c(x[i],x[i]),
        c(0,fit$residuals[i]),
        col="red")
}
par(mfrow=c(1,1), cex.main=1.2)
title("Resíduos: linhas verticais")



x <- runif(100, -3, 3)
y <- x + sin(x) + rnorm(100, sd = .2)
par(mfrow=c(1,2))
par(cex.axis=0.8, cex.main=0.9)
## gráfico de dispersão
plot(x, y, main="(a)")
fit <- lm(y ~ x)
abline(fit, lwd=2)
## gráfico do resíduo em função de x
plot(x, fit$residuals, main="(b)", ylab = "Resíduo")
lines(c(-4,4),c(0,0))
# variação senoidal
y <- x + sin(x)
fit <- lm(y ~ x)
lines(x[order(x)],fit$residuals[order(x)], col="red", lwd=2)
par(mfrow=c(1,1))



# variancia
x <- runif(100, 0, 6)
y <- x + rnorm(100, mean = 0, sd = .01 * x)
par(mfrow=c(1,2))
par(cex.axis=0.8, cex.main=0.9)
## gráfico de dispersão
plot(x, y, main="(a)")
fit <- lm(y ~ x)
abline(fit)
## gráfico do resíduo em função de x
plot(x, fit$residuals, main="(b)", ylab = "Resíduo")
lines(c(-1,7),c(0,0))
lines(c(0,x[order(fit$residuals)[1]]),c(0,min(fit$residuals)), col="red")
lines(c(0,x[order(fit$residuals)[length(x)]]),c(0,max(fit$residuals)), col="red")
par(mfrow=c(1,1))
