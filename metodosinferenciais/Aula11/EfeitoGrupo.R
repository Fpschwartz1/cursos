#############
# Simulacao 1
#############

#set.seed(12345)
#n <- 100; t <- rep(c(0, 1), c(n/2, n/2));
#x <- c(runif(n/2), runif(n/2)) * 10;
#beta0 <- 1; beta1 <- 2; tau <- 10; sigma <- 1.5
## modelo
#y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

#df <- data.frame(anos=x, salario=y, escolaridade=t)
#write.csv2(df, "salario.csv", row.names=FALSE)

rm(list = ls(all = TRUE))

df <- read.csv2("simulacao1.csv")
head(df)

x <- df$anos
y <- df$salario
t <- df$escolaridade
n <- nrow(df)

plot(x, y, type = "n", frame = TRUE,
     main="Simulação 1", xlab = "anos", ylab = "Salario em R$ (x1000)")
abline(h = mean(y[1 : (n/2)]), lwd = 3, col = "lightblue")
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3, col = "salmon")
abline(lm(y ~ x), lwd = 2)

fit <- lm(y ~ x + t)
summary(fit)

abline(coef(fit)[1], coef(fit)[2], lwd = 3, col = "lightblue")
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3, col = "salmon")

points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


#############
# Simulacao 2
#############

#set.seed(12345)
#n <- 100;
#t <- rep(c(0, 1), c(n/2, n/2));
#x <- c(1 + runif(n/2), 2.5 + runif(n/2));
#beta0 <- 0; beta1 <- 2; tau <- 0; sigma <- .2
#y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

#df <- data.frame(x=x, y=y, t=t)
#write.csv2(df, "simulacao2.csv", row.names=FALSE)

rm(list = ls(all = TRUE))

df <- read.csv2("simulacao2.csv")

x <- df$x
y <- df$y
t <- df$t
n <- nrow(df)

plot(x, y, type = "n", frame = FALSE,
     main="Silmulação 2")
abline(h = mean(y[1 : (n/2)]), lwd = 3, col = "lightblue")
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3, col = "salmon")
abline(lm(y ~ x), lwd = 2)

fit <- lm(y ~ x + t)
summary(fit)

abline(coef(fit)[1], coef(fit)[2], lwd = 3, col = "lightblue")
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3, col = "salmon")
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


#############
# Simulacao 3
#############
#set.seed(12345)
#n <- 100;
#t <- rep(c(0, 1), c(n/2, n/2));
#x <- c(runif(n/2), 1.2 + runif(n/2));
#beta0 <- 0; beta1 <- 2; tau <- -1; sigma <- .2
#y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

#df <- data.frame(x=x, y=y, t=t)
#write.csv2(df, "simulacao3.csv", row.names=FALSE)

rm(list = ls(all = TRUE))

df <- read.csv2("simulacao3.csv")

x <- df$x
y <- df$y
t <- df$t
n <- nrow(df)

plot(x, y, type = "n", frame = FALSE,
     main="Simulação 3")
abline(h = mean(y[1 : (n/2)]), lwd = 3, col="lightblue")
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3, col="salmon")
abline(lm(y ~ x), lwd = 2)

fit <- lm(y ~ x + t)
summary(fit)

abline(coef(fit)[1], coef(fit)[2], lwd = 3, col="lightblue")
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3, col="salmon")
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)


#############
# Simulacao 4
#############
#set.seed(123456)
#n <- 100;
#t <- rep(c(0, 1), c(n/2, n/2));
#x <- c(.5 + runif(n/2), runif(n/2));
#beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
#y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)

#df <- data.frame(x=x, y=y, t=t)
#write.csv2(df, "simulacao4.csv", row.names=FALSE)

rm(list = ls(all = TRUE))

df <- read.csv2("simulacao4.csv")

x <- df$x
y <- df$y
t <- df$t
n <- nrow(df)

plot(x, y, type = "n", frame = FALSE,
     main="Simulação 4")
abline(h = mean(y[1 : (n/2)]), lwd = 3, col = "lightblue")
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3, col = "salmon")
abline(lm(y ~ x), lwd = 2)

fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3, col = "lightblue")
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3, col = "salmon")
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)



