rm(list = ls(all = TRUE)) 

library(UsingR); data(galton)
library(scales)

#################
# Histogramas
#################
# distribuicao filhos
filho <- round(galton$child / 39.37008, 2)
mu <- round(mean(filho),2)

hist(filho, col="blue",breaks=100,
     main="Histograma da Altura dos Filhos",
     xlab = "Altura dos Filhos (m)",
     ylab = "Frequência")

lines(c(mu, mu), c(-20, 180),col=alpha("red", 0.4),lwd=5)
text(1.7, 170, paste("média = ", mu))
Sys.sleep(2)

# distribuicao pais
pai <- round(galton$parent / 39.37008, 2)
mu <- round(mean(pai),2)

hist(pai, col="blue",breaks=100,
     main="Histograma da Altura dos Pais",
     xlab = "Altura dos Pais (m)",
     ylab = "Frequência")

lines(c(mu, mu), c(-20, 200),col=alpha("red", 0.4),lwd=5)
text(1.725, 205, paste("média = \n", mu))
Sys.sleep(2)

#################
# Simulacao
#################
require(grDevices); require(graphics)
x <- seq(1.63,1.85,0.01)
SEQ <- sapply(x, function (mu) {mean((pai - mu)^2)})
myHist <- function(mu){
  par(mfrow=c(1,2))
  # histograma
  hist(pai, col="blue", breaks=100, main = "Histograma", xlab="altura pais (m)")
  lines(c(mu, mu), c(0, 210),col="red",lwd=4)
  mse <- mean((pai - mu)^2)
  text(mu, 222, bquote(xi==.(format(mu,digits=3))))
  
  # seq
  plot(x, SEQ, type="l",main="SEQ", xlab="altura pais (m)")
  points(mu, mse, col="red", bg="red", pch=21)
  lines(c(mu, mu), c(0, mse), col="red", lwd=2, lty=2)
  # text(1.73, 0.014, paste("SEQ = ", round(mse, 7)))
  e <- bquote(SEQ==sum((x[i]-xi)^2, i=1, n)*''%~~%''*.(format(mse,digits=6)))
  text(1.73, 0.014, e)
  
  par(mfrow=c(1,1))
}
# manipulate(myHist(mu), mu = slider(1.63, 1.85, step = 0.01))

for(mu in x){
  myHist(mu)
  #if(mu == 1.73){
  #  Sys.sleep(5)
  #}
  Sys.sleep(1)
}


##########################
# Dispersao pais x filhos
##########################
library(dplyr)
library(tidyr)

plot(pai,filho, xlab="Altura pais (m)", ylab = "Altura filhos (m)" )
points(mean(pai),mean(filho),col="red",pch=19,lwd=4)
lines(c(mean(pai),mean(pai)), c(0,mean(filho)), lty=2, col="red")
lines(c(0,mean(pai)), c(mean(filho),mean(filho)), lty=2, col="red")

table(pai)

freqData <- as.data.frame(table(pai, filho))
names(freqData) <- c("pai","filho","freq")
plot(as.numeric(as.vector(freqData$pai)),
     as.numeric(as.vector(freqData$filho)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .2 * freqData$freq, 
     xlab="Altura pais (m)", ylab = "Altura filhos (m)" )
points(mean(pai),mean(filho),col="red",pch=19,lwd=4)
lines(c(mean(pai),mean(pai)), c(0,mean(filho)), lty=2, col="red")
lines(c(0,mean(pai)), c(mean(filho),mean(filho)), lty=2, col="red")

# pontos medios em cada X
df <- data.frame(pai, filho)
df <- df %>% group_by(pai)  %>%
  summarize(media=mean(filho))

# regressao
rpf <- lm(filho ~ pai)
df$mediapop <- rpf$coefficients[1] + rpf$coefficients[2]*df$pai

points(df$pai, df$mediapop, col="red", bg="red", pch=21)
abline(rpf, col="red")
Sys.sleep(2)


######################
# Distancias verticais
######################
v <- c(60,100,392,600, 800,900) # 180, 140
plot(pai, filho, pch=19, cex=0.7, col="blue",
     xlab="Altura pais (m)", ylab = "Altura filhos (m)",
     main="Distâncias verticais", type="n")
points(pai[v], filho[v])
abline(coef(rpf))

points(mean(pai),mean(filho),col="red",pch=19,lwd=4)
lines(c(mean(pai),mean(pai)), c(0,mean(filho)), lty=2, col="red")
lines(c(0,mean(pai)), c(mean(filho),mean(filho)), lty=2, col="red")

# inclinacao
x1 <- 1.67; x2 <- 1.72
y1 <- rpf$coefficients[1] + rpf$coefficients[2]*x1
y2 <- rpf$coefficients[1] + rpf$coefficients[2]*x2
lines(c(x1, x2), c(y1,y1), col="blue", lty=2)
lines(c(x2, x2), c(y1, y2), col="blue", lty=2)

e <- bquote(Delta*x)
text(x1+(x2-x1)/2, y1-0.01, e)
e <- bquote(Delta*y)
text(x2+0.005, y1+(y2-y1)/2, e)

for(i in 1:length(v)){
  lines(c(pai[v[i]],pai[v[i]]),
        c(filho[v[i]],rpf$fitted.values[v[i]]),
        col="red", lwd=2)
}
Sys.sleep(2)


#########################
# Simulacao 2
########################
# ajusta dimensoes dos eixos
dimeixo <- function(x, y, xlim, ylim, inc, cex){
  plot.new()
  plot.window(xlim=c(xlim[1],xlim[2]), ylim=c(ylim[1],ylim[2]))
  title(xlab="Altura pais (m)", ylab = "Altura filhos (m)")
  axis(1, seq(xlim[1],xlim[2],inc), seq(xlim[1],xlim[2],inc))
  axis(2, seq(ylim[1],ylim[2],inc), seq(ylim[1],ylim[2],inc))
  box()

  freqData <- as.data.frame(table(x, y))
  names(freqData) <- c("pai","filho","freq")
  
  points(as.numeric(as.vector(freqData$pai)),
         as.numeric(as.vector(freqData$filho)),
         pch = 21, col = "black", bg = "lightblue",
         cex = cex * freqData$freq)
  points(mean(x), mean(y), col="red",pch=19,lwd=4)
}

# verifica inclinacao vs SEQ
myPlot <- function(beta, x, y, xlim, ylim, inc, cex, tcol = "black"){ 
  
  dimeixo(x, y, xlim, ylim, inc, cex)
  
  lines(c(0,0), c(min(y),max(y)))
  lines(c(min(x),max(x)), c(0,0))
  
  abline(0, beta, lwd = 2, col="red")  
  mse <- mean( (y - beta * x)^2 )
  
  e <- bquote(beta[1]==frac(Delta*y,Delta*x)*''%~~%''*.(format(beta, digits=3)))
  title(e)
  text(-0.03, 0.14, paste("SEQ = ", round(mse, 7)), cex=1.2, col = tcol)
}

# Ato 1 - enquadra a origem
x <- pai
y <- filho
dimeixo(x, y, c(min(x), max(x)), c(min(y),max(y)), 0.01, 0.2)
Sys.sleep(4)
dimeixo(x, y, c(1.55, max(x)), c(1.55,max(y)), 0.01, 0.15)
Sys.sleep(1)
dimeixo(x, y, c(1.5, max(x)), c(1.5,max(y)), 0.01, 0.15)
Sys.sleep(1)
dimeixo(x, y, c(1.3, max(x)), c(1.3,max(y)), 0.05, 0.1)
Sys.sleep(1)
dimeixo(x, y, c(0.8, max(x)), c(0.8,max(y)), 0.1, 0.05)
Sys.sleep(1)
dimeixo(x, y, c(0.4, max(x)), c(0.4,max(y)), 0.1, 0.04)
Sys.sleep(1)
dimeixo(x, y, c(0, max(x)), c(0,max(y)), 0.1, 0.04)
Sys.sleep(1)
dimeixo(x, y, c(-0.4, max(x)), c(-0.4,max(y)), 0.1, 0.04)
lines(c(0,0), c(-0.4,max(y)))
lines(c(-0.4,max(x)), c(0,0))
Sys.sleep(2)

# Ato 2 - desloca centro de massa para a origem
x <- pai-mean(pai)
y <- filho-mean(filho)
dimeixo(x, y, c(-0.4, max(pai)), c(-0.4,max(filho)), 0.1, 0.04)
lines(c(0,0), c(-0.4,max(filho)))
lines(c(-0.4,max(pai)), c(0,0))
Sys.sleep(2)

# Ato 3 - inicia com uma inclinacao qualquer
abline(0, 1.61, lwd = 2, col="red")
Sys.sleep(2)
rect(-0.12, -0.18, 0.12, 0.18)
Sys.sleep(2)

# Ato 4 - zoom na origem
dimeixo(x, y, c(-0.11, 0.12), c(-0.16,0.14), 0.01, 0.2)
abline(0, 1.61, lwd = 2, col="red")
lines(c(0,0), c(min(y),max(y)))
lines(c(min(x),max(x)), c(0,0))
Sys.sleep(2)

# Ato 5 - varia inclinacao ate minimo da SEQ
myPlot(1.61, x, y, c(-0.11, 0.12), c(-0.16,0.14), 0.01, 0.2)
for(beta in round(seq(1.61,0.31,-0.1),2)){

  if(beta == 0.61){
    myPlot(beta, x, y, c(-0.11, 0.12), c(-0.16,0.14), 0.01, 0.2, tcol = "red")
    Sys.sleep(5)
  } else {
    myPlot(beta, x, y, c(-0.11, 0.12), c(-0.16,0.14), 0.01, 0.2)
    Sys.sleep(1)
  }
}
Sys.sleep(2)
myPlot(0.61, x, y, c(-0.11, 0.12), c(-0.16,0.14), 0.01, 0.2, tcol = "red")
Sys.sleep(2)

# Ato 6 - redimensiona eixos ...
x <- pai - mean(pai)
y <- filho - mean(filho)
dimeixo(x, y, c(-0.4, max(pai)), c(-0.4,max(filho)), 0.1, 0.04)
e <- bquote(beta==frac(Delta*y,Delta*x)*''%~~%''*.(format(0.61, digits=3)))
title(e)
lines(c(0,0), c(-0.4,max(filho)))
lines(c(-0.4,max(pai)), c(0,0))
abline(0, 0.61, lwd = 2, col="red")
rect(-0.12, -0.18, 0.12, 0.18)
Sys.sleep(2)
dimeixo(x, y, c(-0.4, max(pai)), c(-0.4,max(filho)), 0.1, 0.04)
e <- bquote(beta==frac(Delta*y,Delta*x)*''%~~%''*.(format(0.61, digits=3)))
title(e)
lines(c(0,0), c(-0.4,max(filho)))
lines(c(-0.4,max(pai)), c(0,0))
abline(0, 0.61, lwd = 2, col="red")
Sys.sleep(2)

# Ato 7 - retorna aa ao centro de massa original
x <- pai
y <- filho
dimeixo(x-mean(x)/2, y-mean(y)/2, c(0, max(x)), c(0,max(y)), 0.1, 0.04)
e <- bquote(beta==frac(Delta*y,Delta*x)*''%~~%''*.(format(0.61, digits=3)))
title(e)
lines(c(0,0), c(-0.4,max(y)))
lines(c(-0.4,max(x)), c(0,0))
abline(0, 0.61, lwd = 2, col="red")
Sys.sleep(1)
dimeixo(x, y, c(0, max(x)), c(0,max(y)), 0.1, 0.04)
e <- bquote(beta==frac(Delta*y,Delta*x)*''%~~%''*.(format(0.61, digits=3)))
title(e)
lines(c(0,0), c(-0.4,max(y)))
lines(c(-0.4,max(x)), c(0,0))
abline(0, 0.61, lwd = 2, col="red")
Sys.sleep(2)


# Ato 8 - desloca a reta ate passar pelo centro de massa original
# ... mantendo-se a inclinacao
for(i in c(seq(0,0.6,0.15),  rpf$coefficients[1])){
  dimeixo(x, y, c(0, max(x)), c(0,max(y)), 0.1, 0.04)
  e <- bquote(hat(beta)[1]==frac(Delta*y,Delta*x)*''%~~%''*.(format(0.61, digits=3)))
  title(e)
  lines(c(0,0), c(-0.4,max(y)))
  lines(c(-0.4,max(x)), c(0,0))
  abline(i, 0.61, lwd = 2, col="red")
  Sys.sleep(1)
}

# Ato 9 - encontra-se o intercepto
lines(c(-0.2,2), c(rpf$coefficients[1],rpf$coefficients[1]), col = "blue")
points(0,rpf$coefficients[1],col="blue",pch=19,lwd=4)
e <- bquote(hat(beta)[0]==.(format(rpf$coefficients[1], digits=2)))
text(0, 0.58, e, pos=4)
Sys.sleep(2)
# ... e dx e dy
lines(c(mean(x),mean(x)), c(-0.1,2), col = "blue")
e <- bquote(Delta*x==.(format(mean(x), digits=3)))
text(0.8, 0.6, e, pos=4)
e <- bquote(Delta*y==.(format(mean(y) - rpf$coefficients[1], digits=3)))
text(mean(x)-0.01, 1.2, e, pos=4)
Sys.sleep(2)
e <- bquote(hat(mu)[i]==hat(beta)[0]+hat(beta)[1]*x[i])
text(0.6, 1.3, e, pos=4)

