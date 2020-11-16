# classificacao 1
y <- c( 0, 0, 0, 0, 1, 1, 1, 1)
x <- c(40,30,20,10,95,80,70,60)

plot(x, y, type="n", 
     xlab="Arrecadação (milhares de reais)",
     ylab="Status", 
     main = "Classificação",
     xlim=c(-10,max(x)), ylim=c(-0.2,1.2))
lines(c(0,0),c(-0.3,1.3), lty=2)
lines(c(-20,max(x)+10),c(1,1), lty=2)
lines(c(-20,max(x)+10),c(0,0), lty=2)

text(-1,0,"Não Eleito", pos = 3, col="red")
text(-7,1,"Eleito", pos = 3, col="blue")
points(x[y==0],y[y==0], pch="x", col="red")
points(x[y==1],y[y==1], pch="o", col="blue" )
fit <- lm(y ~ x)
abline(fit, lwd=2 )

ylim <- 0.5
xlim <- (ylim - coef(fit)[1])/coef(fit)[2]
lines(c(0,xlim),c(ylim,ylim), lty=2, lwd=2)
text(0,0.5,"0.5", pos = 2)

lines(c(xlim,xlim),c(0,ylim), lty=2, lwd=2)
text(xlim,-0.14,round(xlim,2), pos = 3)



# classificacao 2
y <- c( 0, 0, 0, 0, 1, 1, 1, 1,  1,  1)
x <- c(40,30,20,10,95,80,70,60,185,190)

plot(x, y, type="n", 
     xlab="Arrecadação (milhares de reais)", 
     ylab="Status", 
     main = "Classificação",
     xlim=c(-10,max(x)), ylim=c(-0.2,1.2))
lines(c(0,0),c(-0.3,1.3), lty=2)
lines(c(-20,max(x)+10),c(1,1), lty=2)
lines(c(-20,max(x)+10),c(0,0), lty=2)

text(-1,0,"Não Eleito", pos = 3, col="red")
text(-7,1,"Eleito", pos = 3, col="blue")
points(x[y==0],y[y==0], pch="x", col="red")
points(x[y==1],y[y==1], pch="o", col="blue" )
fit <- lm(y ~ x)
abline(fit, lwd=2 )

ylim <- 0.5
xlim <- (ylim - coef(fit)[1])/coef(fit)[2]
lines(c(0,xlim),c(ylim,ylim), lty=2, lwd=2)
text(0,0.5,"0.5", pos = 2)

lines(c(xlim,xlim),c(0,ylim), lty=2, lwd=2)
text(xlim,-0.14,round(xlim,2), pos = 3)

lines(c(60,60),c(0,1), lty=3, lwd=2, col="red")

