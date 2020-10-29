rm(list = ls(all = TRUE))

cores=c("#FF0000B2", 'skyblue')

media1 <- 0
ep1 <- 1


lim <- c(-3,3)
x <- seq(lim[1], lim[2], by = 0.01)

cn1 <- function(x) {dnorm(x,media1,ep1)} # curva normal

# densidade maxima
plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Curva Normal Padrão", type="l", lwd=2)

xaux <- seq(-1.96, 1.96, by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn1(xaux),0), col=cores[2], border=cores[2])


# reforca as curvas
lines(x,cn1(x),lwd=2, type="l")
lines(c(-3,3),c(0,0))
