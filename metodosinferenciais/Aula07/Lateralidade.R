rm(list = ls(all = TRUE))

cores=c("#FF0000B2", 'skyblue')

lim <- c(-3,3)
x <- seq(lim[1], lim[2], by = 0.01)

cn1 <- function(x) {dnorm(x)} # curva normal

######################
# unicaudal a esquerda
######################
plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Unicaudal à esquerda", type="l", lwd=2)

xaux <- seq(qnorm(0.05), lim[2], by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn1(xaux),0), col=cores[2], border=cores[2])

text(-2.5, 0.05, expression(alpha))

# reforca as curvas
lines(x,cn1(x),lwd=2, type="l")
lines(c(-3,3),c(0,0))

#######################
# bicaudal
#######################
plot(x, cn1(x), ylab="", xlab="x",
     main="Bicaudal", type="l", lwd=2)

xaux <- seq(qnorm(0.025), qnorm(0.975), by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn1(xaux),0), col=cores[2], border=cores[2])

text(-2.7, 0.05, expression(alpha))
text(-2.4, 0.05, "/2")

text(2.4, 0.05, expression(alpha))
text(2.7, 0.05, "/2")

# reforca as curvas
lines(x,cn1(x),lwd=2, type="l")
lines(c(-3,3),c(0,0))


######################
# unicaudal a direita
######################
plot(x, cn1(x), ylab="", xlab="x",
     main="Unicaudal à direita", type="l", lwd=2)

xaux <- seq(lim[1], qnorm(0.95), by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn1(xaux),0), col=cores[2], border=cores[2])

text(2.4, 0.05, expression(alpha))

# reforca as curvas
lines(x,cn1(x),lwd=2, type="l")
lines(c(-3,3),c(0,0))



