rm(list = ls(all = TRUE))

cores=c("#FF0000B2", 'skyblue')

n <- 258

media1 <- 58.29
ep1 <- 17.13/sqrt(n)
# ep1 <- 18/sqrt(n)

media2 <- 61
ep2 <- 18/sqrt(n)

nc <- 0.95 # nivel de confianca
alpha <- 1 - nc # nivel de significancia

########################################################
# Abordagem a partir do TCL - desvio padrão populacional
########################################################
cn1 <- function(x) {dnorm(x,media1,ep1)} # curva normal

lim <- c(media1-3*ep1, media1+3*ep1)
x <- seq(lim[1], lim[2], by = 0.01)

# curva normal
plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Teorema Central do Limite", type="l", lwd=2)
lines(lim,c(0,0))

# media1
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2)

# intervalo de confianca 95%

ic <- seq(media1+qnorm(alpha/2)*ep1, media1+qnorm(nc+alpha/2)*ep1, by = 0.01)
polygon(c(min(ic), ic, max(ic)), c(0, cn1(ic),0), col=cores[2], border=cores[2])

# limites do intervalo de confianca
ll <- min(ic)
ul <- max(ic)

# reforca as linhas
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2) # media1
lines(c(ll, ll), c(-1, cn1(ll)), lwd=2) # ll
lines(c(ul, ul), c(-1, cn1(ul)), lwd=2) # ll
lines(c(media2, media2), c(-1, cn1(media2)), lwd=2, col="orange") # media2
lines(x,cn1(x),lwd=2, type="l")
lines(lim,c(0,0))

######################################################
# Abordagem a partir do TCL - desvio padrao da amostra
######################################################
cn1 <- function(x) {dnorm(x,media1,ep1)} # curva normal

lim <- c(media1-3*ep1, media1+3*ep1)
x <- seq(lim[1], lim[2], by = 0.01)

# curva normal
plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Teorema Central do Limite", type="l", lwd=2)
lines(lim,c(0,0))

# media1
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2)

# intervalo de confianca 95%

ic <- seq(media1+qnorm(alpha/2)*ep2, media1+qnorm(nc+alpha/2)*ep2, by = 0.01)
polygon(c(min(ic), ic, max(ic)), c(0, cn1(ic),0), col=cores[2], border=cores[2])

# limites do intervalo de confianca
ll <- min(ic)
ul <- max(ic)

# reforca as linhas
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2) # media1
lines(c(ll, ll), c(-1, cn1(ll)), lwd=2) # ll
lines(c(ul, ul), c(-1, cn1(ul)), lwd=2) # ll
lines(c(media2, media2), c(-1, cn1(media2)), lwd=2, col="orange") # media2
lines(x,cn1(x),lwd=2, type="l")
lines(lim,c(0,0))



####################################
# Transicao
####################################
# curva normal
cn1 <- function(x) {dnorm(x,media1,ep2)} # curva normal

lim <- c(media1-3*ep1, media1+3*ep1)
x <- seq(lim[1], lim[2], by = 0.01)

plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Teorema Central do Limite", type="n", lwd=2)
lines(lim,c(0,0))
lines(c(media2, media2), c(-1, cn1(media2)), lwd=2, col="orange") # media2


####################################
# Abordagem a partir da amostra
####################################
cn2 <- function(x) {dnorm(x,media2,ep2)} # curva normal

lim <- c(media2-3*ep1, media2+3*ep1)
x <- seq(lim[1], lim[2], by = 0.01)

# curva normal
plot(x, cn2(x), ylab="Densidade", xlab="x",
     main="Teorema Central do Limite", type="n", lwd=2)
lines(c(media2, media2), c(-1, cn1(media2)), lwd=2, col="orange") # media2
lines(lim,c(0,0))

# intervalo de confianca 95%
ic <- seq(media2+qnorm(alpha/2)*ep2, media2+qnorm(nc+alpha/2)*ep2, by = 0.01)
polygon(c(min(ic), ic, max(ic)), c(0, cn2(ic),0), col=cores[2], border=cores[2])

# limites do intervalo de confianca
ll <- min(ic)
ul <- max(ic)

# reforca as linhas
lines(c(media2, media2), c(-1,max(cn2(x))), col="orange", lwd=2) # media2
lines(c(ll, ll), c(-1, cn2(ll)), lwd=2) # ll
lines(c(ul, ul), c(-1, cn2(ul)), lwd=2) # ll
lines(x,cn2(x),lwd=2, type="l")
lines(lim,c(0,0))

lines(c(media1, media1), c(-1, cn2(media1)), lwd=2, col="red") # media1
lines(lim,c(0,0))


####################################
# Abordagem do TCL - H1:58.29 < 61
####################################
cn1 <- function(x) {dnorm(x,media1,ep1)} # curva normal

lim <- c(media1-3*ep1, media1+3*ep1)
x <- seq(lim[1], lim[2], by = 0.01)

# curva normal
plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Teorema Central do Limite", type="l", lwd=2)
lines(lim,c(0,0))

# media1
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2)

# intervalo de confianca 95%
ic <- seq(lim[1], qnorm(nc, media1, ep1), by = 0.01)
polygon(c(min(ic), ic, max(ic)), c(0, cn1(ic),0), col=cores[2], border=cores[2])

# limites do intervalo de confianca
ul <- max(ic)

# reforca as linhas
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2) # media1
lines(c(ul, ul), c(-1, cn1(ul)), lwd=2) # ll
lines(c(media2, media2), c(-1, cn1(media2)), lwd=2, col="orange") # media2
lines(x,cn1(x),lwd=2, type="l")
lines(lim,c(0,0))

###################################################
# Abordagem do TCL - bicaudal - unicaudal a direita
###################################################
media2 <- 60.30
cn1 <- function(x) {dnorm(x,media1,ep1)} # curva normal

lim <- c(media1-3*ep1, media1+3*ep1)
x <- seq(lim[1], lim[2], by = 0.01)

# curva normal
plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Bilateral", type="l", lwd=2)
lines(lim,c(0,0))

# media1
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2)

# intervalo de confianca 95%
ic <- seq(media1+qnorm(alpha/2)*ep1, media1+qnorm(nc+alpha/2)*ep1, by = 0.01)
polygon(c(min(ic), ic, max(ic)), c(0, cn1(ic),0), col=cores[2], border=cores[2])

# limites do intervalo de confianca
ll <- min(ic)
ul <- max(ic)

# reforca as linhas
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2) # media1
lines(c(ll, ll), c(-1, cn1(ll)), lwd=2) # ll
lines(c(ul, ul), c(-1, cn1(ul)), lwd=2) # ll
lines(c(media2, media2), c(-1, cn1(media2)), lwd=2, col="orange") # media2
lines(x,cn1(x),lwd=2, type="l")
lines(lim,c(0,0))


# unicaudal a direita
lim <- c(media1-3*ep1, media1+3*ep1)
x <- seq(lim[1], lim[2], by = 0.01)

# curva normal
plot(x, cn1(x), ylab="Densidade", xlab="x",
     main="Unicaudal à direita", type="l", lwd=2)
lines(lim,c(0,0))

# media1
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2)

# intervalo de confianca 95%
ic <- seq(lim[1], qnorm(nc, media1, ep1), by = 0.01)
polygon(c(min(ic), ic, max(ic)), c(0, cn1(ic),0), col=cores[2], border=cores[2])

# limites do intervalo de confianca
ul <- max(ic)

# reforca as linhas
lines(c(media1, media1), c(-1,max(cn1(x))), col="red", lwd=2) # media1
lines(c(ul, ul), c(-1, cn1(ul)), lwd=2) # ll
lines(c(media2, media2), c(-1, cn1(media2)), lwd=2, col="orange") # media2
lines(x,cn1(x),lwd=2, type="l")
lines(lim,c(0,0))
