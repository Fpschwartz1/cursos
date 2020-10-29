rm(list = ls(all = TRUE))

cores=c("#FF0000B2", 'skyblue')

media1 <- 5
ep1 <- 0.4

media2 <- 8
ep2 <- 1


lim <- c(media1-round(4*max(ep1,ep2),0),media2+round(4*max(ep1,ep2),0))
x <- seq(lim[1], lim[2], by = 0.01)

cn1 <- function(x) {dnorm(x,media1,ep1)} # curva normal
cn2 <- function(x) {dnorm(x,media2,ep2)} # curva normal

# densidade maxima
dmax <- max(cn1(x))
if(dmax < max(cn2(x))) dmax <- max(cn2(x))

y=rep(c(0,dmax),c(ceiling(length(x)/2),floor((length(x)/2))))

plot(x, y, ylab="Densidade", xlab="x",
     main="Curva Normal",type="n",lwd=2)


# distribuicao 2
lines(x,cn2(x),lwd=2, col=cores[1], type="l")
ll2 <- media2 - 1.96*ep2
ul2 <- media2 + 1.96*ep2

xaux <- seq(lim[1], ll2, by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn2(xaux),0), col=cores[1], border=cores[1])

xaux <- seq(ul2, lim[2], by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn2(xaux),0), col=cores[1], border=cores[1])



# distribuicao 1
lines(x,cn1(x),lwd=2, col=cores[2], type="l")

xaux <- seq(ll2, lim[2], by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn1(xaux),0), col=cores[2], border=cores[2])


# reforca as curvas
lines(x,cn2(x),lwd=2, col=cores[1], type="l")
lines(x,cn1(x),lwd=2, col=cores[2], type="l")


# forca do teste

xaux <- seq(lim[1], ll2, by = 0.01)
polygon(c(min(xaux), xaux, max(xaux)), c(0, cn1(xaux),0), col=scales::alpha("orange",.7), border=cores[2])


lines(x,cn1(x),lwd=2, col=cores[2], type="l")



# x <- seq(-2, 0, by = 0.01)

# polygon(c(-2,x,0), c(0,cn1(x),0), col=cores[2], border=cores[2])

# lines(x,cn1(x),ylab="Densidade",xlab="x",
#      main="Curva Normal",type="l",lwd=2)

