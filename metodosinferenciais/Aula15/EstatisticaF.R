# Estatísitica F
nsim <- 100000
n1 <- 40
n2 <- 30
Fs <- vector(mode = "numeric", length = nsim) # vector of sample averages
for(i in 1:nsim){
  s1 <- sd(rnorm(n1, mean = 0, sd = 1))^2
  s2 <- sd(rnorm(n2, mean = 3, sd = 1))^2
  Fs[i] <- s1/s2
}
# Distribuição F
hist(Fs, probability =TRUE, breaks=200, xlim=c(0,3), xlab="F", 
     main="Distribuição F")
curve(df(x,n1-1,n2-1),0,3,add=T,col="red",lwd=2)
text(2,1,paste0("gl1 = ",n1-1))
text(2,0.8,paste0("gl2 = ",n2-1))

distF<-df(x,n1-1,n2-1)

# curva sem histograma
dev.off()
curve(df(x,n1-1,n2-1),0,3,col="red",lwd=2,
      ylab = "Density", xlab = "F", main="Distribuição F")
lines(c(0,4),c(0,0))
text(2,1,paste0("gl1 = ",n1-1))
text(2,0.8,paste0("gl2 = ",n2-1))

