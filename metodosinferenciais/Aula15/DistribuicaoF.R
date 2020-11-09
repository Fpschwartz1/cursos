################
# Distribuicao F
################
gl1 <- 1
gl2 <- 4
curve(df(x,gl1,gl2),0,8,col="blue",lwd=2,
      xlab="F", ylab = "Density", main = "Distribuicao F")
text(3,1.2,paste0("glM = ",gl1))
text(3,1.0,paste0("glR = ",gl2))

lines(c(-1,9),c(0,0))
lines(c(0,0),c(-1,2))

q95 <- qf(.95,gl1,gl2)
lines(c(q95,q95),c(-1,0.1), col="red")
text(q95,0.18,round(q95,2), col="red")
