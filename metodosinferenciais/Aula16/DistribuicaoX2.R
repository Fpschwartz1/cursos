# Estatistica X2
k <- 3 # classes
gl <- k - 1 # graus de liberdade
p <- rep(1/k,k) # vetor de probabilidades para classes equiprováveis
n <- 160 # quantidade de observacoes
E <- p * n # frequencia esperada para cada classe
rept <- 10000 # numero de repeticoes
X2 <- vector("double", rept) # vetor de estatisticas qui-quadrdado
for(i in 1:rept){
  # dados observados
  s <- sample(1:k, n, replace=TRUE)
  # frequencia das classes observadas
  O <- matrix(table(s))
  O <- O[1:length(O)]
  # qui-quadrado
  X2i <- sum(((O-E)^2)/E)
  X2[i] <- X2i
}
# Distribuicao X2
X2 <- sort(X2)
hist(X2, probability =TRUE, breaks=30, 
     ylim=c(0,1), xlim=c(0,10),
     xlab=expression(paste("X"^2)),
     main=expression(paste("Distribuição X"^2)))
lines(X2, dchisq(X2, df=gl), col="red", lwd=3)


#####################################
# Distribuicao para varias categorias
#####################################
library(RColorBrewer)
n <- 100
k <- 2:5
cor <- brewer.pal(n = length(k), name = 'Dark2')
for(i in k) {
  X2 <- sort(rchisq(n, df=i-1))
  if(i == min(k)){
    plot(X2, dchisq(X2, df=i-1), lwd=2, type="l", ylim=c(0, 1),
         ylab="Densidade", xlab=expression(paste("X"^2))
         ,main=expression(paste("Distribuição X"^2))
         )
    grid()
  } else{
    lines(X2, dchisq(X2, df=i-1), col=cor[i-1], lwd=2)
  }
}
legend("topright", legend=paste0("k = ",min(k):max(k)),
       col=cor, lty=1, lwd=2, bty="n")


#####################################
# Exemplo PT, PSDB e MDB
#####################################
k = 3
# Experimento 1
X2 <- sum((c(2, 20, 5)-9)^2)/9
1 - pchisq(X2, k-1)
pchisq(X2, k-1, lower.tail = FALSE)


# Experimento 2
X2 <- sum((c(8, 10, 9)-9)^2)/9
1 - pchisq(X2, k-1)


#####################################
# Tabela de Contingencia
#####################################
library(RColorBrewer)
n <- 100
k <- 2
cor <- brewer.pal(n = length(k), name = 'Dark2')
for(i in k) {
  X2 <- sort(rchisq(n, df=i-1))
  if(i == min(k)){
    plot(X2, dchisq(X2, df=i-1), lwd=2, type="l", ylim=c(0, 1),
         ylab="Densidade", xlab=expression(paste("X"^2))
         ,main=expression(paste("Distribuição X"^2))
    )
    grid()
  } else{
    lines(X2, dchisq(X2, df=i-1), col=cor[i-1], lwd=2)
  }
}
legend("topright", legend=paste0("k = ",min(k):max(k)),
       col=cor, lty=1, lwd=2, bty="n")

q <- qchisq(.95,k)
lines(c(-1,11),c(0,0), lty=3)
lines(c(0,0),c(-1,2), lty=3)
lines(c(q,q),c(-1,0.1),col="red")
text(q,0.12,round(q,2), col="red", pos = 3)

lines(c(1.74,1.74),c(-1,dchisq(1.74,k)),col="blue")
text(1.74,dchisq(1.74,k),"1.74", col="blue", pos = 3)
