library(UsingR); data(galton)

# 1 metro = 39,37008 polegadas
x <- round(galton$parent / 39.37008, 2)
y <- round(galton$child / 39.37008, 2)

mean(x)
mean(y)

# sem bolhas
par(cex.axis=0.8, cex.main=0.8)
plot(x, y, pch=19, cex=0.15, col="blue",
     xlab="Pais", ylab="Filhos", main="(a)")

# com bolhas
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("parent","child","freq")
plot(as.numeric(as.vector(freqData$parent)),
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .08 * freqData$freq, xlab = "Pais", ylab = "Filhos", main="(b)")

points(mean(x),mean(y),col="red",pch=19)

# centralizacao - com bolhas
par(cex.axis=0.8, cex.main=1.1)
x <- galton$parent - mean(galton$parent)
y <- galton$child - mean(galton$child)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("parent","child","freq")
plot(as.numeric(as.vector(freqData$parent)),
     as.numeric(as.vector(freqData$child)),
     main = "Centralização",
     pch = 21, col = "black", bg = "lightblue",
     cex = .15 * freqData$freq, xlab = "Pais", ylab = "Filhos")
points(mean(x),mean(x),col="red",pch=19)



