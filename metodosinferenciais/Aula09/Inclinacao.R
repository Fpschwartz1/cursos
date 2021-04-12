library(UsingR); data(galton); library(manipulate)
myPlot <- function(beta){ 
x <- round(galton$parent / 39.37008, 2)
y <- round(galton$child / 39.37008, 2)

y <- y - mean(y)
x <- x - mean(x)
freqData <- as.data.frame(table(x, y))
names(freqData) <- c("parent", "child", "freq")
plot(as.numeric(as.vector(freqData$parent)),
     as.numeric(as.vector(freqData$child)),
     pch = 21, col = "black", bg = "lightblue",
     cex = .15 * freqData$freq, xlab = "parent", ylab = "child")
abline(0, beta, lwd = 3)
points(0, 0, cex = 2, pch = 19)
mse <- mean( (y - beta * x)^2 )
title(paste("beta = ", beta, "SEQ = ", round(mse, 9)))}
manipulate(myPlot(beta), beta = slider(0.6, 0.62, step = 0.002))

lm(y~x)
