library(manipulate); library(UsingR); data(galton)
myHist <- function(mu){
  hist(galton$child,col="blue",breaks=20)
  lines(c(mu, mu), c(0, 150),col="red",lwd=5)
  mse <- mean((galton$child - mu)^2)
  text(63, 150, paste("mu = ", mu))
  text(63, 140, paste("SEQ = ", round(mse, 2)))
}
manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))
mean(galton$child)