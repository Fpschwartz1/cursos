x <- seq(-6,6,0.1)
sigmoide <- function(z){
  1/(1+exp(-z))
}
plot(c(-7,-6,-3,0,3,6), c(0,0.2,0.4,0.6,0.8,1.0),
     type= "n", main="Sigmoide", ylab="g(x)",
     xlab=expression(paste(mu,"(x)=x")))
lines(x,sigmoide(x))
lines(c(-6,6), c(0,0), lty=2)
lines(c(-6,6), c(1,1), lty=2)
text(-7.5, 0, "Y = 0", pos = 4)
text(-7.5, 1, "Y = 1", pos = 4)

lines(c(0,0),c(0,sigmoide(0)), lty=3)
lines(c(-15,0),c(sigmoide(0),sigmoide(0)), lty=3)
