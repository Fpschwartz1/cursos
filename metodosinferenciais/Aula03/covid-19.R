library(scales)
set.seed(12345)

n1 <- rnorm(10000, mean = 20, sd = 3) 

hist(n1,freq=FALSE, border=FALSE, breaks=20, 
     col=alpha("red", 0.2),
     xlim = c(10, 65),
     ylim = c(0, 0.15),
     main="COVID-19", 
     xlab="dias",
     ylab="Taxa de infecção")
lines(density(n1),lwd=3,col="red")

n2 <-  rnorm(10000, mean = 45, sd = 7) 
hist(n2,freq=FALSE, border=FALSE, breaks=38, main=NULL, 
     col=alpha("blue", 0.4), add=T)
lines(density(n2),lwd=3,col="blue")

legend(40,0.15,legend=c("sem isolamento", 
                "isolamento horizontal"),
       fill = c(alpha("red", 0.3), alpha("blue", 0.4)))




