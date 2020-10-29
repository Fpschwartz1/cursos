
dnormalComp <- function(media1=0, dp1=1, media2=0, dp2=1, nc=.95, rc="=")
{

  ########################
  # Script principal
  ########################
  
  cores=c("#7FFFD4", "#ADD8E6", '#FF6347')
  
  # eixo x da curva normal
  lim <- c(
           min(c(media1+c(-4,4)*dp1, media2+c(-4,4)*dp2)), 
           max(c(media1+c(-4,4)*dp1, media2+c(-4,4)*dp2))
         )
  x <- seq(lim[1], lim[2], by = 0.01)
  
  # curva normal
  cn1 <- function(x) {dnorm(x,media1,dp1)} # curva normal
  cn2 <- function(x) {dnorm(x,media2,dp2)} # curva normal
  
  # traca as curvas normais 1 e 2
  if(cn1(media1)>=cn2(media2)){
    plot(x,cn1(x),ylab="Densidade",xlab="x",
         main="Compara Amostras",type="l",lwd=2, col=cores[1])
  } else {
    plot(x,cn2(x),ylab="Densidade",xlab="x",
         main="Compara Amostras",type="l",lwd=2, col=cores[2])
  }
 
  # intervalos de confianca
  if(rc=="="){
    xI11 <- media1 - qnorm(nc+(1-nc)/2)*dp1
    xI12 <- media1 + qnorm(nc+(1-nc)/2)*dp1
    xI21 <- media2 - qnorm(nc+(1-nc)/2)*dp2
    xI22 <- media2 + qnorm(nc+(1-nc)/2)*dp2
  } else if(rc=="<"){
    xI11 <- media1 - 4*dp1
    xI12 <- media1 + qnorm(1-nc)*dp1
    xI21 <- media2 - 4*dp2
    xI22 <- media2 + qnorm(1-nc)*dp2  
    cores[1] <- cores[2] <- cores[3]
  } else if(rc==">"){
    xI11 <- media1 + qnorm(nc)*dp1
    xI12 <- media1 + 4*dp1
    xI21 <- media2 + qnorm(nc)*dp2
    xI22 <- media2 + 4*dp2    
    cores[1] <- cores[2] <- cores[3]
  }
    
  if(rc %in% c("=",">")){
    ic <- seq(xI11, xI12, by = 0.01)
    polygon(c(min(ic), ic, max(ic)), c(0, cn1(ic),0), col=scales::alpha(col=cores[1],.7))
  }
  
  if(rc %in% c("=","<")){
    ic <- seq(xI21, xI22, by = 0.01)
    polygon(c(min(ic), ic, max(ic)), c(0, cn2(ic),0), col=scales::alpha(col=cores[2],.7))
  }
  
  # linha horizontal em zero
  lines(lim,c(0,0))
  
  # linhas da media
  lines(c(media1,media1),c(-1,cn1(media1)),lwd=2,type="l")
  lines(c(media2,media2),c(-1,cn2(media2)),lwd=2,type="l")

  # linha das curvas
  lines(x,cn2(x),lwd=2)
  lines(x,cn1(x),lwd=2)
}      