dnormal <- function(media=0, dp=1, x1=0, x2=NULL)
{


  ##################################################
  # Lab 3.2.2
  ##################################################
    
  ########################
  # Script principal
  ########################
  
  # eixo x da curva normal padronizada
  lim <- media+c(-4,4)*dp
  x <- seq(lim[1], lim[2], by = 0.01)
  
  # curva normal
  cnp <- function(x) {dnorm(x,media,dp)} # curva normal
  
  # traça a curva normal
  plot(x,cnp(x),ylab="Densidade",xlab="x",
       main="Curva Normal",type="l",lwd=2)
  # linha horzontal em zero
  lines(lim,c(0,0))
  
  # valores de x
  if(!is.null(x1)) { 
    text(x1,-0.02,paste("x1=",round(x1,2)),cex=0.7,font=4) 
    lines(c(x1,x1),c(0,cnp(x1)),lty=4,type="l")
  }
  if(!is.null(x2)) { 
    text(x2,-0.02,paste("x2=",round(x2,2)),cex=0.7,font=4) 
    lines(c(x2,x2),c(0,cnp(x2)),lty=4,type="l")
  }
  
  # probabilidades
  integral <- function(f,a,b) {i<-integrate(f,a,b); as.numeric(i[1])}
  
  # hachura da área depois de x1
  if(!is.null(x1)) {
    if(is.null(x2)) {
      x2 <- lim[2]
    }
    else {
      text(x2,-dnorm(media,media,dp)/70,paste("x2=",x2),cex=0.7)
      text(lim[1],cnp(media)*.85,paste("x2 =",x2),cex=0.8,pos=4)
    }
    inc <- (x2-x1)/20
    i<-x1+inc
    while(i < x2){
      lines(c(i,i),c(0,cnp(i)),col="gray",lwd=0.5)
      i<-i+inc
    }
    phachura<-round(integral(cnp,x1,x2),4)
    text(lim[1],cnp(media),paste("Área hachurada =",phachura), cex=0.8, pos=4)
    text(lim[1],cnp(media)*.95,paste("Área branca =",1-phachura), cex=0.8, pos=4)
    text(lim[1],cnp(media)*.90,paste("x1 =",x1),cex=0.8,pos=4)
    text(x1,-dnorm(media,media,dp)/70,paste("x1=",x1),cex=0.7)
  }

}
