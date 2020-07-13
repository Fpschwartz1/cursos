
dnormalpIC <- function(p=95)
{

      ##################################################
      # Lab 3.2.1
      ##################################################
      media <- 0
      dp <- 1
      p <- (1-(p/100))/2
      
      x1 <- qnorm(p)
      x2 <- -x1
      
      ########################
      # Script principal
      ########################
      if(!is.null(x1)) { 
        z1<-(x1-media)/dp 
        if(!is.null(x2) ) { 
          z2<-(x2-media)/dp 
          if(z1 > z2) {aux<-z1; z1<-z2; z2<-aux; aux<-x1; x1<-x2; x2<-aux}
        }
        else z2 <- NULL
      }
      
      # eixo x da curva normal padronizada
      lim <- c(-4,4)
      x <- seq(lim[1], lim[2], by = 0.01)
      
      # traça a curva normal padronizada
      plot(x,dnorm(x,0,1),ylab="Densidade",xlab="x",
           main="Curva normal padronizada",type="l",lwd=2,ylim=c(-0.05,0.4))
      # linha horzontal em zero
      lines(c(lim[1],lim[2]),c(0,0))
      
      # curva normal padronizada
      cnp <- function(x) {dnorm(x,0,1)} # curva normal padronizada
      
      # valores de z
      if(!is.null(z1)) { 
        text(z1,-0.02,paste("z1=",round(z1,2)),cex=0.7,font=4) 
        lines(c(z1,z1),c(0,cnp(z1)),lty=4,type="l")
      }
      if(!is.null(z2)) { 
        text(z2,-0.02,paste("z2=",round(z2,2)),cex=0.7,font=4) 
        lines(c(z2,z2),c(0,cnp(z2)),lty=4,type="l")
      }
      
      # probabilidades
      integral <- function(f,a,b) {i<-integrate(f,a,b); as.numeric(i[1])}
      
      # hachura da área
      if(!is.null(z1)) {
        if(is.null(z2)) {
          z2 <- lim[2]
        }
        else {
          text(-4.3,0.29,paste("x2 =",x2),cex=0.8,pos=4)
        }
        inc <- (z2-z1)/20
        i<-z1+inc
        while(i < z2){
          lines(c(i,i),c(0,cnp(i)),col="red",lwd=0.5)
          i<-i+inc
        }
        phachura<-round(integral(cnp,z1,z2),4)
        text(-4.3,0.38,paste("Área hachurada =",phachura),cex=0.8,pos=4)
        text(-4.3,0.35,paste("Área branca =",1-phachura),cex=0.8,pos=4)
        text(-4.3,0.32,paste("x1 =",x1),cex=0.8,pos=4)
      }

}      
