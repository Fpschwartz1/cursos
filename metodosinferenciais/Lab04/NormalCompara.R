
dnormalComp <- function(media1=0, ep1=1, media2=0, ep2=1, nc=.95, rc="=", ft=NULL)
{
  # cores das curvas 
  cores=c("red", "blue")
  
  # verificacao dos parametros
  if(media1 > media2){
    # troca medias
    aux <- media1; media1 <- media2; media2 <- aux
    # troca erros padrao
    aux <- ep1; ep1 <- ep2; ep2 <- aux
    
    if(!is.null(ft)){
      if(ft==1) { 
        ft <- 2
      } else ft <- 1
    }
  }

  if(is.null(ep1) & is.null(ep2)) return("Erros padrao nulos simultaneamente")
  ep1_i <- ep1; ep2_i <- ep2
    
  # verifica se o teste eh de uma ou duas amostras
  if(is.null(ep1) | is.null(ep2)) {
    titulo <- "Teste de hipotese para uma amostra"
    ft <- NULL # nao calcula forca do teste
  
    if(is.null(ep1)) ep1_i <- 1
    if(is.null(ep2)) ep2_i <- 1
    
  } else titulo <- "Teste de hipotese para duas amostras"
  
  # eixo x da curva normal
  lim <- c(
    min(c(media1+c(-4,4)*ep1_i, media2+c(-4,4)*ep2_i)), 
    max(c(media1+c(-4,4)*ep1_i, media2+c(-4,4)*ep2_i))
  )

  x <- seq(lim[1], lim[2], by = 0.01)
  
  # curva normal
  cn1 <- function(x) {dnorm(x,media1,ep1_i)} # curva normal 1
  cn2 <- function(x) {dnorm(x,media2,ep2_i)} # curva normal 2
  
  # traca as curvas normais 1 e 2
  if((cn1(media1)>=cn2(media2) & !is.null(ep1)) | is.null(ep2)){
    plot(x,cn1(x),ylab="Densidade",xlab="x",
         main=titulo,type="l",lwd=1.5, col=cores[1])
  } else {
    plot(x,cn2(x),ylab="Densidade",xlab="x",
         main=titulo,type="l",lwd=1.5, col=cores[2])
  }
 
  # intervalos de confianca
  if(rc=="="){
    xI11 <- media1 - qnorm(nc+(1-nc)/2)*ep1_i
    xI12 <- media1 + qnorm(nc+(1-nc)/2)*ep1_i
    xI21 <- media2 - qnorm(nc+(1-nc)/2)*ep2_i
    xI22 <- media2 + qnorm(nc+(1-nc)/2)*ep2_i
  } else if(rc=="<"){
    xI21 <- media2 + qnorm(1-nc)*ep2_i
    xI22 <- lim[2]
  } else if(rc==">"){
    xI11 <- lim[1]
    xI12 <- media1 + qnorm(nc)*ep1_i
  }
    
  if((rc %in% c("=",">")) & !is.null(ep1)){
    ic <- seq(xI11, xI12, by = 0.01)
    polygon(c(min(ic), ic, max(ic)), c(0, cn1(ic),0), col=scales::alpha(col=cores[1],.4), border = cores[1])
  }
  
  if((rc %in% c("=","<")) & !is.null(ep2)){
    ic <- seq(xI21, xI22, by = 0.01)
    polygon(c(min(ic), ic, max(ic)), c(0, cn2(ic),0), col=scales::alpha(col=cores[2],.4), border = cores[2])
  }
  
  # linhas da media
  lines(c(media1,media1),c(-1,cn1(media1)),lwd=1.5, col = cores[1])
  lines(c(media2,media2),c(-1,cn2(media2)),lwd=1.5, col = cores[2])

  # linha das curvas
  if(!is.null(ep2)) lines(x,cn2(x),lwd=2, col = cores[2])
  if(!is.null(ep1)) lines(x,cn1(x),lwd=2, col = cores[1])
  
  # linha horizontal em zero
  lines(lim,c(0,0))
  
  # forca do teste
  if(!is.null(ft)){
    integral <- function(f,a,b) {i<-integrate(f,a,b); as.numeric(i[1])}
    
    if(ft == 1){
      xaux <- seq(xI21, xI22, by = 0.01)
      polygon(c(min(xaux), xaux, max(xaux)), c(0, cn1(xaux),0), col="orange", border="orange")
      beta <- integral(cn1,xI21,xI22)
    }
    if(ft == 2){
      xaux <- seq(xI11, xI12, by = 0.01)
      polygon(c(min(xaux), xaux, max(xaux)), c(0, cn2(xaux),0), col="orange", border="orange")
      beta <- integral(cn2,xI11,xI12)
    }
    
    return(paste0("Forca do teste: ", 1-beta, "  beta: ", beta))
  }
  
}      
