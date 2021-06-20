teste.t <- function(x, y = NULL, alternative = "two.sided",
                  mu = 0, paired = FALSE, var.equal = FALSE,
                  conf.level = 0.95, dif.media = TRUE){
  
  cor = scales::alpha(col="blue",.2)
  
  inc <- 0.01 # incremento de seq
  
  alpha <- 1 - conf.level
  
  if(is.null(y)) { # Status quo
    # preparacao
    media1 <- mean(x)
    n1 <- length(x)
    gl <- n1-1
    s1 <- sd(x)
    EP <- s1/sqrt(n1)
    
    t <- (media1-mu)/EP
    
    media2 <- mu
    
    # titulo
    e1 <- "status quo"
    # expressao para mostrar medias
    e2 <- bquote(bar(x)==~.(format(media1,digits=4))~~~~mu[0]==~.(format(media2,digits=4)))
    # erro padrao
    e3 <- bquote(EP==frac(s,sqrt(n))*''%~~%''*.(format(EP,digits=4)))
    # graus de liberdade
    e4 <- bquote(t==frac(bar(x)-mu[0],EP)*''%~~%''*.(format(t,digits=3)))

  } else if(paired){ # grupos pareados
    if(length(x) != length(y)) return("Comprimentos diferentes de x e y.")

    # preparacao
    media1 <- mean(x)
    s1 <- sd(x)
    media2 <- mean(y)
    s2 <- sd(y)
    dif <- x-y
    n <- length(dif)
    gl <- n-1
    EP <- sd(dif)/sqrt(n)
    
    t <- mean(dif)/EP
    
    # ajusta valores da media1 para calculo dos ICs
    if(dif.media){
      media1 <- media1-media2
      media2 <- 0
      s1 <- sd(dif)
    }
    
    # titulo
    e1 <- "grupos pareados"
    # expressao para mostrar medias
    if(dif.media){
      e2 <- bquote(bar(x)[dif]==~.(format(media1,digits=4)))
    } else {
      e2 <- bquote(bar(x)[1]==~.(format(media1,digits=4))~~~~bar(x)[2]==~.(format(media2,digits=4)))
    }
      
    # erro padrao
    e3 <- bquote(EP==frac(s[dif],sqrt(n))*''%~~%''*.(format(EP,digits=4)))
    # graus de liberdade
    e4 <- bquote(t==frac(bar(x)[1]-bar(x)[2],EP)*''%~~%''*.(format(t,digits=3)))
    
    
  } else if(var.equal){ # grupos independentes - variancas iguais
    # preparacao
    media1 <- mean(x)
    s1 <- sd(x)
    n1 <- length(x)
    media2 <- mean(y)
    s2 <- sd(y)
    n2 <- length(y)
    gl <- n1+n2-2
    
    sp2 <- ((n1-1)*s1^2 + (n2-1)*s2^2) / gl
    EP <- sqrt(sp2)*sqrt((1/n1)+(1/n2))
    
    t <- (media1-media2)/EP
    
    # ajusta valores da media1 para calculo dos ICs
    if(dif.media){
      media1 <- media1-media2
      media2 <- 0
      s1 <- sqrt(sp2)
    }
    
    # titulo
    e1 <- "independentes - variancas iguais"
    # expressao para mostrar medias
    if(dif.media){
      e2 <- bquote(bar(x)[dif]==~.(format(media1,digits=4)))
    } else {
      e2 <- bquote(bar(x)[1]==~.(format(media1,digits=4))~~~~bar(x)[2]==~.(format(media2,digits=4)))
    }
    # erro padrao
    # e3 <- bquote(EP==frac(s[dif],sqrt(n))*''%~~%''*.(format(EP,digits=4)))
    e3 <- bquote(EP==sqrt(frac((n[1]-1)*s[1]^2+(n[2]-1)*s[2]^2,n[1]+n[2]-2))*
                 sqrt(frac(1,n[1])+frac(1,n[2]))*''%~~%''*.(format(EP,digits=3)) )
    # graus de liberdade
    e4 <- bquote(t==frac(bar(x)[1]-bar(x)[2],EP)*''%~~%''*.(format(t,digits=3)))
    
  } else if(!var.equal){ # grupos independentes - variancas diferentes
    
    # preparacao
    media1 <- mean(x)
    s1 <- sd(x)
    n1 <- length(x)
    media2 <- mean(y)
    s2 <- sd(y)
    n2 <- length(y)
    
    # graus de liberdade
    gl <- (s1^2/n1 + s2^2/n2)^2 / ((s1^2/n1)^2/(n1-1) + (s2^2/n2)^2/(n2-1))
    EP <- sqrt( s1^2/n1 + s2^2/n2 )
    
    t <- (media1-media2)/EP
    
    # ajusta valores da media1 para calculo dos ICs
    if(dif.media){
      media1 <- media1-media2
      media2 <- 0
      s1 <- sd(rt(n1+n2, gl))
    }

    # titulo
    e1 <- "independentes - variancas diferentes"
    # expressao para mostrar medias
    if(dif.media){
      e2 <- bquote(bar(x)[dif]==~.(format(media1,digits=4)))
    } else {
      e2 <- bquote(bar(x)[1]==~.(format(media1,digits=4))~~~~bar(x)[2]==~.(format(media2,digits=4)))
    }
    # erro padrao
    # e3 <- bquote(EP==frac(s[dif],sqrt(n))*''%~~%''*.(format(EP,digits=4)))
    e3 <- bquote(EP==sqrt(frac(1,n[1])+frac(1,n[2]))*''%~~%''*.(format(EP,digits=3)) )
    # graus de liberdade
    e4 <- bquote(t==frac(bar(x)[1]-bar(x)[2],EP)*''%~~%''*.(format(t,digits=3)))
    
  }
  
  # lateralidade
  if(alternative == "two.sided"){
    ica <- media1+c(-1,1)*EP*qt(conf.level+alpha/2, gl) # ic amostra
    ict <- c(-1,1)*qt(conf.level+alpha/2, gl) # ic student
    # expressao da hipotese alternativa
    if(is.null(y)){ # se for status quo
      e <- expression(H[1]:bar(x) != mu[0])
    } else if(dif.media){
      e <- expression(H[1]:bar(x)[1] - bar(x)[2] != 0)
    } else e <- expression(H[1]:bar(x)[1] != bar(x)[2])
  } else if(alternative == "less"){
    ica <- c(-Inf, media1+EP*qt(conf.level, gl)) # ic amostra
    ict <- c(qt(alpha, gl), Inf) # ic student
    # expressao da hipotese alternativa
    if(is.null(y)){ # se for status quo
      e <- expression(H[1]:bar(x) < mu[0])
    } else if(dif.media){
      e <- expression(H[1]:bar(x)[1] - bar(x)[2] < 0)
    } else e <- expression(H[1]:bar(x)[1] < bar(x)[2])
  } else if(alternative == "greater"){
    ica <- c(media1+EP*qt(alpha, gl), Inf) # ic amostra
    ict <- c(-Inf, qt(conf.level, gl)) # ic student
    # expressao da hipotese alternativa
    if(is.null(y)){ # se for status quo
      e <- expression(H[1]:bar(x) > mu[0])
    } else if(dif.media){
      e <- expression(H[1]:bar(x)[1] - bar(x)[2] > 0)
    } else e <- expression(H[1]:bar(x)[1] > bar(x)[2])
  }

  par(mfrow=c(2,1))
  # dominio t
  if(t <= qt(.005,gl) | t >= qt(.995,gl)){
    lim <- seq(-abs(round(t,2)), abs(round(t,2)), by = inc)
  } else {
    lim <- seq(qt(.005,gl), qt(.995,gl), by = inc)
  }
  plot(lim, dt(lim,gl), type="l", lwd = 2, 
       ylab = "Densidade", xlab = "t", main = paste0("Dominio t (gl=",round(gl,2),") - ",e1))
  text(axis(1, labels = FALSE)[1], axis(2, labels = FALSE)[length(axis(2, labels = FALSE))-1], e, pos = 4)
  text(axis(1, labels = FALSE)[1], axis(2, labels = FALSE)[length(axis(2, labels = FALSE))-2], e2, pos = 4)
  text(axis(1, labels = FALSE)[length(axis(1, labels = FALSE))-1], axis(2, labels = FALSE)[length(axis(2, labels = FALSE))-1], e3, pos = 4)
  text(axis(1, labels = FALSE)[length(axis(1, labels = FALSE))-1], axis(2, labels = FALSE)[length(axis(2, labels = FALSE))-2], e4, pos = 4)
  # ic - dominio t
  if(alternative == "two.sided"){
    ic <- seq(ict[1], ict[2], by = inc)
  } else if (alternative == "less"){
    ic <- seq(ict[1], max(lim), by = inc)
  } else if (alternative == "greater"){
    ic <- seq(min(lim), ict[2], by = inc)
  }
  polygon(c(min(ic), ic, max(ic)), c(0, dt(ic, gl),0), col=cor, border=cor)
  lines(c(lim[1],-lim[1]), c(0,0))
  lines(c(ict[1],ict[1]), c(0,dt(ict[1], gl)))
  lines(c(ict[2],ict[2]), c(0,dt(ict[2], gl)))
  text(ict[1], dt(ict[1], gl), round(ict[1],2), pos = 2)
  text(ict[2], dt(ict[2], gl), round(ict[2],2), pos = 4)
  yh <- max(axis(2))/4 # altura do texto no eixo y 
  lines(c(t,t), c(-1, yh), col = "red", lwd = 2)
  text(t, yh, round(t,2), col = "red", pos = 3)

    
  # dominio x
  if(media2 <= media1-4*s1 | media2 >= media1+4*s1){
    lim <- seq(-abs(round(media2,2)), abs(round(media2,2)), by = inc)
  } else {
    lim <- seq(media1-4*s1, media1+4*s1, by = inc)
  }
  plot(lim, dnorm(lim,media1,s1), type="l", lwd = 2, 
       ylab = "Densidade", xlab = "x", main = paste0("Dominio x"),
       axes = FALSE)
  text(axis(1, labels = FALSE)[1], axis(2, labels = FALSE)[length(axis(2, labels = FALSE))-1], e, pos = 4)
  axis(1, c(round(axis(1, labels = FALSE),2), round(media1,2)))
  lines(c(lim[1],lim[length(lim)]), c(0,0)) # linha em y = 0
  lines(c(media1,media1), c(-1, dnorm(media1,media1,s1)), lwd = 1) # media
  box()
  # ic - dominio x
  if(alternative == "two.sided"){
    ic <- seq(ica[1], ica[2], by = inc)  
  } else if (alternative == "less"){
    ic <- seq(min(lim), ica[2], by = inc)
  } else if (alternative == "greater"){
    ic <- seq(ica[1], max(lim), by = inc)
  }
  polygon(c(min(ic), ic, max(ic)), c(0, dnorm(ic, media1,s1),0), col=cor, border=cor)
  lines(c(ica[1],ica[1]), c(0,dnorm(ica[1],media1,s1)))
  lines(c(ica[2],ica[2]), c(0,dnorm(ica[2],media1,s1)))
  text(ica[1], dnorm(ica[1], media1,s1), round(ica[1],2), pos = 2)
  text(ica[2], dnorm(ica[2], media1,s1), round(ica[2],2), pos = 4)
  yh <- max(axis(2))/4 # altura do texto no eixo y 
  lines(c(media2,media2), c(-1, yh), col = "red", lwd = 2)
  text(media2, yh, round(media2,2), col = "red", pos = 3)

  # retorna mfrow
  par(mfrow=c(1,1))
  

  t.test(x, y, alternative, mu, paired, var.equal, conf.level)
  
  
  # print("fim simulacao")
  #?plotmath
  #example(plotmath)
  #demo(plotmath)
}