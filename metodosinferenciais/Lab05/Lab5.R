###########################################################
## Simulacao

###########################################################
# Lab 3a - Comparacao entre normal padrao e curva t
###########################################################
library(ggplot2)
library(manipulate)

k <- 1000
xvals <- seq(-5, 5, length = k)
myplot <- function(df){
  d <- data.frame(y = c(dnorm(xvals), dt(xvals, df)),
                  x = xvals,
                  dist = factor(rep(c("Normal", "T"), c(k,k))))
  g <- ggplot(d, aes(x = x, y = y)) 
  g <- g + geom_line(size = 2, aes(colour = dist))
  g
}
manipulate(myplot(mu), mu = slider(1, 20, step = 1))


###########################################################
# Lab 3b
###########################################################
library(ggplot2)
library(manipulate)
pvals <- seq(.01, .99, by = .01)
myplot2 <- function(df){
  d <- data.frame(n= qnorm(pvals),t=qt(pvals, df)
                  ,p = pvals)
  g <- ggplot(d, aes(x= n, y = t))
  g <- g + geom_abline(size = 2, col = "lightblue")
  g <- g + geom_line(size = 2, col = "black")
  g <- g + geom_vline(xintercept = qnorm(0.975))
  g <- g + geom_hline(yintercept = qt(0.975, df))
  g
}


#################################################################
## Comparacao entre a media da populacao e a media de uma amostra
# Temperaturas corporais medidas
x<-c(30.5,35.3,33.2,40.8,42.3,41.5,36.3,43.2,34.6,38.5)

# One Sample t-test
t.test(x,     #amostra a ser testada
       mu=35, #hipotese de nulidade
       alternative="greater") #teste unilateral pela direita

# intervalo de confianca para gl = 9
ic<-mean(x)-1.833*sd(x)/sqrt(length(x))
ic



#######################################################
## Comparacao entre as medias de duas amostras independentes
x<-c(30.5,35.3,33.2,40.8,42.3,41.5,36.3,43.2,34.6,38.5)
y<-c(28.2,35.1,33.2,35.6,40.2,37.4,34.2,42.1,30.5,38.4)

#  Welch Two Sample t-test
t.test(x,y,               #amostras a serem testadas
       conf.level = 0.99) #n?vel de confian?a



#######################################################
## Comparacao entre as medias de duas amostras pareadas
x<-c(30.5,35.3,33.2,40.8,42.3,41.5,36.3,43.2,34.6,38.5)
y<-c(28.2,35.1,33.2,35.6,40.2,37.4,34.2,42.1,30.5,38.4)

# Paired t-test
t.test(x,y,             #amostras a serem testadas
       conf.level=0.99, #nivel de confianca
       paired=T)        #indica dependencia entre as amostras

