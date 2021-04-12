rm(list = ls(all = TRUE)) # apaga variaveis no Global Environment
source("NormalCompara.R") # carrega a funcao dnormalCopm()

# exercicio 1
# http://www.portalaction.com.br/en/node/1948
a1 <- c(
      18.8,17.591,20.835,19.169,18.755,20.504,
      18.756,17.527,19.29,19.203,18.621,18.977,17.078,22.059,18.419,
      19.919,20.308,17.62,18.585,20.764,21.117,18.899,21.426,17.89,21.055
)

a2 <- c(
      22.284,22.057,22.629,24.62,21.491,21.198,
      21.901,22.881,22.86,22.058,22.699,22.909,
      25.302,17.968,24.515,23.15,24.662,23.327,
      22.447,23.382,22.426,22.787,21.983,24.534,
      22.771,21.043,21.203,24.009,21.917,21.152
)

dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc=">")

# exercicio 2
# gera arquivo csv
# set.seed(837364)
# df <- data.frame(
#          a1 = rnorm(100,mean=45,sd=10),
#          a2 = rnorm(100,mean=48,sd=8),
#          a3 = rnorm(100,mean=47,sd=2)
#)
#write.csv(df,file="exercicio.csv",row.names = FALSE)

df <- read.csv("exercicio.csv",sep=",", header=TRUE)

a1 <- df$a1
a2 <- df$a2
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc="=")
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc=">",nc=.95) # nc 99%
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc="<")


a1 <- df$a2
a2 <- df$a3
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc="=")
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc=">")
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc="<")


a1 <- df$a1
a2 <- df$a3
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc="=") # for?a do teste
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc=">")
dnormalComp(media1=mean(a1),dp1=sd(a1)/sqrt(length(a1)),media2=mean(a2),dp2=sd(a2)/sqrt(length(a2)),rc="<")
