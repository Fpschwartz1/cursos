rm(list = ls(all = TRUE))

source("NormalCompara.R")

######################################
# Teste de hipotese para uma amostra
######################################

# verifique se as amostras abaixo pertencem aa populacao
# cujo status quo eh ve

ve <- 50

# 1
set.seed(764939)
a1 <- rnorm(30, 30, 20)
dnormalComp(ve, NULL, mean(a1), sd(a1)/sqrt(length(a1)))


# 2
set.seed(764939)
a2 <- rnorm(20, 60, 20)
dnormalComp(ve, NULL, mean(a2), sd(a2)/sqrt(length(a2)))
dnormalComp(ve,NULL,mean(a2),sd(a2)/sqrt(length(a2)), rc="<",
            nc=.9)

# 3
set.seed(764939)
a3 <- rnorm(60, 60, 20)
dnormalComp(ve, NULL, mean(a3), sd(a3)/sqrt(length(a3)), rc="<")
dnormalComp(ve, NULL, mean(a3), sd(a3)/sqrt(length(a3)), nc=.99)


# 4
set.seed(764939)
a4 <- rnorm(100, 40, 110)
dnormalComp(ve,NULL,mean(a4),sd(a4)/sqrt(length(a4)), rc=">")



######################################
# Teste de hipotese para duas amostras
######################################

# verifique se as amostras abaixo pertencem aa mesma populacao

# 1
set.seed(764939)
a1 <- rnorm(25,19,2.5)
a2 <- rnorm(30,22,2)


dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)))

# 2
set.seed(764939)
a1 <- rnorm(25,20,2.5)
a2 <- rnorm(30,22,2)


dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)))

dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)), rc=">")

dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)), rc="<")

# 3
set.seed(764939)
a1 <- rnorm(30,21,2.7)
a2 <- rnorm(80,22,5)


dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)))

dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)), rc=">")

dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)), rc="<")


# 4 - forca do teste
set.seed(764939)
a1 <- rnorm(50,6,3)
a2 <- rnorm(30,8,4)

dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)))

dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)), ft=1)

dnormalComp(mean(a1), sd(a1)/sqrt(length(a1)), 
            mean(a2), sd(a2)/sqrt(length(a2)), ft=2)

