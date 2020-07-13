##################################################
# Simulacao
##################################################
# remove todas as vari?veis da mem?ria
rm(list = ls(all = TRUE)) 
# leitura do arquivo
tse <- read.table("consulta_cand_2012_PR.txt",sep=";") # com fatores
# renomeia a vari?vel V26
names(tse)[26] <- "DATA_NASCIMENTO"
# converte o campo DATA_NASCIMENTO para o formato data
dtnasc <- as.Date(tse$DATA_NASCIMENTO, format="%d/%m/%Y")
# determina a idade dos candidatos na data da elei??o de 2012 (7/10/2012)
dteleicao <- as.Date("7/10/2012", format="%d/%m/%Y")
idade2012 <- difftime(dteleicao,dtnasc,units="days")
# converte o resultado para a idade em anos
idade2012 <- floor(as.numeric(idade2012)/365.25)
idade2012 <- idade2012[idade2012 < 150]

hist(idade2012, breaks=20, main="Candidatos do Paran?",
     xlab="Idade",ylab="Frequ?ncia", col="orange")

length(idade2012)

media <- mean(idade2012)
dp    <- sd(idade2012)

n <- 30 # tamanho da amostra
SE <- dp/sqrt(n)
nsim <- 100
X  <- vector(mode = "numeric", length = nsim) # vector of sample averages
S  <- vector(mode = "numeric", length = nsim) # vector containing the standard deviation of each sample
sn <- vector(mode = "numeric", length = nsim) # vector of normalized averages
for(i in 1:nsim){
  idade_i <- sample(idade2012, n, replace = TRUE)
  xi    <- mean(idade_i)
  X[i]  <- xi
  S[i]  <- sd(idade_i)
  sn[i] <- (xi-media)/SE
}

hist(X)

mmed <- mean(X)
mmed
media

ep <- sd(X)
ep
SE

# intervalo de confianca para 95%
x1 <- mmed - 1.96 * ep
x2 <- mmed + 1.96 * ep
sprintf("IC: %f  %f", x1,x2)
mmed

# intervalo de confianca para 98%
x1 <- mmed - 2.32 * ep
x2 <- mmed + 2.32 * ep
sprintf("IC: %f  %f", x1,x2)
mmed


library(ggplot2)
dsn <- density(sn)
k <- length(dsn[[1]])
df <- data.frame(y = c(dnorm(dsn[[1]]), dsn[[2]]),
                 x = dsn[[1]],
                 dist = factor(rep(c("Standard Normal", "Normalized Averages"), c(k,k))))
g <- ggplot(df, aes(x = x, y = y)) 
g <- g + geom_line(size = 1.5, aes(colour = dist))
g <- g + ylab("density")
g

# cobertura do IC relativa ao tamanho da amostra
coverage <- vector(mode = "logical", length = nsim)
for(i in 1:nsim){
  ll <- X[i] - 1.96 * S[i]/sqrt(n)
  ul <- X[i] + 1.96 * S[i]/sqrt(n)
  coverage[i] <- ll < media & ul > media
}

sum(coverage)

mean(coverage)
