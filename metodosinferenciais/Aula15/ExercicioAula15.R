# Considere um modelo composto por uma unica variavel 
# independente categorica com tres categorias, representando
# os tres partidos supostamente mais influentes na 
# Camara dos Deputados: MDB, PSDB e PT. 
# A variavel dependente consiste no tempo em minutos de
# exposicao na midia, nos últimos 100 dias, 
# dos respectivos lideres concedendo entrevistas.

set.seed(1456728)
g1 <- rnorm(100,5,2) # MDB
g2 <- rnorm(100,15,2) # PSDB
g3 <- rnorm(100,5,2) # PT

m1 <- mean(g1); sd1 <- sd(g1)
m2 <- mean(g2); sd2 <- sd(g2)
m3 <- mean(g3); sd3 <- sd(g3)
k <- 3 # quantidade de fatores ou grupos

n1 <- length(g1)
n2 <- length(g2)
n3 <- length(g3)
nT <- n1 + n2 + n3 # quantidade total de observacoes

g <- c(g1,g2,g3) # grupao

mg <- mean(g) # media grupao

SST <- sum((g - mg)^2)
SSM <- n1*(m1-mg)^2 + n2*(m2-mg)^2 + n3*(m3-mg)^2
SSR <- sum((g1-m1)^2) + sum((g2-m2)^2) + sum((g3-m3)^2)
#ou
SST = SSM + SSR
glM <- k - 1 # graus de liberdade do modelo
glR <- nT - k # graus de liberdade do residuo
MSM <- SSM / glM # media quadratica do modelo
MSR <- SSR / glR # media quadrAtica do residuo
Fs <- MSM / MSR # estatistica F

p.value <- 1 - pf(Fs, glM, glR)

sprintf("F = %7.5f p.vaule = %7.5f", Fs, p.value)


# 
dados <- read.csv2("tempopartido.csv")

# exploracao dos dados
boxplot(tp ~ partido, data=dados)

# ANOVA
modelo <- aov(tp ~ partido, data=dados)
summary(modelo)

# post hoc
# pairwise.t.test(outcome, predictor, paired = FALSE,
#                 p.adjust.method = "method=c("bonferroni", "BH")"
pairwise.t.test(dados$tp, dados$partido, paired = FALSE, 
                p.adjust.method = "bonferroni")

pairwise.t.test(dados$tp, dados$partido, paired = FALSE, 
                p.adjust.method = "BH")

# newModel<-glht(aov.Model, linfct = mcp(predictor = "method"), base = x)
library(multcomp)
newModel<-glht(modelo, linfct = mcp(partido = "Tukey"), base = dados)
summary(newModel)

# ANOVA e Regressao Multipla
s <- summary( lm(tp ~ partido, data=dados) )
s




