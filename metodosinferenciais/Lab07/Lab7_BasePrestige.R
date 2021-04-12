library(car)
data(Prestige)

? Prestige
View(Prestige)
summary(Prestige)

###########################
# analise exploratoria
###########################

prestige <- Prestige
str(prestige)

# variaveis com valores nulos
colSums(is.na(prestige))
# linhas com valores nulos
prestige <- cbind(prestige, somaNulos = rowSums(is.na(prestige)))
prestige[prestige$somaNulos > 0, ]
# tratamento dos nulos
prestige[prestige$somaNulos > 0, "type"] <- "prof"
# prestige <- prestige[prestige$somaNulos == 0, ] 
# verificacao
colSums(is.na(prestige))
# retira somaNulos
prestige$somaNulos <- NULL
# diferenca entre tratar e nao tratar nulos
fit <- lm(prestige ~ . - census, data = Prestige)
summary(fit)
fit <- lm(prestige ~ . - census, data = prestige)
summary(fit)


# resumo
summary(prestige)
summary(prestige[prestige$type=="bc",])
summary(prestige[prestige$type=="prof",])
summary(prestige[prestige$type=="wc",])

boxplot(prestige ~ type, data=prestige)
boxplot(women ~ type, data=prestige)
boxplot(income ~ type, data=prestige)

# Correlacao
pairs(prestige ~ education + income + women + type,
      panel = panel.smooth,
      data = prestige, main = "Base 'Prestige'")

# corrplot
library(corrplot)
par(mfrow = c(2, 2))
corrplot(cor(prestige[,1:4]), method = "number", main="Todos os tipos")
corrplot(cor(prestige[prestige$type=="bc",1:4]), method = "number", main="bc")
corrplot(cor(prestige[prestige$type=="prof",1:4]), method = "number", main="prof")
corrplot(cor(prestige[prestige$type=="wc",1:4]), method = "number", main="wc")
par(mfrow = c(1, 1))


cor(prestige[, 1:4])
for(x in levels(prestige$type)){
  print(x)
  print(cor(prestige[prestige$type == x, 1:4]))
}


###########################
# escolha do modelo
###########################

# tudo menos census
fit <- lm(prestige ~ . - census, data = prestige)
summary(fit)

# comeco pelos significativos, iniciando com o mais significativo
fit1 <- lm(prestige ~ education, data = prestige)
summary(fit1)
fit2 <- lm(prestige ~ education + income, data = prestige)
summary(fit2)
# a adicao de novos regressores faz a SEQ decrescer monotonicamente
# mas a variacao eh significativa?
anova(fit1, fit2)
# a adicao de novo regressor aumenta o erro padrao dos coeficientes
# dos demais regressores
vif(fit2)

# variacao de women nao eh significativa
fit3 <- lm(prestige ~ education + income + women, data = prestige)
summary(fit3)
anova(fit1, fit2, fit3)
# a adicao de women aumenta a variancia e o erro padrao
vif(fit3)

# variacao de type eh significativa mas os coeficientes, nao.
fit4 <- lm(prestige ~ education + income + type, data = prestige)
summary(fit4)
anova(fit1, fit2, fit4)

# e type tem alto fator de inflacao e inflaciona education: nao compensa
vif(fit4) 
# alem disso, o R2 eh praticamente o mesmo

# interacoes
fit5 <- lm(prestige ~ education * income, data = prestige)
summary(fit5)
anova(fit2,fit5)

# grafico da interacao
plot.new()
plot.window(xlim=c(6,16), ylim=c(-20,90))
title("Interacao entre education e income",xlab="education", ylab="prestige")
education <- seq(6, 16, 0.1)
## como prestige varia com education quando income eh o valor minimo
income <- rep(min(prestige$income),length(education))
lines(education,predict(fit5,data.frame(education=education, income=income)),type="l",col="red",lwd="2")
## como prestige varia com education quando income eh o valor maximo
income <- rep(max(prestige$income),length(education))
lines(education,predict(fit5,data.frame(education=education, income=income)),type="l",col="blue",lwd="2")
## como prestige varia com education quando income eh o valor medio
income <- rep(mean(prestige$income),length(education))
lines(education,predict(fit5,data.frame(education=education, income=income)),type="l",col="green",lwd="2")
grid(); axis(1); axis(2)
legend(12, 40, c("min income", "mean income", "max income"), col = c("red", "green", "blue"), lwd=2)


# comparando o modelo sem interacao ...
fit2$coefficients
p2 <- predict(fit2,list(education=0,income=0),interval = "conf")
p2[3] - p2[2] # intervalo de confianca
# ... com o modelo com interacao
fit5$coefficients
p5 <- predict(fit5,list(education=0,income=0),interval = "conf")
p5[3] - p5[2] # intervalo de confianca: novo preditor aumenta o erro padrao

# o novo preditor em vif5 inflaciona o modelo ...
vif(fit5)
# quando comparando com vif2
vif(fit2)
# nao ha muita diferenca no R2 ...
summary(fit5)$adj.r.squared
summary(fit2)$adj.r.squared
# ... NAO VALE A PENA


# mais um teste,
# adicionar type aa interacao education * income:
# resulta em coeficientes nao significativos ...
fit6 <- lm(prestige ~ education * income + type, data = prestige)
summary(fit6)
# embora a reducao do RSS seja significativa ...
anova(fit2,fit6)
# .. porque caminha para overffiting ...
# que eh quando o modelo se ajusta muito bem ao conjunto de dados

# entao fechamos o modelo em fit2: prestige ~ education + income

# temos um bom modelo que explica o prestigio, mas,
# suponha que o estudo seja sobre perstigio em funcao do genero: fit2 nao eh util
# o contexto deve ser levado em conta: voltamos ao fit3 ...
summary(fit3)
vif(fit3)
# que alem de inflacionar a variancia tem coeficiente nao significativo


# proposta de dois modelos lineares
# retiramos income que tinha correlacao neg. significativa com women
cor.test(prestige$income, prestige$women)
fit7 <- lm(prestige ~ women + education, data = prestige)
summary(fit7)
par(mfrow = c(2, 2))
plot(fit7) 
# aparentemente, a premissa da normalidade do residuo nao eh atendida ...
# .,, atencao para newsboys


# acrescentamos type
fit8 <- lm(prestige ~ women + education + type, data = prestige)
summary(fit8)
plot(fit8)


anova(fit7,fit8)
# fit8 diminui a SEQ ...

vif(fit7)
vif(fit8)
# ... inflaciona a variancia ...

summary(fit7)$adj.r.squared
summary(fit8)$adj.r.squared
# ... e melhora um pouco o R2

# E se tirarmos education, R2 cai ...
fit9 <- lm(prestige ~ women + type, data = prestige)
summary(fit9)
plot(fit9)
# ... e perdemos potencial explicativo


#######################
# escolha: fit8
#######################
# considerando que existe alguma variacao quando type varia de bc para wc
# e que os residuos se aproximam de uma distribuicao normal quando a 
# variavel type eh adicionada

# na verdade, tem que testar premissas, nao basta ver
shapiro.test(fit7$residuals)
shapiro.test(fit8$residuals)


# e se tivessemos tomado outra decisao sobre os nulos
prestige.v2 <- Prestige
prestige.v2 <- cbind(prestige.v2, somaNulos = rowSums(is.na(prestige.v2)))
# tratamento dos nulos
# prestige.v2[prestige.v2$somaNulos > 0, "type"] <- "bc"
prestige <- prestige[prestige$somaNulos == 0, ] 
prestige.v2$somaNulos <- NULL
# acrescentamos type
fit8.v2 <- lm(prestige ~ women + education + type, data = prestige.v2)
summary(fit8.v2)
plot(fit8.v2)
shapiro.test(fit8.v2$residuals)



#######################
# stepwise
#######################
library(MASS)
prestige <- Prestige
fit <- lm(prestige ~ . - census, data = prestige)
stepw <- stepAIC(fit, direction="both", trace=FALSE)
summary(stepw)

stepw <- step(fit, direction="both", trace=FALSE)
summary(stepw)



#######################
# predicoes
#######################
fit8$coefficients

# modelo geral
x <- c(1, 0, 0, 0, 0) # intercepto
sum(fit8$coefficients*x)

# predicao pelo modelo geral
x[1] <- 1   # intercepto
x[2] <- 50  # women
x[3] <- 10  # education
x[4] <- 0   # typeprof
x[5] <- 1   # typewc

sum(fit8$coefficients*x)

# predicao com a funcao predict
predict(fit8,list(education=10,women=50,type="wc"),interval = "conf")


######################################
# outra possibilidade: income na saida
######################################
fit <- lm(income ~ . - census, data = prestige)
summary(fit)

stepw <- step(fit, direction="both", trace=FALSE)
summary(stepw)

cor.test(prestige$prestige, prestige$women)



######################
# Outro exemplo de Interacao
######################
# serve apenas como exemplo visto que os 
# coeficientes nao sao significativos
fit <- lm(prestige ~ education * type, data = prestige)
summary(fit)

x <- vector(mode = "numeric", length = 6)

# Y = b1.x1 + b2.x2 + b3.x3 + b4.x4 + b5.x2.x3 + b6.x2.x4
x[1] <- 1   # intercepto
x[2] <- 10  # education
x[3] <- 0   # typeprof
x[4] <- 1   # typewc
x[5] <- x[2]*x[3]
x[6] <- x[2]*x[4]

fit$coefficients*x
sum(fit$coefficients*x)

# predicao com a funcao predict
predict(fit,list(education=10,women=50,type="wc"),interval = "conf")

