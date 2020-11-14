###################################
# https://ww2.coastal.edu/kingw/statistics/R-tutorials/logistic.html

library(MASS)
data("menarche")
str(menarche)

? menarche

plot(Menarche/Total ~ Age, data=menarche)

# nao temos uma variavel para cada individuo pesquisado, ou seja,
# nao temos uma variavel binaria (0,1)
# mas podemos usar glm com uma matriz na qual a primeira coluna 
# corresponde aos casos de sucesso e a segunda, de insucesso
glm.out <- glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial, data=menarche)

lines(menarche$Age, glm.out$fitted.values, type = "l", col="red")
# os valores ajustados est?o bem pr?ximos dos observados

summary(glm.out)
glm.out$deviance
glm.out$df.residual
# a hipotese nula Ho eh o modelo saturado
# verificar se ha diferenca significativa entre o modelo proposto 
# e o saturado
1-pchisq(glm.out$deviance, glm.out$df.residual)
# verificar se ha diferenca significativa entre o modelo proposto 
# e o nulo
1-pchisq(glm.out$null.deviance - glm.out$deviance, glm.out$df.null - glm.out$df.residual)

# beta0 - chance da menarca com zero anos
beta0 <- glm.out$coefficients[1]
exp(beta0)

# beta1 - razao das chances quando a idade varia 1 ano
beta1 <- glm.out$coefficients[2]
exp(beta1)

# sigmoide
sigmoide <- function(x) {as.numeric(1 / (1 + exp(-(beta0 + beta1*x))))}

# beta0
p0 <- sigmoide(0) # probabilidade em Age = 0
p0 / (1 - p0) # chance quando Age = 0

# beta1
# probabilidades com 13 e 14 anos
px  <- sigmoide(13)
px1 <- sigmoide(14)

# plotando 
lines(c(0,20), c(0,0), lty = "dashed") # linha em 0
lines(c(0,20), c(1,1), lty = "dashed") # linha em 1
lines(c(13,13), c(-1,px), lty = "dashed", col="red") # Age = 13
text(13,0,"13",col="red",pos=3)
lines(c(0,13), c(px,px), lty = "dashed", col="red") # px
text(9.5,px,round(px,3),col="red",pos=3)
lines(c(14,14), c(-1,px1), lty = "dashed", col="red") # Age = 14
lines(c(0,14), c(px1,px1), lty = "dashed", col="red") # px1
text(9.5,px1,round(px1,3),col="red",pos=3)

# razao das chances
(px1/(1 - px1)) / (px/(1 - px))
