##################################################
# http://data.princeton.edu/wws509/datasets/#cuse
# https://data.princeton.edu/r/glms
##################################################
rm(list = ls(all = TRUE)) 

cuse <- read.table("http://data.princeton.edu/wws509/datasets/cuse.dat", header=TRUE)

lrfit <- glm( cbind(using, notUsing) ~ age + education + wantsMore , family = binomial, data=cuse)
summary(lrfit)

# transformacoes para facilitar a interpretacao
cuse$noMore <- cuse$wantsMore == "no"
cuse$hiEduc <- cuse$education == "high"

lrfit <- glm( cbind(using,notUsing) ~ age + hiEduc + noMore, family=binomial, data=cuse)
summary(lrfit)

# ha diferenca significativa entre o modelo proposto e o saturado?
lrfit$deviance
lrfit$df.residual
1-pchisq(lrfit$deviance, lrfit$df.residual)
# SIM

# vamos tentar uma interacao entre idade e o desejo por mais filhos
lrfit <- glm( cbind(using,notUsing) ~ age * noMore + hiEduc , family=binomial, data=cuse)
summary(lrfit)
lrfit$deviance
lrfit$df.residual
# diferen?a entre proposto e saturado
1-pchisq(lrfit$deviance, lrfit$df.residual)
# a 95% de confianca nao ha evidencia contra o modelo
# ha diferenca significativa entre o modelo proposto e nulo?
1-pchisq(lrfit$null.deviance - lrfit$deviance, lrfit$df.null - lrfit$df.residual)


# alterando o modelo
lrfit0 <- update(lrfit, ~ . - age:noMore) # voltando ao original sem a intera??o
anova(lrfit0,lrfit, test="Chisq")

# para modelos lineraes utilizamos a estatistica "F"
# para modelos lineraes generalizados utilizamos a estatistica "X2"
anova(lrfit,test="Chisq")


# Selecao do Modelo
# drop1
drop1(lrfit, test = "Chisq")

# add1
add1(lrfit, ~.^2, test = "Chisq")

# step
search <- step(lrfit0, ~.^2, k = log(1607))
search$anova
