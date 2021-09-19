library(datasets)
data(swiss)
require(stats)
require(graphics)

# conhecendo os dados
? swiss

# verificando as relações marginais
pairs(swiss, panel = panel.smooth, main = "Swiss data", col = 3 +
        (swiss$Catholic > 50))


# regressão múltipla - ver estimativa ajustada de Agriculture

fit <- lm(Fertility ~ . , data = swiss)
summary(fit)$coefficients

summary(lm(Fertility ~ . , data = swiss))

# estimativa ajustada de Agriculture
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients

# inversao de sinal - exemplo 1
y = swiss$Fertility
x1 = swiss$Agriculture
x2 = swiss$Examination


summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef
# 
library(ggplot2)
dat = data.frame(y = swiss$Fertility, x1 = swiss$Agriculture, x2 =
                   swiss$Examination, ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm,
                                                            se = FALSE, colour = "black")
g = g + geom_point(size = 4)
g

g2 = ggplot(dat, aes(y = ey, x = ex1, colour = x2))
g2 = g2 + geom_point(colour="grey50", size = 5) + geom_smooth(method = lm, se = FALSE, colour = "black") + geom_point(size = 4)
g2


# quanto o modelo explica o índice de fertilidade
summary(lm(Fertility ~ Agriculture
           + Examination
           + Education
           + Catholic
           + Infant.Mortality
           , data = swiss))

# o sinal inverte com a exclusao de Examination + Education
# ambos são inversamente correlacionados com Agriculture
summary(lm(Fertility ~ Agriculture
           # + Examination
           + Education
           + Catholic
           + Infant.Mortality
           , data = swiss))
cor(swiss$Agriculture, swiss$Examination)
cor(swiss$Agriculture, swiss$Education)
cor(swiss$Education, swiss$Examination)

# e se incluirmos uma variável desnecessaria
z <- swiss$Agriculture + swiss$Education
fit <- lm(Fertility ~ . + z, data = swiss)
summary(fit)


# predicao de valores
fit <- lm(Fertility ~ Agriculture
          + Education
          + Catholic
          + Infant.Mortality
          , data = swiss)
summary(fit)

df <- data.frame(Agriculture=c(0,50),Examination=c(0,50),
                 Education=c(0,50),Catholic=c(0,50),Infant.Mortality=c(0,50))
df

predict(fit, df, interval = "confidence")
predict(fit, df, interval = "prediction")
