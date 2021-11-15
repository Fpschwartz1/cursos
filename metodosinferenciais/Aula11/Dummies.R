library(datasets)
require(stats)
require(graphics)
data(InsectSprays); head(InsectSprays)
? InsectSprays

boxplot(count ~ spray, data = InsectSprays, col = "lightgray",
        xlab="Tipo do spray", ylab="Quantidade de insetos")
# modelo linear
summary(lm(count ~ spray, data = InsectSprays))$coef

# explicitando o modelo
summary(lm(count ~
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F'))
           , data = InsectSprays))$coef
# e se incluirmos as 6 variáveis
summary(lm(count ~
             I(1 * (spray == 'B')) + I(1 * (spray == 'C')) +
             I(1 * (spray == 'D')) + I(1 * (spray == 'E')) +
             I(1 * (spray == 'F')) + I(1 * (spray == 'A')), data =
             InsectSprays))$coef
# e se omitirmos o intercepto
summary(lm(count ~ spray - 1, data = InsectSprays))$coef

unique(ave(InsectSprays$count, InsectSprays$spray))

# reordenando os níveis
spray2 <- relevel(InsectSprays$spray, "C")
summary(lm(count ~ spray2, data = InsectSprays))$coef
