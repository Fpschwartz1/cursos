fit1 <- lm(Fertility ~ Agriculture, data = swiss)


fit2 <- update(fit1, Fertility ~ Agriculture + Education,
               data = swiss)


fit3 <- update(fit2, Fertility ~ Agriculture + Education + Catholic, data = swiss)

anova(fit1, fit2, fit3)

