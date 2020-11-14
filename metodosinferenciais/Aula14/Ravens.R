load("ravensData.rda")
head(ravensData)

logRegRavens <- glm(ravensData$ravenWinNum ~ ravensData$ravenScore,
                    family = "binomial")
s <- summary(logRegRavens)
s

s$coefficients[1]
exp(s$coefficients[1])

s$coefficients[2]
exp(s$coefficients[2])

# qualidade do modelo
1-pchisq(s$deviance,s$df.residual)



