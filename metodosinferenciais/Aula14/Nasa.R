dados.nasa = read.csv('nasa_disaster.csv')
modelo = glm(failure ~ temperature, data = dados.nasa, family = binomial(link = 'logit'))  
summary(modelo) 


temp = dados.nasa$temperature
p = modelo$fitted.values  
xlb = expression(paste('Temperatura [', degree,'F]'))  
titulo = 'NASA - Desastre Challenger\nProb falha x Temperatura'  
plot(temp, p,  col = 'navy', pch = 4, cex = 1.5, ylab = 'p', xlab = xlb, main = titulo)  
grid(10, 10, col = '#CCCCCC')  

# A temperatura dos aneis de vedacao no dia do lancamento era de 31F
new.temp = c(31, 54, 55, 58, 59, 60, 61, 62, 64, 65, 71, 74, 77)  
new.data = data.frame(temperature = new.temp)  
probs = predict(modelo, new.data, type = 'response')  
plot(temp, p,  col = 'navy', pch = 4, cex = 1.5, ylab = 'p', xlab = xlb, main = titulo, xlim=c(20,85), ylim=c(0,1))  
grid(10, 10, col = '#CCCCCC')  
points(new.temp, probs, pch = 3, col  ='red', cex = 1.5)  
legenda = c('modelo', 'previsto')  
pontos.tipo = c(4, 3)  
cores = c('navy', 'red')  
legend('topright', legenda, col = cores, pch = pontos.tipo)  

text(33,probs[1],round(probs[1],4),pos=4)

# 
s <- summary(modelo)
s

s$coefficients[1]
exp(s$coefficients[1])

s$coefficients[2]
exp(s$coefficients[2])


# qualidade do modelo
1-pchisq(s$deviance,s$df.residual)
