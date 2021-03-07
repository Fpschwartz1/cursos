# consumo gasolina
cg <- c(10.8, 12.3, 11.2, 12.4, 13, 10.7, 12.7)
mu <- mean(cg)
plot(cg, ylab = "Consumo (km/l)", xlab = "Aferições", 
      main = "Desempenho")


library(ggplot2)
df <- data.frame(afericao = 1:length(cg), cg)

p <- ggplot(df, aes(x=afericao, y=cg)) + geom_point() +
  labs(title="Desempenho do carro", x="Aferição", y = "Consumo (km/l)")
  # + theme_classic()
p

p <- p + geom_hline(yintercept=mean(cg), color="orange", size=1) 
p
