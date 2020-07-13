set.seed(18092003)
n <- 50000
media <- cumsum(sample(0:1, n, replace = TRUE))/(1:n)
g <- ggplot(data.frame(x = 1:n, y = media), aes(x = x, y = y))
g <- g + geom_hline(yintercept = 0.5) + geom_line(size = 1)
g <- g + labs(x = "Número de observações", y = "Média cumulativa")
g
