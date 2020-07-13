# Atribuicao de valores
a <- 3
b <- 10

# Quatro opercaoes basicas
a + b
a - b
a * b
a / b

# vetores
idade <- c(45, 30, 23, 49)
idade[3]

idade[2:4]

idade[c(1,4)]

nome <- c("Nome1", "Nome2", "Nome3", "Nome4")
nome[3]
nome[c(2,4)]

# matrizes
v <- 1:12
m_v <- matrix(v, 4, 3)
m_v

m_v[2, 2]
m_v[3, 1]

# varaveis logicas
2 == 4
a == 10
a > 10
a < b
a != b
idade >= 30

# data frames
df <- data.frame(nome, idade)
View(df)

# medidas de tendencia central
mu <- mean(idade)
median(idade)

# medidas de dispersao
sd(idade)
n <- length(idade)

# variancia
va <- sum((idade - mu)^2)/(n-1)
va

# desvio padrao
dp <- sqrt(va)
dp








