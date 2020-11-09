# fatorial n!
f <- function(n){factorial(n)}

# combinacao de n p-a-p
comb <- function(n,p){f(n)/( f(p) * f(n-p) )}

# Mega Sena
comb(60,6) # todas as possibilidades de resultados

# se jogarmos 8 números, quantas combinacoes temos?
comb(8,6)
# se jogarmos 8 números, cada jogo custando R$ 4,50
comb(8,6)*4.5

# probabilidade de ganhar comb jogo simples
1/comb(60,6)

# probabilidade de ganhar comb 7 números
comb(7,6)/comb(60,6)
