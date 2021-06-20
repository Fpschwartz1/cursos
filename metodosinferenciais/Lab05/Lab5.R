source("teste_t.R")

# Considere nível de confiança de 95% e teste bicaudal para os testes abaixo. 

# status quo
sq <- 50

# amostra
set.seed(764939)
a <- rnorm(30, 30, 20)

teste.t(x=a, mu=sq)
teste.t(x=a, mu=sq, alternative = "less")
teste.t(x=a, mu=sq, alternative = "greater")


# grupos pareados
x <- c(2.3, 0.0, 1.4, 0.4, 1.5, 5.0, 5.3, 2.4, 1.6, 3.6)
y <- c(3.5, 2.4, 2.7, 1.7, 1.5, 6.0, 7.1, 3.2, 6.2, 5.0)

teste.t(x, y, paired = TRUE)
teste.t(x, y, paired = TRUE, dif.media = FALSE)
teste.t(x, y, paired = TRUE, alternative = "less")
teste.t(x, y, paired = TRUE, alternative = "less", dif.media = FALSE)
teste.t(x, y, paired = TRUE, alternative = "greater")
teste.t(x, y, paired = TRUE, alternative = "greater", dif.media = FALSE)

# x menor que y eh o mesmo que ...
teste.t(x, y, paired = TRUE, alternative = "less", dif.media = FALSE)
# y maior que x.
teste.t(y, x, paired = TRUE, alternative = "greater", dif.media = FALSE)



# grupos independentes - variancias iguais
#set.seed(57830)
# a1 <- rnorm(25, 21.989, 1.1535)
x <- c(24.18751, 20.00562, 21.22403, 19.86628, 21.83883,
       23.20288, 20.85793, 22.44734, 21.45421, 23.57761,
       21.65793, 23.69565, 21.33809, 21.33315, 21.90242,
       20.41248, 21.89598, 22.21946, 23.63658, 20.34899,
       21.58881, 20.59255, 24.85809, 24.25010, 21.61999)

#set.seed(57830)
#a2 <- rnorm(30, 20.91, 1.3465)
y <- c(23.47635, 18.59477, 20.01704, 18.43211, 20.73471,
       22.32699, 19.58968, 21.44502, 20.28573, 22.76441, 
       20.52354, 22.90220, 20.15018, 20.14442, 20.80894,
       19.06970, 20.80142, 21.17903, 22.83325, 18.99559,
       20.44286, 19.27990, 24.25914, 23.54942, 20.47925,
       21.10211, 21.41440, 21.26698, 21.34999, 21.78228)

teste.t(x, y, var.equal = TRUE)
teste.t(x, y, var.equal = TRUE, dif.media = FALSE)
teste.t(x, y, var.equal = TRUE, alternative = "less")
teste.t(x, y, var.equal = TRUE, alternative = "less", dif.media = FALSE)
teste.t(x, y, var.equal = TRUE, alternative = "greater")
teste.t(x, y, var.equal = TRUE, alternative = "greater", dif.media = FALSE)

# x menor que y eh o mesmo que ...
teste.t(x, y, var.equal = TRUE, alternative = "less", dif.media = FALSE)
# y maior que x.
teste.t(y, x, var.equal = TRUE, alternative = "greater", dif.media = FALSE)



# grupos independentes - variancias diferentes
#set.seed(578301)
# a1 <- rnorm(23, 21.98, 1.168)
x <- c(22.55839, 22.54998, 23.78845, 20.58032, 22.28616,
       23.66850, 21.87726, 24.85481, 20.58217, 20.60274,
       21.59821, 22.17370, 22.06624, 23.41259, 20.62497,
       21.84104, 22.05482, 20.98034, 22.70375, 22.27651,
       20.54948, 19.28819, 23.08621)

#set.seed(57830)
#a2 <- rnorm(32, 21.2627, 2.43025)
y <- c(25.89462, 17.08402, 19.65103, 16.79044, 20.94632,
       23.82017, 18.87970, 22.22834, 20.13598, 24.60966,
       20.56519, 24.85836, 19.89132, 19.88092, 21.08029,
       17.94120, 21.06673, 21.74825, 24.73390, 17.80745,
       20.41957, 18.32058, 27.30744, 26.02649, 20.48526,
       21.60943, 22.17308, 21.90701, 22.05682, 22.83704,
       24.93095, 20.31521)

teste.t(x, y)
teste.t(x, y, dif.media = FALSE)
teste.t(x, y, alternative = "less")
teste.t(x, y, alternative = "less", dif.media = FALSE)
teste.t(x, y, alternative = "greater")
teste.t(x, y, alternative = "greater", dif.media = FALSE)

# x menor que y eh o mesmo que ...
teste.t(x, y, alternative = "less", dif.media = FALSE)
# y maior que x.
teste.t(y, x, alternative = "greater", dif.media = FALSE)

