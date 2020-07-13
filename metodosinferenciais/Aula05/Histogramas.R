rm(list = ls(all = TRUE)) 

set.seed(19701006)

fidade <- function(N) {c(
  runif(N*0.05,10,20),
  runif(N*0.10,20,30),
  runif(N*0.20,30,40),
  runif(N*0.30,40,50),
  runif(N*0.20,50,60),
  runif(N*0.10,60,70),
  runif(N*0.05,70,80)
)}

idade <- fidade(100)
breaks <- 7

# idades inteiras
idader <- idade # round(idade,0)
hist(idader, breaks = breaks*2^0, col = "green", main = "Idade sem decimal", xlab = "idade")
hist(idader, breaks = breaks*2^1, col = "lightblue", add = T)
hist(idader, breaks = breaks*2^2, col = "red", add = T)
hist(idader, breaks = breaks*2^3, col = "orange", add = T)

# com uma casa decimal
idader1 <- round(idade,1)
hist(idader1, breaks = (breaks*2^3)*10, add = T, border = TRUE)
# zoom
hist(idader, breaks = breaks*2^3, col = "orange", border = FALSE, main = "Idade com uma decimal", xlab = "idade")
hist(idader1, breaks = (breaks*2^3)*10, add = T, border = TRUE)
