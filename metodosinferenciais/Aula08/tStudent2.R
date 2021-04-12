data(sleep); sleep <- sleep

g1 <- sleep$extra[1 : 10]; g2 <- sleep$extra[11 : 20]

difference <- g2 - g1

mn <- mean(difference); s <- sd(difference); n <- 10

# estatistica t
t <- mn/(s/sqrt(10))

# graus de liberdade
gl <- n - 1
# p-value bicaudal
p_value <- 2*(1-pt(t, gl))
# intervalo de confiança para nível de confiança de 95%
ic <- mn + c(-1, 1) * qt(.975, n-1) * s / sqrt(n)
# média
media <- mn

cat("t = ",t,", df = ",gl,", p-value = ",p_value,"\nic = ",ic[1]," ",
    ic[2],"\nmédia da diferença = ", media,
    "\ng1: média = ",mean(g1)," SE = ",sd(g1)/sqrt(length(g1)),
    "\ng2: média = ",mean(g2)," SE = ",sd(g2)/sqrt(length(g2)), sep="")


t.test(difference)

t.test(g2, g1, paired = TRUE)
