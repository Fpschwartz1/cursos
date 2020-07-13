source("Normal.R")

# tabelas de integrais
mu <- 500
dp <- 8
h <- hist(rnorm(1000, mu, dp), col = "lightblue", main = "", xlab = "x")
text(mu+1.5*dp, max(h$counts), paste("mu =", mu), adj = c(0,1))
text(mu+1.5*dp, max(h$counts)-20, paste("dp =", dp), adj = c(0,1))


dnormal(mu, dp, 490)
q <- 485
df <- data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp))
q <- 490
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 495
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 500
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 505
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 510
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
View(df)


mu <- 0; dp <- 1
dnormal()
q <- -0.01
df <- data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp))
q <- 0
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 0.01
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 0.02
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 0.02
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
q <- 1.38
df <- rbind(df,data.frame(x=q, A.Esq=pnorm(q,mu,dp), A.Dir=1-pnorm(q,mu,dp)))
View(df)


