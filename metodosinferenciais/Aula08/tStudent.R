x <- seq(-15, 15, .01)
normal.curve <- dnorm(x)
df <- c(1, 5, 9)
colors <- c("purple", "red", "green", "black")
labels <- c(df[1], df[2], df[3], "normal")
plot(x, normal.curve, xlab="x", type="l", lwd = 2,
     ylab="Densidade", main="Distribuições t com diferentes graus de liberdade")

xt <- 1.96; yt <- dnorm(xt)
lines(c(xt,xt), c(-1,yt), lwd=2)
lines(c(-xt,-xt), c(-1,yt), lwd=2)


lines(x, dt(x,df[1]), lwd=2, col=colors[1])
xt <- qt(.975,df[1]); yt  <- dt(xt, df[1])
lines(c(xt,xt), c(-1,yt), lwd=2, col=colors[1])
lines(c(-xt,-xt), c(-1,yt), lwd=2, col=colors[1])


lines(x, dt(x,df[2]), lwd=2, col=colors[2])
xt <- qt(.975,df[2]); yt  <- dt(xt, df[2])
lines(c(xt,xt), c(-1,yt), lwd=2, col=colors[2])
lines(c(-xt,-xt), c(-1,yt), lwd=2, col=colors[2])


lines(x, dt(x,df[3]), lwd=2, col=colors[3])
xt <- qt(.975,df[3]); yt  <- dt(xt, df[3])
lines(c(xt,xt), c(-1,yt), lwd=2, col=colors[3])
lines(c(-xt,-xt), c(-1,yt), lwd=2, col=colors[3])


legend("topleft", inset=.1, title="Graus liberdade",
       labels, lwd=2, lty=c(1, 1, 1, 2), col=colors)


# for (i in 1:3){
#  lines(x, dt(x,df[i]), lwd=2, col=colors[i])
#}
