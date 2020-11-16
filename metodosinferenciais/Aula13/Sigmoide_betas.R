library(manipulate)
x <- seq(-10, 10, length = 1000)
manipulate(
  plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)),
       type = "l", lwd = 3, frame = FALSE),
  beta1 = slider(-4, 4, step = .1, initial = 2),
  beta0 = slider(-4, 4, step = .1, initial = 0)
)