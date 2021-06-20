# shapiro-wilk
set.seed(45673)
cd1 <- rnorm(1000)
hist(cd1)
shapiro.test(cd1)

set.seed(45673)
cd2 <- runif(1000)
hist(cd2)
shapiro.test(cd2)


# kolmogorov-smirnov
set.seed(45673)
cd1 <- rnorm(10000)
hist(cd1)
z1 <- (cd1 - mean(cd1)) / sd(cd1)
ks.test(z1, "pnorm")

set.seed(45673)
cd2 <- runif(10000)
hist(cd2)
z2 <- (cd2 - mean(cd2)) / sd(cd2)
ks.test(z2, "pnorm")

