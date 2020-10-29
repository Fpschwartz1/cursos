source("NormalCompara.R")

set.seed(18092003)

# x1 <> x2 ?
dnormalComp(56,19/sqrt(210), 61,18/sqrt(262))
a1 <- rnorm(210,56,19)
a2 <- rnorm(262,61,18)
t.test(a1,a2)

# x1 <> x2 ?
dnormalComp(56,19/sqrt(50), 61,18/sqrt(100))
a1 <- rnorm(50,56,19)
a2 <- rnorm(100,61,18)
t.test(a1,a2)

# x1 <> x2 ?
dnormalComp(56,19/sqrt(100), 61,18/sqrt(45))
a1 <- rnorm(100,56,19)
a2 <- rnorm(45,61,18)
t.test(a1,a2)

# x1 <> x2 ?
dnormalComp(56,27/sqrt(100), 61,20/sqrt(45))
a1 <- rnorm(100,56,27)
a2 <- rnorm(45,61,20)
t.test(a1,a2)

# x1 < x2 ?
dnormalComp(56,27/sqrt(100), 61,20/sqrt(45), rc="<")
a1 <- rnorm(100,56,27)
a2 <- rnorm(45,61,20)
t.test(a1,a2, alternative = "less")

# x2 > x1 ?
dnormalComp(56,27/sqrt(100), 61,20/sqrt(45), rc=">")
a1 <- rnorm(100,56,27)
a2 <- rnorm(45,61,20)
t.test(a2,a1, alternative = "greater")

