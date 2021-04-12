
lmIC <- function(x, Y, vx=NULL, nc=.95){
  
  n <- length(x)
  tc <- nc + (1-nc)/2 # tc = t critico
  
  plot(x, Y , frame=FALSE, pch=21, col="black", bg="lightblue", cex=2)
  fit <- lm(Y ~ x)
  abline(fit, lwd=2)
  beta0 <- coef(fit)[1]
  beta1 <- coef(fit)[2]
  xVals <- seq(min(x,na.rm=TRUE), max(x,na.rm=TRUE), by=(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))/200)
  yVals <- beta0 + beta1 * xVals
  sigma <- sqrt(sum(fit$residuals^2) / (n-2))
  ssx <- sum((x - mean(x))^2)
  se1 <- sigma * sqrt(1 / n + (xVals - mean(x))^2 / ssx)
  se2 <- sigma * sqrt(1 + 1 / n + (xVals - mean(x))^2 / ssx)  
  lines(xVals, yVals + qt(tc,n-2) * se1, col="red")
  lines(xVals, yVals - qt(tc,n-2) * se1, col="red")
  lines(xVals, yVals + qt(tc,n-2) * se2, col="blue")
  lines(xVals, yVals - qt(tc,n-2) * se2, col="blue")
  
  if( !is.null(vx) ){
    ret <- NULL    
    yVals <- beta0 + beta1 * vx
    se2 <- sigma * sqrt(1 + 1 / n + (vx - mean(x))^2 / ssx) 
    for(i in 1:length(vx)){
      ret <- c(ret, yVals[i], 
               yVals[i] - qt(tc,n-2) * se2[i], 
               yVals[i] + qt(tc,n-2) * se2[i])
    }
    return(matrix(ret, ncol=3, byrow = TRUE,
                  dimnames=list(c(),c("esperado","Icmin","ICmax"))))
  }
}
