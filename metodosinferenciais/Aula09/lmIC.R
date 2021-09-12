lmIC <- function(x, Y, vx=NULL, nc=.95){
  n <- length(x)
  tc <- nc + (1-nc)/2 # tc = t crítico
  plot(x, Y , frame=FALSE, pch=21, col="black", bg="lightblue", cex=1.5)
  grid()
  fit <- lm(Y ~ x)
  abline(fit, lwd=2)
  beta0 <- coef(fit)[1]
  beta1 <- coef(fit)[2]
  xVals <- seq(min(x,na.rm=TRUE),
               max(x,na.rm=TRUE),
               by=(max(x,na.rm=TRUE)-min(x,na.rm=TRUE))/200)
  yVals <- beta0 + beta1 * xVals
  sigma <- sqrt(sum(fit$residuals^2) / (n-2))
  ssx <- sum((x - mean(x))^2)
  se1 <- sigma * sqrt(1 / n + (xVals - mean(x))^2 / ssx)
  se2 <- sigma * sqrt(1 + 1 / n + (xVals - mean(x))^2 / ssx)
  lines(xVals, yVals + qt(tc,n-2) * se1, col="blue")
  lines(xVals, yVals - qt(tc,n-2) * se1, col="blue")
  lines(xVals, yVals + qt(tc,n-2) * se2, col="red")
  lines(xVals, yVals - qt(tc,n-2) * se2, col="red")
  if( !is.null(vx) ){
    
    yVal <- beta0 + beta1 * vx
    if(vx %in% x){
      se <- sigma * sqrt(1 / n + (vx - mean(x))^2 / ssx)
      col = "blue"
    } else {
      se <- sigma * sqrt(1 + 1 / n + (vx - mean(x))^2 / ssx)  
      col = "red"
    }
    
    ret <- c(yVal, yVal - qt(tc,n-2) * se, yVal + qt(tc,n-2) * se)
    
    lines(c(0,vx), c(ret[2],ret[2]), col = col, lty="dotted")
    lines(c(0,vx), c(ret[3],ret[3]), col = col, lty="dotted")
    lines(c(vx,vx), c(ret[2],ret[3]), col = col, lty="dotted")
    points(vx, yVal, col = col, bg = col, pch=21, cex=1.5)
    
    return(matrix(ret, ncol=3, byrow = TRUE,
                  dimnames=list(c(),c("esperado","ICmin","ICmax"))))
  }
}