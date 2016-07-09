################
#---PACKAGES---#
################
require(ggplot2)
require(reshape2)


###################################
#---BINOMIAL LIKELIHOOD EXAMPLE---#
###################################
myBlue = rgb(r = 86, g = 110, b = 126, maxColorValue = 250)

like <- function(theta, k = 8, n = 10) {
  return(dbinom(k, n, theta))
}

# 900 x 600
theta <- seq(from = 0, to = 1, by = 0.01)
plot(theta, like(theta), type = "l", col = myBlue, lwd = 3)

points(0.8, like(0.8), pch = 19, col = myBlue, cex = 1.5)


######################
#---NEWTON RAPHSON---#
######################
myBlue = rgb(r = 86, g = 110, b = 126, maxColorValue = 250)

f <- function(x) {
  f = exp(x)/(1 + exp(x))^2
  fx = exp(x)/(1 + exp(x))^2 - 2*exp(2*x)/(1 + exp(x))^3
  fxx = 6 * exp(x)/(1 + exp(x))^4 - 6*exp(2*x)/(1 + exp(x))^3 + exp(x)/(1+exp(x))^2
  return( data.frame(f = f, fx = fx, fxx = fxx ) )
}

x = seq(-10, 10, length.out = 100)
plot(x, f(x)[,1], type = "l", col = myBlue, lwd = 3)

newtonRaphson <- function(f, x0, tol = 1/10^9, maxIter = 100) {

      x  = x0
      fx = f(x)
      i  = 0
      
      cat(paste(c("i", "x", "f(x)", "\n"), sep = "\t"))
      while ( (abs(fx[, 2]) > tol) ) {
      
      if(i < maxIter) {
      
             x  = x - fx[, 2] / fx[, 3]
             fx = f(x)
             
             points(x, fx[,1], pch = 19, col = myBlue, cex = 1.5)
             cat(paste(c(i, x, fx[,1], "\n"), sep = "\t"))
             Sys.sleep(0.5)  
             
             i  = i + 1
             
      } else {
      break;
      }#END: maxIter check
  }#END: tolerance loop
  
  return(x)
}

x.star <- newtonRaphson(f, 5)
# f(x.star)






