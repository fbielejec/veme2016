################
#---PACKAGES---#
################
require(ggplot2)
require(reshape2)


###########################
#---POISSON SAMPLE PATH---#
###########################
# 4 x 12
paths = 1
N = 50

rate = 0.1

colorBeta <- rgb(252, 141, 98, maxColorValue = 255, alpha = 255)  
colorAlpha <- rgb(141, 160, 203, maxColorValue = 255, alpha = 255)  

theList <- list()
for(j in 1 : paths) {
  
  data = data.frame(
    x = rep(NA, 2 * N + 1),
    xend = rep(NA, 2 * N + 1 ),
    y = rep(NA, 2 * N + 1),
    yend = rep(NA, 2 * N + 1),
    path = rep(NA, 2 * N + 1)
  )
  
  data$x[1] = 0
  data$xend[1] = rexp(1, rate = rate)
  
  data$y[1] = sample(c("A", "C", "G", "T"), size = 1)
  data$yend = data$y
  
  ids = seq( from = 1, to = (2 * N - 1), by = 2 )
  
  for(i in ids) {
    
    char = sample(c("A", "C", "G", "T"), size = 1)
    time = rexp(1, rate = rate)
    
    data$x[i + 1] = data$xend[i]
    data$xend[i + 1] = data$x[i + 1]
    
    data$y[i + 1] = data$yend[i]
    data$yend[i + 1] = char
    
    data$x[i + 2] = data$xend[i + 1]
    data$xend[i + 2] = data$x[i + 2] + time
    
    data$y[i + 2] = data$yend[i + 1]
    data$yend[i + 2] = data$y[i + 2]
    
    data$path[i] <- data$path[i + 1] <- data$path[i + 2] <- j
    
  } # END: i
  
  theList[[j]] = data
} # END: j

data_melt <- do.call("rbind", theList)
data_melt$path <- as.factor(data_melt$path)
  
p <- ggplot(data_melt)
p <- p + geom_segment(aes(x = x, y = y, xend = xend, yend = yend, linetype = path), colour = colorAlpha, size = 1)
# p <- p + geom_point(aes(x = x, y = y), colour = colorAlpha, fill = colorAlpha, shape = 21, size = 2)

theme1 <- theme(
  axis.line = element_line(colour = "black"),
  axis.text = element_text(colour = "black"),
  axis.ticks = element_line(colour = "black"),
  legend.position = "none",
  panel.background = element_rect(size = 1, fill = "white", colour = NA),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)

nth = 2
ticks <- unique(round(data_melt$x, 1))
ticks <- ticks[seq(1, length(ticks), nth)]
p <- p + scale_x_continuous(
  breaks = ticks
  )

p <- p + theme1
p <- p + xlab("Time") + ylab("")
print(p)


#########################
#---JC69 CTMC EXAMPLE---#
#########################
JC69 <- function (theta, t) {
  p0 = 1/4 + 3/4 * exp(-4*theta*t)
  p1 = 1/4 - 1/4 * exp(-4*theta*t)
  
  trans_prob = matrix(c(p0, p1, p1, p1,
                        p1, p0, p1, p1,
                        p1, p1, p0, p1,
                        p1, p1, p1, p0
  ), nrow = 4, ncol = 4, byrow = T)
  
  colnames(trans_prob) <- rownames(trans_prob) <- c("T", "C", "A", "G")
  return(trans_prob)
}

N = 50
times = seq(0, 0.1, length.out = N)
data <- data.frame(
    T = rep(NA, N),
    C = rep(NA, N),
    A = rep(NA, N),
    G = rep(NA, N)
    )

# set.seed(123)
r    <- runif(4)
vect <- r / sum(r)


data[1, ] <- vect
theta    <- 2

for (i in 2 : N) {
   probs = as.matrix(data[i - 1, ], nrow = 1, ncol = 4)
   data[i, ] <- probs %*% JC69(theta, t = times[i])   
}

data_melt <- melt(cbind(t = times, data), id.vars = "t") 

p <- ggplot(data_melt)
p <- p + geom_line(aes(x = t, y = value, color = variable))    
p <- p + xlab("Time") + ylab("Probability")
theme <- theme(
  axis.title = element_text(colour = "black"),
  axis.text = element_text(colour = "black"),
  axis.line = element_line(colour = "black"),
  axis.ticks = element_line(colour = "black"),
  panel.background = element_rect(size = 1, fill = "white", colour = NA),
  plot.background = element_rect(colour = NA),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank()
)
p <- p + theme
print(p)


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
