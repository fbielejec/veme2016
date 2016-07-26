################
#---PACKAGES---#
################
require(ggplot2)
require(reshape2)


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


# fair coin?

