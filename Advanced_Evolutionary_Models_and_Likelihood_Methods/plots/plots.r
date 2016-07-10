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

rate = 10

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
JC69 <- function (lambda, t) {
  p0 = 1/4 + 3/4 * exp(-4*lambda*t)
  p1 = 1/4 - 1/4 * exp(-4*lambda*t)
  
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

set.seed(123)
r    <- runif(4)
vect <- r / sum(r)


data[1, ] <- vect
lambda    <- 2

for (i in 2 : N) {
   probs = as.matrix(data[i - 1, ], nrow = 1, ncol = 4)
   data[i, ] <- probs %*% JC69(lambda, t = times[i])   
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

# library(expm)
# c(0,1,0,0) %*% (JC69(lambda = lambda, t=100))
