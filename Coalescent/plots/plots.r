################
#---PACKAGES---#
################
require(ggplot2)
require(reshape2)


###################################
#--- EXPONENTIAL APPROXIMATION ---#
###################################
p = 0.01
k = c(0:100)
geomProbs = 1 - pgeom(k, prob = p)
expProbs = 1 - pexp(k, rate = p)

p <- ggplot()
p <- p + geom_rect( aes(xmin = k, xmax = k + 1, ymin = 0, ymax = geomProbs), linetype = 2, fill = "white", color = "black" )
p <- p + geom_line(aes(x = k, y = expProbs), color = "red")
p <- p + theme_bw()
p <- p + xlab("k") + ylab("P(X>k)")
p

