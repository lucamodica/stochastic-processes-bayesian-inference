library(ggplot2) #Only need to do once

# exercise 1.14
xvals <- seq(-4,6,.02)
plotdata <- data.frame(x = xvals, y = dnorm(xvals, 1, 2))
ggplot(plotdata, aes(x = x, y = y)) + geom_line()

