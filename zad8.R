library(ggplot2)
diamonds
tapply(diamonds$price[diamonds$price >1000], diamonds$clarity[diamonds$price >1000], mean)
