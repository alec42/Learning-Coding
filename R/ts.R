library(tidyverse)
library(quantmod)
getSymbols(Symbols = "CAE.TO", src = "yahoo")

barChart(CAE.TO)
candleChart(CAE.TO, multi.col = T, theme = "white")
chartSeries(to.weekly(CAE.TO), up.col = "white", dn.col = "blue", subset = "20130102::20171227")
addVolatility()
addBBands()
addMACD()

ggplot(CAE.TO, aes(x = date, y = value)) + 
    geom_line(aes(color = variable), size = 1) +
    scale_color_manual(values = c("#00AFBB", "#E7B800")) +
    theme_minimal()
    
