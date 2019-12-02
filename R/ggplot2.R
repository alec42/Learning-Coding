library(tidyverse)

## différents histogrammes selon des variables de la BD mpg
ggplot(mpg, aes(x = model)) + 
    geom_histogram(bins = 10)
ggplot(mpg, aes(x = cty)) + 
    geom_histogram(bins = 10)
ggplot(mpg, aes(x = hwy, fill = class)) + 
    geom_histogram(bins = 20) +
    scale_fill_brewer(palette = "PuBu")
    
## Couleur par la classe du véhicule
ggplot(mpg, aes(x = displ, y = hwy, colour = class)) + 
    geom_point()

## Grosseur des points par la classe du véhicule
ggplot(mpg, aes(x = displ, y = hwy, size = cyl)) + 
    geom_point()

###
### Vignette qui décrit les aestétiques
###
vignette("ggplot2-specs")