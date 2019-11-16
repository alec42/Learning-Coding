install.packages("nycflights13")
library(nycflights13)
data(flights)
library(dplyr)

flights %>%
    group_by(month) %>%
    summarise(mean = mean(distance), sd = sd(distance))
View(flights)
attach(flights)

flights %>%
    filter(is.na(dep_delay) != T) %>%
    group_by(carrier) %>%
    summarise(mean = mean(dep_delay), sd = sd(dep_delay))

flights %>%
    filter(is.na(dep_delay) != T) %>%
    group_by(dest) %>%
    summarise(mean = mean(dep_delay), 
              sd = sd(dep_delay), 
              n = n())


