# install.packages("nycflights13")
library(nycflights13)
data(flights)
library(tidyverse)

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

###
### Joins
###
state_name <- data.frame(cbind(state.abb, state.name))[1:30, ] 
state_area <- data.frame(cbind(state.abb, state.area))[11:50, ] 

View(left_join(state_name, state_area))
View(left_join(state_area, state_name))
state_inner <- inner_join(state_area, state_name)
View(full_join(state_area, state_name))

## with good syntax
state_name %>% 
    left_join(state_area, by = "state.abb")


