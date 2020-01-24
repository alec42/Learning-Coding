library(tidyverse)
library(hflights)
data("hflights")

#--------------#
#### Tibble ####
#--------------#
### The purpose is essentially to make it easier to look at.
### It's essentially a data.frame that has nicer printing properties
###
flights <- as_tibble(hflights)
flights

#-----------------#
#### Filtering ####
#-----------------#
### Instead of having indices [c(1,2), ] and stuff like that,
### dplyr has an intuitive approach with the filter function.
###
#-------#
#  AND  #
#-------#
filter(flights,    #    data
       Month == 6, #    condition(s)
       DayofMonth == 21
) 
filter(flights,
       Month == 6 & DayofMonth == 21 #    we can equivalently use &
) 
#--------#
#   OR   #
#--------#
filter(flights,
       Month == 6 & 
           DayofMonth == 21  | #    we use the pipe | for or conditions
           DayofMonth == 22
) 
#--------#
#   IN   #
#--------#
filter(flights,
       Dest %in% c("MIA", "BOS") #  R has a nifty operator for limiting 
)                                #  to a subset of possible values.

#-----------------#
#### Selecting ####
#-----------------#
### Instead of having indices like for example:
###     `flights[, c("DepTime", "ArrTime", "FlightNum")]`
### we can use the select() function like in SQL.
select(flights,
       DepTime,
       ArrTime,
       FlightNum
)
### dplyr also has some ways of integrating REGEX

#-----------------------------------#
##### Mutating multiple columns #####
#-----------------------------------#
### We can apply functions over several columns.
### 
### `mutate_all`
###     Will apply a function over all columns.
### For example, we use the tolower function to make all columns lower-case:
flights %>% 
    mutate_all(tolower)
### We often need to pass arguments to the function, but want to keep it in _functional_ form;
### `funs` converts something to a function in the same way lm(Y ~ x)
### `~paste(., " /n ")` <=>  `funs(paste(., " /n "))`.
### 
### For example, we replace all new lines by nothing:
flights %>% 
    mutate_all(~str_replace_all(., "/n", ""))

### `mutate_if`
###     Will apply a function depending on a first boolean value
###     and needs instructions for what to do.
### For example: `is.numeric`, `is.integer`, `is.double`, etc.
flights %>% 
    select(Distance, Origin) %>% 
    mutate_if(is.numeric, round)
### More generally
flights %>% 
    select(Distance, Origin) %>% 
    mutate_if(is.numeric, ~round(., 2))
### ~: treats it as a function
### .: says to use previous value, floating variable sort of
### 
### 
### 