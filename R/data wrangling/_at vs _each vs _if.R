library(scales)
library(tidyverse)
library(sqldf)
library(CASdatasets)
library(lubridate)

data("ausautoBI8999")
data("danishmulti")
ausautoBI8999 %>% 
    group_by(AccDate) %>% 
    summarise_each(funs(mean))

danishmulti %>% 
    mutate_at(c("Total", "Building"), dollar) 
    # equivalent to
    # %>% mutate(Total = dollar(Total)) %>% 
    # mutate(Building = dollar(Building))

danishmulti %>% 
    mutate_at(c("Total", "Building"), ~dollar(., accuracy = 1))  
    # can also add arguments too the function

ausautoBI8999 %>% 
    mutate_at(vars(matches("Mth")), ~.*10)
    # can apply a function to all columns which contain the
    # word Mth. For example multiply by 10.

ausautoBI8999 %>% 
    mutate_at(vars(c("AccMth", "ReportMth")), ~.*10)
   # can apply a function to selection of columns which we 
   # select by name.

ausautoBI8999 %>% 
    mutate_if(is.numeric, ~./100)
   # can apply a function to all columns which match the
   # a filter.

danishmulti %>% 
    summarise_at(c("Total"), list(mean, sd))
    # mean and SD of the total column

danishmulti %>% 
    summarise_all(list(mean, sd))
    # calculate the mean and sd of every column
danishmulti %>% 
    summarise_all(list(~mean(.), ~sd(.)))
    # equivalent

danishmulti %>% 
    summarise_if(is.numeric, list(Q2 = ~quantile(., probs = 0.25)))
    # if a column is numeric, calculate the second quantile