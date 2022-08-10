# install.packages("googlesheets4")
# install.packages("tidyverse")
library(tidyverse)
library(googlesheets4)

urlSheet <- "https://docs.google.com/spreadsheets/d/16BYXmUtOwr3xYni_e0xVN2mgItJvsX1ER-2siZB5wpI/edit#gid=0"

dat <- read_sheet(ss = urlSheet, sheet = "Activities", range = "Activities!C2:H1000")
View(dat)
