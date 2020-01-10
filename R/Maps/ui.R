## Alec James van Rassel
# Packetages à installer
# install.packages("shiny")
# install.packages("leaflet")
# install.packages("dplyr")
# install.packages("leaflet.extras")
# install.packages("brazilmaps")
# install.packages("congressbr")
# install.packages("sp")

library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)
library(congressbr)
library(brazilmaps)
library(sp)
library(sf)

accidents_QC_import <- read.csv(file = "Maps/rapports-accident-2018.csv")
accidents_QC <- accidents_QC_import %>% 
    mutate(DT_ACCDN = as.Date(DT_ACCDN)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Accidents automobiles Québec"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       absolutePanel(top = 60, left = 20, 
                     checkboxInput("losses_per_population", "Population", FALSE),
                     checkboxInput("losses_heat", "Heatmap", FALSE)
       )
    ),

    mainPanel(
        #this will create a space for us to display our map
        leafletOutput(outputId = "map_brazil")
        #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
        
    )
  )
))


