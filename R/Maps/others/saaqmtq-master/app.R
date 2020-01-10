library(shiny)
library(tidyverse)
library(leaflet)
library(mapview)
# Define UI for app that draws a histogram ----
ui <- 
  fluidPage(
    fluidRow(
      column(3,uiOutput("choose_region")),
      column(3,uiOutput("choose_ville")),
      column(3, checkboxInput("mortel", "Accidents mortels seulement?", value= FALSE)),
      column(3, checkboxGroupInput(inputId= "types", 
                                   label = "Types d'accidents:",
                                   choices = c("piéton", "vélo"), 
                                   selected = c("piéton", "vélo") ))),
    fluidRow(
      column(5,tableOutput("mytable")),
      column(7,leafletOutput("mapplot"))
      
    )
      #,
      #,
      #mapview:::plainViewOutput("test"),
      
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
types <- c("piéton", "vélos")
  
  
  prepared3 <-   read_rds("prepared3_for_shiny.rds")

  output$choose_region <- renderUI({
    reg.names <- prepared3 %>% distinct(clean_REG_ADM) %>% arrange(clean_REG_ADM) %>% pull(clean_REG_ADM) %>% c("   ",.)
    #reg.names = as.vector(unique(prepared3$clean_REG_ADM))
    selectInput("region", "Region: ", choices = reg.names, selected = "Outaouais")
  })
  
  # return the filtered data
  my_results <- reactive ({
    req(input$region)
    if (input$region != "   "){
    mydata <- prepared3 %>% filter(clean_REG_ADM == input$region) 
    }
    else { mydata <- prepared3}
    
    if (input$mortel == TRUE){mydata <- mydata %>% filter(gravite == "Mortel")}
    mydata <- mydata %>% filter(type %in% c("vélos_et_piétons", input$types))
    list("mydata"= mydata)
  })
  
  output$choose_ville <- renderUI({
    city.names = my_results()$mydata %>% distinct(NAME_MUNCP) %>% arrange(NAME_MUNCP) %>% pull(NAME_MUNCP) %>% c("   ",.)
    selectInput("ville", "Ville: ", choices = city.names)
  }) 
  
  my_results1 <- reactive ({
    req(input$region)
    req(input$ville)
    req(my_results)
    if (input$ville != "   "){
      mydata <- my_results()$mydata %>% filter(NAME_MUNCP == input$ville) 
    }
    else {
    mydata <- my_results()$mydata 
    }
    
    top10 <- mydata %>%
      filter(!is.na(final_lat)) %>%
      group_by(final_lat, final_lon)  %>%
      mutate( rapports = n(), ) %>%
      select(location,NAME_MUNCP , clean_REG_ADM, rapports , final_lat, final_lon) %>%
      group_by(final_lat, final_lon,location)%>%
      mutate(location_count = n()) %>%
      group_by(final_lat, final_lon) %>%
      arrange(-location_count) %>%
      slice(1) %>%
      ungroup() %>%
      arrange(-rapports) %>%
      select(-location_count) %>%
      select(location, everything())%>%
      rename(location_name = location)
    
    
    m <- top10 %>% filter(!is.na(final_lon)) %>% sf::st_as_sf(x = ., coords = c("final_lon", "final_lat"), crs = 4326, agr = "constant") %>%
      mapview::mapview( zcol = "rapports")
    
    
    list("mydata"= mydata %>% head(20),
         "top10" = top10,
         "m"= m)
  })
  
  output$mytable <- renderTable({ 
    req(my_results1)
    my_results1()$top10 %>% select(rapports, location_name, NAME_MUNCP) %>% head(10)})
  output$mapplot <- renderLeaflet({
    req(my_results1)
    my_results1()$m@map
  })
  

  
  
}

shinyApp(ui, server)