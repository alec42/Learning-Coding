### ACT-2003: Modèles Linéaires
### Alec James van Rassel
## Code pour l'application basé sur le template de Joy P Wyckoff
## lien à son application: https://medium.com/@joyplumeri/how-to-make-interactive-maps-in-r-shiny-brief-tutorial-c2e1ef0447da

library(shiny)

# Add session argument to server function
shinyServer(function(input, output, session) {
   
    # defini la palette de couleurs par l'amplitude des pertes.
    pal <- colorBin(
        palette = "YlOrRd",
        domain = merged_losses_pop_by_AB$AvgAmount
        )
    
    # defini la palette de couleurs par la taille de la populaiton.
    pal2 <- colorBin(
        palette = "Greens",
        domain = merged_losses_pop_by_AB$Population
    )
    
    # defini la palette de couleurs pour le ratio
    pal3 <- colorBin(
        palette =  c("blue", "red", "green"), 
        domain = merged_losses_pop_by_AB$ratio_avg_pop
    )

    output$map_brazil <- renderLeaflet({
        leaflet(data = merged_losses_pop_by_AB) %>%
            setView(lng = -71, lat = 46.8, zoom = 3)  %>% # placer la carte au-dessus du Brésil
            addTiles() %>%
            addPolygons(
                fillColor = ~pal(AvgAmount),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7
            ) %>%
            addLegend("topright", 
                      pal = pal, 
                      values = merged_losses_pop_by_AB$AvgAmount,
                      title = "Average Amount",
                      opacity = 1)
    })
    # 
    # output$map_brazil <- renderLeaflet({
    #     leaflet(data = merged_losses_pop_by_AB) %>% 
    #         setView(lng = -52, lat = -14, zoom = 3)  %>% # placer la carte au-dessus du Brésil
    #         addTiles() %>% 
    #         addPolygons(
    #             fillColor = ~pal2(Population),
    #             weight = 2,
    #             opacity = 1,
    #             color = "white",
    #             dashArray = "3",
    #             fillOpacity = 0.7
    #         )
    # })
    # 
    observe({
        proxy <- leafletProxy("map_brazil", data = merged_losses_pop_by_AB)
        proxy %>% clearMarkers()
        if (input$losses_per_population) {
            proxy %>% 
                addPolygons(stroke = FALSE, 
                                 color = ~pal2(Population), 
                                 fillOpacity = .8,      
                                 label = ~as.character(paste0("Population: ", sep = " ", Population))) %>%
                addLegend("bottomright", 
                          pal = pal2, 
                          values = merged_losses_pop_by_AB$Population,
                          title = "Population",
                          opacity = 1)}
        else {
            proxy %>% clearMarkers() %>% clearControls() %>% clearShapes() %>% addPolygons(
                fillColor = ~pal(AvgAmount),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7
            ) %>%
                addLegend("topright", 
                          pal = pal, 
                          values = merged_losses_pop_by_AB$AvgAmount,
                          title = "Average Amount",
                          opacity = 1)
        }
    })
    
    observe({
        proxy <- leafletProxy("map_brazil", data = merged_losses_pop_by_AB)
        proxy %>% clearMarkers()
        if (input$losses_heat) {
            proxy %>%  
                addHeatmap(lng=geometry$X, 
                           lat=geometry$Y, 
                           intensity = merged_losses_pop_by_AB$ratio_avg_popw, 
                           # blur =  5,
                           # max = 5, 
                           minOpacity = .8,
                           radius = 10) %>%
                addLegend("topleft", 
                          pal = pal3,
                          values = merged_losses_pop_by_AB$ratio_avg_pop,
                          title = "Ratio losses to population",
                          opacity = .8)
        }
        else{
            proxy %>% clearHeatmap() %>% clearMarkers() %>% clearControls()%>% addPolygons(
                fillColor = ~pal(AvgAmount),
                weight = 2,
                opacity = 1,
                color = "white",
                dashArray = "3",
                fillOpacity = 0.7
            ) %>%
                addLegend("topright", 
                          pal = pal, 
                          values = merged_losses_pop_by_AB$AvgAmount,
                          title = "Average Amount",
                          opacity = 1)
        }
        
        
    })
    
})
