
emission <- readRDS('emission_test.rds')

server <- function(input, output, session) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    emission[emission[, input$radio] >= input$range[1] & emission[, input$radio] <= input$range[2],]
  })
  
 output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet(emission) %>% addTiles() %>%
      fitBounds(~min(longitude), ~min(latitude), ~max(longitude), ~max(latitude))
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
  
    #print(input$radio)
    
    pal <- colorNumeric(
      palette = "Spectral",
      domain = emission[,input$radio]
    )
    
    data = filteredData()
    print(nrow(data))
    
    if(input$radio == "e_co"){
      
      leafletProxy("map", data = filteredData()) %>%
        clearShapes() %>%
        addCircles(radius = ~(e_co*10 + 200)*5, weight = 1, color = ~pal(e_co),
                   fillColor = ~pal(e_co), fillOpacity = 1, popup = ~paste(sep = "<br/>", name, e_co))  
    }
    else{
      if(input$radio == "e_hydrocarbon"){
        
        leafletProxy("map", data = filteredData()) %>%
          clearShapes() %>%
          addCircles(radius = ~(e_hydrocarbon*1000 + 50)*5, weight = 1, color = ~pal(e_hydrocarbon),
                     fillColor = ~pal(e_hydrocarbon), fillOpacity = 1, popup = ~paste(sep = "<br/>", name, e_hydrocarbon))  
      }
      else{
        if(input$radio == "e_nox"){
          
          leafletProxy("map", data = filteredData()) %>%
            clearShapes() %>%
            addCircles(radius = ~(e_nox*100 + 50)*5, weight = 1, color = ~pal(e_nox),
                       fillColor = ~pal(e_nox), fillOpacity = 10, popup = ~paste(sep = "<br/>", name, e_nox))  
        }
        else{
          if(input$radio == 'e_fuel'){
            
            leafletProxy("map", data = filteredData()) %>%
              clearShapes() %>%
              addCircles(radius = ~(e_fuel*100 + 50)*5, weight = 1, color = ~pal(e_fuel),
                         fillColor = ~pal(e_fuel), fillOpacity = 10, popup = ~paste(sep = "<br/>", name, e_fuel))  
          }
        }
      }
    }
  })
  
  observe({
    print(input$radio)
    ma <- as.integer(max(emission[,input$radio])) + 1
    mi <- as.integer(min(emission[,input$radio])) -1
    n <- length(emission[,input$radio])
    print(n)
    print(range(emission[,input$radio]))
    
    updateSliderInput(session, "range",  max = ma, min <- mi,
                      value = range(emission[,input$radio]), step = (ma - mi)/n) 
  })
  
  observe({
    
    proxy <- leafletProxy("map", data = filteredData())
    
    proxy %>% clearControls()
    if (input$legend && input$radio == 'e_co') {
      pal <- colorNumeric(
        palette = "Spectral",
        domain = emission[,input$radio]
      )
      proxy %>% addLegend("bottomright", pal = pal, values = ~e_co, title = "Legend", opacity = 1 
      )
    }
    
    if (input$legend && input$radio == 'e_hydrocarbon') {
      pal <- colorNumeric(
        palette = "Spectral",
        domain = emission[,input$radio]
      )
      proxy %>% addLegend("bottomright", pal = pal, values = ~e_hydrocarbon, title = "Legend", opacity = 1 
      )
    }
    
    if (input$legend && input$radio == 'e_nox') {
      pal <- colorNumeric(
        palette = "Spectral",
        domain = emission[,input$radio]
      )
      proxy %>% addLegend("bottomright", pal = pal, values = ~e_nox, title = "Legend", opacity = 1 
      )
    }
    
    if (input$legend && input$radio == 'e_fuel') {
      pal <- colorNumeric(
        palette = "Spectral",
        domain = emission[,input$radio]
      )
      proxy %>% addLegend("bottomright", pal = pal, values = ~e_fuel, title = "Legend", opacity = 1 
      )
    }
  })
}