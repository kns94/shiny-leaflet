library(shiny)
library(leaflet)
library(RColorBrewer)

emission <- readRDS('emission_test.rds')

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min = as.integer(min(emission$e_hydrocarbon)) -1, max = as.integer(max(emission$e_hydrocarbon)) + 1,
                            value = range(emission$e_hydrocarbon), step = .1
                ),
                radioButtons("radio", label = 'Pollution Identifier', choices = list("Carbon Monoxide" = "e_co", "Hydrocarbon" = "e_hydrocarbon", 
                                                  "NOX" = "e_nox", "Fuel Emission" = "e_fuel"), selected = "e_co"),
                checkboxInput("legend", "Show legend", TRUE)
  )
)