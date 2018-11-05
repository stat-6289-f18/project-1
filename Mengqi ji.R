library(shiny)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(tools)
library(stringr)
library(dplyr)
library(DT)


library(readr)
drinks <- read_csv("~/Desktop/drinks.csv")
drink=as.data.frame(drinks[,1:4])


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Input:
      #select beer,spirit, wine  
      selectInput(inputId = "type1", 
                  label = "Choose one type:",
                  choices = c("beer","spirit","wine"), 
                  selected = "beer"),
      hr(), 
      # Select beer,spirit, wine 
      radioButtons(inputId = "type2", 
                   label = "Choose one type:",
                   choices = c("beer","spirit","wine"), 
                   selected = "beer"),
      hr(), 
      # Enter text for plot title
      textInput(inputId = "title", 
                label = "Please enter the type you choose:", 
                placeholder = "Enter text to be used as title"),
      hr(), # Horizontal line for visual separation
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      hr()),
    # Output:
    mainPanel(
      tabsetPanel(type = "tabs",
                  # Tab 1: Map
                  tabPanel(title = "Where Do People Drink The Most Beer In the U.S.?", 
                           leafletOutput(outputId = "worldmap",height=1000)),
                  # Tab 2: Data Table
                  tabPanel(title = "Top 10 countries by servings consumed per person, 2010", 
                           br(),        #",toTitleCase(input$title),"
                           DT::dataTableOutput(outputId = "toptable"),
                           br(),
                           h5(textOutput("description")))
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  beer=reactive({
    beer=drink[,1:2]
    beer
  })
  spirit=reactive({
    spirit=drink[,c(1,3)]
    spirit
  })
  wine=reactive({
    wine=drink[,c(1,4)]
    wine
  })
  
  beer2=head(beer[order(beer[,2],decreasing=T),c(1,2)],10)
  spirit2=head(spirit[order(spirit[,2],decreasing = T),c(1,2)],10) 
  wine2=head(wine[order(wine[,2],decreasing=T),c(1,2)],10)
  a=c(unlist(beer2),unlist(spirit2),unlist(wine2))
  b=matrix(a,10,6)
  c=as.data.frame(b)
  
  #create the map
  # From http://leafletjs.com/examples/choropleth/us-states.js
  states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")
  
  bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal <- colorBin("YlOrRd", domain = states$density, bins = bins)
  
  labels <- sprintf(
    "<strong>%s</strong><br/>%g people / mi<sup>2</sup>",
    states$name, states$density
  ) %>% lapply(htmltools::HTML)
  
  leaflet(states) %>%
    setView(-96, 37.8, 4) %>%
    addProviderTiles("MapBox", options = providerTileOptions(
      id = "mapbox.light",
      accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN'))) %>%
    addPolygons(
      fillColor = ~pal(density),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto")) %>%
    addLegend(pal = pal, values = ~density, opacity = 0.7, title = NULL,
              position = "bottomright")
  
  # Print data table
  output$toptable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = c, 
                    options = list(pageLength = 10), 
                    rownames = c("Country","Beer","Country","Spirit","Country","Wine"))
    })
  
  
  # Create description of plot
  output$description <- renderText({
    print("Namibia is the world’s biggest beer-drinking country, with 376 12-ounce cans of beer consumed per person. In Germany, the average is 346 cans, and in the U.S. it’s 249 cans.
          If you assume that 1.5 fluid ounces is a normal measure of hard liquor (that’s about a shot-glass worth), then the average person in Grenada drinks 438 measures of spirit per year. That’s far more than any other country — including Russia, where the average is 326 measures.
          Some of that data conforms to stereotypes. The French consume more wine than people in any other country — 370 glasses of wine per person per year, compared to just 84 glasses in the U.S. Remember though.")
  })
  }

# Create Shiny app object
shinyApp(ui = ui, server = server)
