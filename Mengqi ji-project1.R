library(shiny)
library(ggplot2)
library(DT)
library(tools)
library(stringr)
library(tidyverse)
library(dplyr)

library(readr)
drinks <- read_csv("drinks.csv")


ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # Input:
      #select beer,spirit, wine for x-axis 
      selectInput(inputId = "x", 
                  label = "Choose one type of drinks as x-axis:",
                  choices = c("beer"="beer_servings",
                              "spirit"="spirit_servings",
                              "wine"="wine_servings"), 
                  selected = "beer_servings"),
      hr(), 
      # Select beer,spirit, wine for y-axis
      selectInput(inputId = "y", 
                   label = "Choose one type of drinks as y-axis:",
                   choices = c("beer"="beer_servings",
                               "spirit"="spirit_servings",
                               "wine"="wine_servings"), 
                   selected = "beer_servings"),
      hr(), 
      # Enter text for plot title
      textInput(inputId = "title", 
                label = "Please enter the tittle name:", 
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
                  # Tab 1: Scatterplot
                  tabPanel(title = "What do People Drink The Most: Beer, Wine or Spirits? ", 
                           plotOutput(outputId = "scatterplot"),
                           br(),
                           h5(textOutput("description"))),
                  # Tab 2: Data Table
                  tabPanel(title = "Top 10 countries by servings consumed per person, 2010", 
                           br(),        #",toTitleCase(input$title),"
                           DT::dataTableOutput(outputId = "toptable"),
                           br(),
                           h5(textOutput("description2")))
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  drink= as.data.frame(drinks[,1:4])
  beer=drink[,1:2]  
  beer2=head(beer[order(beer[,2],decreasing=T),c(1,2)],10)
  spirit=drink[,c(1,3)]
  spirit2=head(spirit[order(spirit[,2],decreasing = T),c(1,2)],10) 
  wine=drink[,c(1,4)]
  wine2=head(wine[order(wine[,2],decreasing=T),c(1,2)],10)
  a=c(unlist(beer2),unlist(spirit2),unlist(wine2))
  b=matrix(a,10,6)
  c=as.data.frame(b)

  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  #create the plot 
  output$scatterplot <- renderPlot({
    ggplot(data=drink, aes_string(x = input$x, y = input$y))+
      geom_point()+
      labs(x = x(),
           y = y())
  })
  
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          x(),
          "and",
          y(),
          "for",
          nrow(drink),
          "countries worldwide.")
  })
  
  # Print data table
  output$toptable <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = drink, 
                    options = list(pageLength = 10), 
                    rownames =FALSE)
                      #c("Country","Beer","Country","Spirit","Country","Wine"))
    })
  
  
  # Create description of plot
  output$description2 <- renderText({
    print("Namibia is the world’s biggest beer-drinking country, with 376 12-ounce cans of beer consumed per person. In Germany, the average is 346 cans, and in the U.S. it’s 249 cans.
          If you assume that 1.5 fluid ounces is a normal measure of hard liquor (that’s about a shot-glass worth), then the average person in Grenada drinks 438 measures of spirit per year. That’s far more than any other country — including Russia, where the average is 326 measures.
          Some of that data conforms to stereotypes. The French consume more wine than people in any other country — 370 glasses of wine per person per year, compared to just 84 glasses in the U.S. Remember though.")
  })
  }

# Create Shiny app object
shinyApp(ui = ui, server = server)
