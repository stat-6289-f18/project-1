library(shiny)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tidyverse)
library(tools)


airlines <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/airline-safety/airline-safety.csv")

airlines <- airlines %>%
  mutate(total_accidents = incidents_85_99 + fatal_accidents_85_99 + fatalities_85_99 + incidents_00_14 + fatal_accidents_00_14 + fatalities_00_14)

# Inputs

ui <- fluidPage(
  
  titlePanel("Airline Accidents, 1985 - 2014", windowTitle = "Airlines"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h3("Downloading"),
      
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv","tsv"),
                   selected = "csv"),
      
    
    h3("Plotting"),
    
    selectInput(inputId = "y",
                label = "Y-axis:",
                choices = c("Available seat kilometers flown every week"="avail_seat_km_per_week",
                            "Total number of incidents from 1985-1999"="incidents_85_99",
                            "Total number of fatal accidents from 1985-1999"="fatal_accidents_85_99",
                            "Total number of fatalities from 1985-1999"="fatalities_85_99",
                            "Total number of incidents from 2000-2014"="incidents_00_14",
                            "Total number of fatal accidents from 2000-2014"="fatal_accidents_00_14",
                            "Total number of fatalities from 2000-2014"="fatalities_00_14"),
                selected = "avail_seat_km_per_week"),
    
    selectInput(inputId = "x",
                label = "X-axies:",
                choices = c("Airline"="airline",
                            "Available seat kilometers flown every week"="avail_seat_km_per_week",
                            "Total number of incidents from 1985-1999"="incidents_85_99",
                            "Total number of fatal accidents from 1985-1999"="fatal_accidents_85_99",
                            "Total number of fatalities from 1985-1999"="fatalities_85_99",
                            "Total number of incidents from 2000-2014"="incidents_00_14",
                            "Total number of fatal accidents from 2000-2014"="fatal_accidents_00_14",
                            "Total number of fatalities from 2000-2014"="fatalities_00_14"),
                selected = "airline"),
    
    sliderInput(inputId = "alpha",
                label = "Alpha:",
                min=0, max=1,
                value = 0.5)
  ),
  
  hr(),
  
  h3("Subsetting"),
  
  checkboxGroupInput(inputId = "selected_var",
                     label = "Select variables:",
                     choices = levels(airlines$airline),
                     selected = levels(airlines$airline)
  ),
  
  wellPanel(
    checkboxInput(inputId = "show_data",
                  label = "Show data table",
                  value = TRUE))
  ),
  
  # Output(s)
  
  mainPanel(
    tabsetPanel(type = "tabs",
                tabPanel("Plot",plotOutput(outputId = "scatterplot")),
                tabPanel("Summary",tableOutput(outputId = "summarytable")),
                tabPanel("Data",DT::dataTableOutput(outputId = "airlinestable"))
                ),
    
              
    HTML("Select filetype and variable, then download and/or view the data."),
    br(), br(),
    downloadButton("download_data","Download data"),
    br(), br(),
    DT::dataTableOutput(outputId = "airlinestable"),
    
    h3("Scatterplot"),
    plotOutput(outputId = "scatterplot"),
    textOutput(outputId = "avg_x"),
    textOutput(outputId = "avg_y"),
    verbatimTextOutput(outputId = "lmoutput"),
    tableOutput(outputId = "summarytable")
  )
  
)



#Server
server <- function(input, output){
  
  airlines_selected <- reactive({
    req(input$selected_var)
    airlines %>% select(input$selected_var)
  })
  
  output$airlinestable <- DT:: renderDataTable({
    DT::datatable(data = airlines_selected(),
                  options = list(pageLength = 10),
                  rownames = FALSE)
  })
  
  output$download_data <- downloadHandler(
    filename = function(){
      paste0("airlines.", input$filetype)
    },
    content = function(file) {
      if(input$filetype == "csv"){
        write_csv(airlines %>% select(input$selected_var), path = file)
      }
      if(input$filetype == "tsv") {
        write_tsv(airlines %>% select(input$selected_var), path = file)
      }
    }
  )
  
  output$scatterplot <- renderPlot({
    ggplot(data = airlines, aes_string(x = input$x, y = input$y)) +
      geom_point(alpha=input$alpha)
  })
  
  output$avg_x <- renderText({
    avg_x <- airlines %>% pull(input$x) %>% mean() %>% round(2)
    paste("Average", input$x, "=", avg_x)
  })
  
  output$avg_y <- renderText({
    avg_y <- airlines %>% pull(input$y) %>% mean() %>% round(2)
    paste("Average", input$y, "=", avg_y)
  })
  
  output$lmoutput <- renderPrint({
    x <- airlines %>% pull(input$x)
    y <- airlines %>% pull(input$y)
    summ <- summary(lm(y ~ x, data = airlines))
    print(summ, digits = 3, signif.stars = FALSE)
  })
  
  output$summarytable <- renderTable({
    airlines %>%
      filter(airlines %in% input$selected_airlines) %>%
      group_by(avail_seat_km_per_week) %>%
      summarise(Mean = mean(total_accidents), SD = sd(total_accidents), n = n()) },
    striped = TRUE, spacing = "l", align = "lccr", digits = 4, width = "90%",
    caption = "Fatality ratio (fatalities / all accidents) summary statistics by available seat km flown per week")
  
}

shinyApp(ui = ui, server = server)
