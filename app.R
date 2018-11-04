#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)


dataset<- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/airline-safety/airline-safety.csv")

per_capital_GDP_of_airlines_home_country <- c(34.620,24.970,25.595,25.876,22.205,21.779,34.620,26.056,25.876,21.441,35.014,12.445,25.234,34.620,4.952,26.598,25.848,34.620,34.620,31.447,15.176,25.595,4.074,34.620,7.970,19.577,24.930,35.014,3.942,3.029,6.073,38.339,1.989,0.889,21.258,29.207,9.906,2.102,34.620,3.414,0.861,10.026,0.375,0.460,8.360,3.455,1.407,1.081,2.429,0.442,1.451,0.687,13.535,0.579,0.122,1.334)

safety_score_85_99 <- c(0.99,0.91,0.80,0.90,0.73,0.77,0.37,0.46,0.57,0.60,0.57,0.51,0.42,0.47,0.12,0.35,0.26,0.40,-0.16,0.03,0.39,0.00,-0.05,0.39,0.03,0.04,0.15,-0.45,0.11,-0.24,-0.06,-0.31,-0.54,-0.08,-0.34,0.28,-0.98,-0.20,-0.59,-0.13,-0.66,0.32,-0.98,-0.50,-0.87,0.26,-1.11,-1.29,-1.86,0.23,-1.49,-1.42,-2.45,-1.90,-2.74,-4.59)

safety_score_00_14 <- c(0.82,0.86,0.96,0.85,0.73,0.65,0.98,0.76,0.62,0.58,0.57,0.51,0.47,0.41,0.62,0.35,0.40,0.26,0.79,0.46,0.06,0.44,0.49,0.00,0.36,0.34,0.16,0.74,0.17,0.47,0.22,0.42,0.57,0.10,0.33,-0.31,0.74,-0.05,0.34,-0.16,0.32,-0.77,0.46,-0.19,0.07,-1.10,0.15,0.04,0.43,-1.71,-0.01,-0.21,-0.32,-1.07,-0.64,0.08)

safety_score_85_14 <- round((safety_score_85_99+safety_score_00_14)/2,2)

GDP <- c("Rich",rep("Medium",5),"Rich",rep("Medium",3),"Rich","Poor","Medium","Rich","Poor",rep("Medium",2),rep("Rich",3),rep("Medium",2),"Poor","Rich","Poor",rep("Medium",2),"Rich",rep("Poor",3),"Rich",rep("Poor",2),rep("Medium",2),rep("Poor",2),"Rich",rep("Poor",17))

safety <- cbind(dataset,per_capital_GDP_of_airlines_home_country,GDP,safety_score_85_99,safety_score_00_14,safety_score_85_14)


ui <- fluidPage(theme = shinytheme("united"),
                
                
                titlePanel("Airlines safety, 1985 - 2014", windowTitle = "Airlines"),
                
                
                sidebarLayout(
                  
                  
                  sidebarPanel(
                    
                    h3("Plotting"),      # Third level header: Plotting
                    
                    
                    
                    
                    
                    selectInput(inputId = "y", 
                                label = "Y-axis:",
                                choices = c("Incidents 1985-1999" = "incidents_85_99", 
                                            "Fatal accidents 1985-1999" = "fatal_accidents_85_99", 
                                            "Fatalities 1985-1999" = "fatalities_85_99", 
                                            "Available seat km per week" = "avail_seat_km_per_week",
                                            "Incidents 2000-2014" = "incidents_00_14", 
                                            "Fatal accidents 2000-2014" = "fatal_accidents_00_14", 
                                            "Fatalities 2000-2014" = "fatalities_00_14",
                                            "Per capital GDP of airline's home country "="per_capital_GDP_of_airlines_home_country",
                                            "Safety score from 1985 to 1999"="safety_score_85_99",
                                            "Safety score from 2000 to 2014"="safety_score_00_14",
                                            "Safety score from 1985 to 2014"="safety_score_85_14"), 
                                selected = "incidents_00_14"),
                    
                    
                    
                    selectInput(inputId = "x", 
                                label = "X-axis:",
                                choices = c("Incidents 1985-1999" = "incidents_85_99", 
                                            "Fatal accidents 1985-1999" = "fatal_accidents_85_99", 
                                            "Fatalities 1985-1999" = "fatalities_85_99", 
                                            "Available seat km per week" = "avail_seat_km_per_week",
                                            "Incidents 2000-2014" = "incidents_00_14", 
                                            "Fatal accidents 2000-2014" = "fatal_accidents_00_14", 
                                            "Fatalities 2000-2014" = "fatalities_00_14",
                                            "Per capital GDP of airline's home country "="per_capital_GDP_of_airlines_home_country",
                                            "Safety score from 1985 to 1999"="safety_score_85_99",
                                            "Safety score from 2000 to 2014"="safety_score_00_14",
                                            "Safety score from 1985 to 2014"="safety_score_85_14"), 
                                selected = "incidents_85_99"),
                    
                    
                    textInput(inputId = "plot_title", 
                              label =  "Plot title", 
                              placeholder = "Enter text to be used as plot title"),
                    
                    hr(),
                    
                    h3("Conclusion"),
                    
                    hr(),
                    
                    actionButton("words", "Check"),
                    hr(),
                    textOutput("plot"),
                    
                    hr(),
                    
                    h3("Subsetting"),    # Third level header: Subsetting
                    
                    
                    checkboxGroupInput(inputId = "selected_type",
                                       label = "Select GDP level of airline's home country type(s):",
                                       choices = c("Rich","Medium","Poor"),
                                       selected =  "Medium"),
                    
                    hr(),
                    
                    
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table",
                                  value = TRUE),
                    
                    
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                       ".")
                    
                  ),
                  
                  
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                id = "tabsetpanel",
                                tabPanel(title = "Plot", 
                                         plotOutput(outputId = "scatterplot"),
                                         br(),
                                         h5(textOutput("description"))),
                                tabPanel(title = "Data", 
                                         br(),
                                         DT::dataTableOutput(outputId = "safetytable")),
                                tabPanel("Summary", 
                                         br(),
                                         dataTableOutput(outputId = "summary"))         
                                
                    )
                  )
                )
)


server <- function(input, output, session) {
  
  
  GDP_selected <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(safety, GDP %in% input$selected_type)
  })
  
  
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  
  output$scatterplot <- renderPlot({
    ggplot(data = GDP_selected(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(x = x(),
           y = y(),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = toTitleCase(input$plot_title))
  })
  
  
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          x(),
          "and",
          y(),
          "for",
          nrow(GDP_selected()),
          "airlines.")
  })
  
  
  output$safetytable <- DT::renderDataTable(
    DT::datatable(data = GDP_selected()[, 1:13], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
  
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
    } else {
      hideTab(inputId = "tabsetpanel", target = "Data")
    }
  })
  
  
  output$summary <- renderDataTable({
    datatable(data = summary(safety),
              options = list(pageLength = 10, lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })
  
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$words, {
    v$data <- safety[c(2:9,11:13)]
    
  })
  
  output$plot <- renderText({
    if (is.null(v$data)) return()
    paste("The plots shown above reveal that an airline’s track record tells you something about its probability of future crashes — although not a lot, and only if looked at in the right way. In particular, you should look toward an airline’s rate of dangerous incidents of any kind rather than its number of fatalities or fatal accidents. These near-misses are more consistent from period to period — and could result in a deadly crash the next time around. A better rule to follow is that if you’re insistent on minimizing your crash risk, you should avoid airlines from developing countries. ")
    
  })
  
  
}


shinyApp(ui = ui, server = server) 
