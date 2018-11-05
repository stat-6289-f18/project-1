library(readr)
airlines<-read_csv("/Users/apple/airline-safety.csv")
airlines2<-airlines[,-1]
library(shiny)
library(markdown)
library(DT)
ui<-
  tabsetPanel(
    tabPanel("Plot",
             sidebarLayout(
               pageWithSidebar(
                 
                 headerPanel('Analysis on airlines accidents'),
                 
                 sidebarPanel(
                   
                   textInput("txt", "title", "1-1 Relationship Between Variables"),
                   
                   selectInput('xcol', 'X Variable', names(airlines2)),
                   
                   selectInput('ycol', 'Y Variable', names(airlines2),
                               selected=names(airlines2)[[2]]),
                   
                   radioButtons("plotType", "Plot type",
                                c("Scatter"="p", "Histogram"="h"))
                 ),
                 
                 mainPanel(
                   
                   verbatimTextOutput("txtout"),
                   
                   plotOutput('plot1')
                 )
               ),
               
               mainPanel(
                 
                 plotOutput("plot")
               )
             )
    ),
    tabPanel("Summary and Data",
             
             sidebarLayout(
               
               # Sidebar panel for inputs ----
               sidebarPanel(
                 
                 # Input: Numeric entry for number of obs to view ----
                 numericInput(inputId = "obs",
                              
                              label = "Number of observations to view:",
                              
                              value = 5)
               ),
               
               # Main panel for displaying outputs ----
               mainPanel(
                 
                 # Output: Verbatim text for data summary ----
                 verbatimTextOutput("summary"),
                 
                 # Output: HTML table with requested number of observations ----
                 tableOutput("view")
                 
               )
    )
    )
  )


server<-function(input, output, session) {
  selectedData <- reactive({
    airlines[, c(input$xcol, input$ycol)]
  })
  
  output$plot1 <- renderPlot({
    
    plot(selectedData(),
         type=input$plotType,
         
         pch = 20, cex = 3)
    par(mar = c(0, 0, 0, 0))
  })
  
  output$txtout <- renderText({
    paste(input$txt)
  })
  
  output$summary <- renderPrint({
    summary(airlines)
  })
  output$view <- renderTable({
    head(airlines, n = input$obs)
  })  
}


shinyApp(ui = ui, server = server)
