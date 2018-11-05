library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

Poll<-read_csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/early-senate-polls/early-senate-polls.csv"))


# Define UI for application that plots features of movies
ui <- fluidPage(
  
  theme = shinytheme("superhero"),
  
  # App title
  titlePanel("Early Senate Polls"),
  
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h2("Title Editing and Data Downloading"),
      
      # Enter text for plot title
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as plot title"),
      br(),
      
      # select filetype
      radioButtons(inputId = "filetype",
                   label = tags$strong("Select Download filetype:"),
                   choices = c("csv", "txt"),
                   selected = "csv"),
      
      # Horizontal line for visual separation
      hr(),
      h2("Variable Selection"),
      
      # Select variable for y-axis
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Election Resutl" = "election_result", 
                              "Presidential Approval" = "presidential_approval", 
                              "Poll Average" = "poll_average"), 
                  selected = "election_result"),
      
      # Single line break for a little bit of visual separation
      br(),
      
      # Select variable for x-axis
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Election Resutl" = "election_result", 
                              "Presidential Approval" = "presidential_approval", 
                              "Poll Average" = "poll_average"), 
                  selected = "poll_average"),
      
      
      # Single line break for a little bit of visual separation
      br(),
      
      # Select variable for color
      selectInput(inputId = "z", 
                  label = "Color by:",
                  choices = c("Election Resutl" = "election_result", 
                              "Presidential Approval" = "presidential_approval", 
                              "Poll Average" = "poll_average"), 
                  selected = "poll_average"),
      
      # Horizontal line for visual separation
      hr(), 
      h2("Time Selection"),
      
      # Select years to download
      checkboxGroupInput(inputId = "selected_year",
                         label = "Select years:",
                         choices = c("2006","2007","2008","2009","2010","2011","2012"),
                         selected = c("2006")),
      
      # Built with Shiny by RStudio
      br(), 
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "60px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "60px"),
         ".")
    ),
    
    
    # Output
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("scatterplot"),br(),
                           h5(textOutput("description"))),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"),tags$strong("Select filetype and variables, then download and/or view the data."),
                           br(), br(),
                           downloadButton(outputId = "download_data", label = "Download data"))
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output) {
  
  # Create reactive data frame
  years_selected <- reactive({
    req(input$selected_year)
    filter(Poll, Poll$year %in% input$selected_year)
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("recent_poll.",input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(countries_selected() %>% select(input$selected_var), path = file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(countries_selected() %>% select(input$selected_var), path = file) 
      }
    }
  )
  
  # Create the scatterplot object the plotOutput function is expecting
  output$scatterplot <- renderPlot({
    ggplot(data = years_selected(),  aes_string(x = input$x, y = input$y, color=input$z))+
      geom_point()+labs(x = x(),
                        y = y(),
                        color = toTitleCase(str_replace_all(input$z, "_", " ")),
                        title = toTitleCase(input$plot_title))
  })
  
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          x(),
          "and",
          y(),
          "for",
          nrow(years_selected()),
          "records")
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    summary(years_selected())
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    years_selected()
  })
  
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)

