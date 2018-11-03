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
library(RCurl)

score<-read_csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/fandango/fandango_score_comparison.csv"))

# Define UI for application that plots features of movies
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  # App title
  titlePanel("Movie score", windowTitle = "Movies"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h3("Plotting"),      # Third level header: Plotting
      
      # Select variable for y-axis 
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Rotten Tomatoes user score" = "RottenTomatoes_User", 
                              "Metacritic user score" = "Metacritic_User", 
                              "IMDb user score" = "IMDB", 
                              "Fandango Rating Value" = "Fandango_Ratingvalue", 
                              "number of user votes the film had on IMDb" = "IMDB_user_vote_count",
                              "number of user votes the film had on Fandango" = "Fandango_votes"), 
                  selected = "Metacritic_User"),
      
      # Select variable for x-axis 
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Rotten Tomatoes user score" = "RottenTomatoes_User", 
                              "Metacritic user score" = "Metacritic_User", 
                              "IMDb user score" = "IMDB", 
                              "Fandango Rating Value" = "Fandango_Ratingvalue", 
                              "number of user votes the film had on IMDb" = "IMDB_user_vote_count",
                              "number of user votes the film had on Fandango" = "Fandango_votes"), 
                  selected = "Fandango_votes"),
      
      # Enter text for plot title
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as plot title"),
      
      actionButton(inputId = "update_plot_title", 
                   label = "Update plot title"),
      
      
      hr(),
      
      h3("Selecting"),    # Third level header: Subsetting
      
      # Select which types of movies to plot
      checkboxGroupInput(inputId = "Fandango_Stars",
                         label = "number of stars the film had on Fandango",
                         choices = c("5", "4.5", "4", "3.5"),
                         selected = "4.5"),
      
      hr(),
      
      # Show data table
      checkboxInput(inputId = "show_data",
                    label = "Show data table",
                    value = TRUE),
      
      # Built with Shiny by RStudio
      br(), br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
      
    ),
    
    # Output:
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "scatterplot"),
                           br(),
                           h5(textOutput("description"))),
                  tabPanel(title = "Data", 
                           br(),
                           DT::dataTableOutput(outputId = "moviestable"))
      )
    )
  )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  moviestar_selected <- reactive({
    req(input$Fandango_Stars) # ensure availablity of value before proceeding
    filter(score, Fandango_Stars %in% input$Fandango_Stars)
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplot(data = moviestar_selected(), aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(x = x(),
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
          nrow(moviestar_selected()),
          "movies.")
  })
  
  # Print data table if checked
  output$moviestable <- DT::renderDataTable(
    DT::datatable(data = moviestar_selected()[, 1:6], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  
  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
    } else {
      hideTab(inputId = "tabsetpanel", target = "Data")
    }
  })
  
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
