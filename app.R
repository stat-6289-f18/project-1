library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

projections <- read_csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/nba-draft-2015/historical_projections.csv")
n_total <- nrow(projections)

# Define UI for application that plots features of movies
ui <- fluidPage(
  
   theme = shinytheme("readable"),
   
  # App title
  titlePanel("Projecting The Top 50 Players In The 2015 NBA Draft Class", windowTitle = "NBA Draft 2015"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      h3("Sample size"),
      
      ## Text instructions
      HTML(paste("Enter a value between 1 and", n_total)),
      
      # Numeric input for sample size
      numericInput(inputId = "n",
                   label = "Sample size:",
                   value = 1090,
                   step = 1,
                   min=1,max=n_total),
      
      actionButton(inputId = "sampletable",
                   label = "Select the sample size"),

    
        # Break for visual separation
      br(), br(),
      hr(),
      
      h3("Plotting"),      # Third level header: Plotting
      
      # Select variable for y-axis 
      selectInput(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("Probability of becoming a superstar player" = "Superstar", 
                              "Probability of becoming a starting-caliber player" = "Starter", 
                              "Probability of becoming a bust"= "Bust"), 
                  selected = "Starter"),
      
      # Select variable for x-axis 
      selectInput(inputId = "x", 
                  label = "X-axis:",
                  choices = c("Probability of becoming a superstar player" = "Superstar", 
                              "Probability of becoming a starting-caliber player" = "Starter", 
                              "Probability of becoming a bust"= "Bust"), 
                  selected = "Superstar"),
      
      # Enter text for plot title
      textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as plot title"),
      
      
      hr(),
      
      h3("Subsetting"),    # Third level header: Subsetting
      
      # Select which positions of players to plot
      checkboxGroupInput(inputId = "Position",
                         label = "Select position for the player:",
                         choices = c("C","PG","PF","SF","SG"),
                         selected = "C"),
      
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
    
    # Output: Show data table
    mainPanel(
    
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "Sample data", 
                           br(),
                           DT::dataTableOutput(outputId = "sampletable")),
                  tabPanel(title = "Plot", 
                           plotOutput(outputId = "scatterplot",brush = "plot_brush"),
                           br(),
                           h5(textOutput("description"))),
                  tabPanel(title = "Data", 
                           br(),
                           DT::dataTableOutput(outputId = "playerstable"))
                  
      )
    )
  )
)



# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  position_selected <- reactive({
    req(input$Position) # ensure availablity of value before proceeding
    filter(projections, Position %in% input$Position)
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplot(data = position_selected(), aes_string(x = input$x, y = input$y)) +
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
          nrow(position_selected()),
          "players.")
  })
  
  ### Create data table
  output$sampletable <- DT::renderDataTable({
    projections_sample <- projections %>%
      sample_n(input$n) %>%
      select(Player:Bust)
    DT::datatable(data = projections_sample, 
                  options = list(pageLength = 20), 
                  rownames = FALSE)
  })
  
  # Print data table if checked
  output$playerstable <- DT::renderDataTable({
    brushedPoints(projections,input$plot_brush) %>% 
    DT::datatable(data = position_selected()[, 1:9], 
                  options = list(pageLength = 20), 
                  rownames = FALSE)
  })
  
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