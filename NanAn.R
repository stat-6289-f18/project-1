install.packages("shiny")
install.packages("shinythemes")
install.packages("DT")
install.packages("dplyr")
install.packages("readr")

library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)

NBA_draft = read.csv("E:/6289 R Programming/Project One/historical_projections.csv")
NBA_draft_Codebook = read.csv("E:/6289 R Programming/Project One/Codebook.csv")

# Define UI for application that plots features of nba_draft
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                # App title
                titlePanel("NBA draft projection model, 2001-2015", windowTitle = "NBA Draft"),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    
                    h3("Plotting"),      # Third level header: Plotting
                    
                    # Select variable for y-axis 
                    selectInput(inputId = "y", 
                                label = "Y-axis:",
                                choices = c("Draft Year" = "Draft.Year", 
                                            "Projected SPM" = "Projected.SPM", 
                                            "Superstar" = "Superstar", 
                                            "Starter" = "Starter", 
                                            "Role Player" = "Role.Player",
                                            "Bust" = "Bust"), 
                                selected = "Superstar"),
                    
                    # Select variable for x-axis 
                    selectInput(inputId = "x", 
                                label = "X-axis:",
                                choices = c("Draft Year" = "Draft.Year", 
                                            "Projected SPM" = "Projected.SPM", 
                                            "Superstar" = "Superstar", 
                                            "Starter" = "Starter", 
                                            "Role Player" = "Role.Player",
                                            "Bust" = "Bust"), 
                                selected = "Projected.SPM"),
                    
                    # Enter text for plot title
                    textInput(inputId = "plot_title", 
                              label = "Plot title", 
                              placeholder = "Enter text to be used as plot title"),
                    
                    hr(),
                    
                    h3("Subsetting"),    # Third level header: Subsetting
                    
                    # Select which position of players to plot
                    checkboxGroupInput(inputId = "selected_position",
                                       label = "Select player's position(s):",
                                       choices = c("PG", "SG", "SF","PF","C"),
                                       selected = "PG"),
                    
                    hr(),
                    
                    # Show data table
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table",
                                  value = TRUE),
                    
                    # Built with Shiny by RStudio
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                       "in",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                       "by Nan An."
                    )
                    
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
                                         DT::dataTableOutput(outputId = "NBAdrafttable")),
                                # New tab panel for Codebook
                                tabPanel("Codebook", 
                                         br(),
                                         dataTableOutput(outputId = "codebook"))
                    )
                  )
                )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected Positions
  NBAdraft_selected <- reactive({
    req(input$selected_position) # ensure availablity of value before proceeding
    filter(NBA_draft, Position %in% input$selected_position)
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplot(data = NBAdraft_selected(), aes_string(x = input$x, y = input$y)) +
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
          nrow(NBAdraft_selected()),
          "Players in NBA Draft.")
  })
  
  # Print data table if checked
  output$NBAdrafttable <- DT::renderDataTable(
    DT::datatable(data = NBAdraft_selected()[, 1:9], 
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
  
  # Render data table for codebook
  output$codebook <- renderDataTable({
    datatable(data = NBA_draft_Codebook,
              options = list(pageLength = 10, lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)