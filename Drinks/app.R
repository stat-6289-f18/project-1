#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shiny", type="binary")
#install.packages('httpuv', type='binary')
update.packages("ggplot2")
library(shiny)
library(dplyr)
library(DT)
library(plotly)
library(ggplot2)
drinks_data <- read.csv("~/STAT 6289 FALL 18/Project 1/drinks_data.csv")


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Drinks Browser", windowTitle = "Drinks"),
   
   # Sidebar layout with input and output definitions
   sidebarLayout(
     
     #Inputs
      sidebarPanel(
        
        wellPanel(
          h3("Plotting")),      # Third level header: Plotting
        
        # Select variable for y-axis
        selectInput(inputId = "y", 
                    label = "Y-axis:",
                    choices = c("Beer Servings" = "beer_servings", 
                                "Spirit Servings" = "spirit_servings", 
                                "Wine Servings" = "wine_servings", 
                                "Total Liters of Pure Alcohol" = "total_litres_of_pure_alcohol"), 
                    selected = "wine_servings"),
        
        # Select variable for x-axis
        selectInput(inputId = "x", 
                    label = "X-axis:",
                    choices = c("Beer Servings" = "beer_servings", 
                                "Spirit Servings" = "spirit_servings", 
                                "Wine Servings" = "wine_servings", 
                                "Total Liters of Pure Alcohol" = "total_litres_of_pure_alcohol"), 
                    selected = "beer_servings"),
        
        # Test Input - Enter text for plot title
        textInput(inputId = "plot_title", 
                  label = "Plot title", 
                  placeholder = "Enter text for plot title that represents the Drink and Country you're selecting"),
        
        wellPanel(
          h3("Subsetting")),    # Third level header: Subsetting
        
        # Select which group of countries to plot
        checkboxGroupInput(inputId = "selected_type",
                           label = "Select Group of Countries:",
                           choices = c("East ", "Middle", "West"),
                           selected = "Middle")
        
        ),
        
      # Show a plot of the generated distribution
      mainPanel(
        
        tabsetPanel(type = "tabs",
                    # Tab 1: Plot
                    tabPanel(title = "Plot", 
                             plotOutput(outputId = "diagram"),
                             br(),
                             h5(textOutput("description"))),
                    # Tab 2: Data
                    tabPanel(title = "Data", 
                             br(),
                             DT::dataTableOutput(outputId = "drinkstable"))
                    
        )
      )
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Create a subset of data filtering for selected country location
  drinks_subset <- reactive({
    req(input$selected_type)
    filter(drinks_data, country %in% input$selected_type)
  })
  
  # Create scatterplot object the plotOutput function is expecting
  output$diagram <- renderPlot({
    ggplot(data = drinks_subset(), 
           aes_string(x = input$x, y = input$y)) +
      geom_point()
  })
  
    #Create Descriptive Text
    output$description <- renderText({
      paste0("The plot below titled ", input$plot_title, " visualizes the ", 
             input$selected_type, " region.")
    })
    
    # Show selected country location
    output$drinkstable <- DT::renderDataTable({
      new <- drinks_data %>%
        filter(country %in% input$selected_type) %>%
        group_by(country) %>%
        summarise(total_beer=sum(beer_servings), total_spirit=sum(spirit_servings), total_wine=sum(wine_servings), total_pure_alcohol=
                    sum(total_litres_of_pure_alcohol))
      DT::datatable(data = new, 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    })
    

}

# Run the application 
shinyApp(ui = ui, server = server)

