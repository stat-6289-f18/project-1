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
library(tidyverse)

#data preprocessing

men=read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/marriage/men.csv"))
women=read.csv(url("https://raw.githubusercontent.com/fivethirtyeight/data/master/marriage/women.csv"))
men1 <- men %>% as_tibble %>% mutate(sex="Male")
women1 <- women %>% as_tibble %>% mutate(sex="Female")
marriage=rbind(men1,women1)%>%select(year,sex,all_2534,HS_2534,SC_2534,BAp_2534,BAo_2534,GD_2534,White_2534,Black_2534,Hisp_2534,NE_2534,MA_2534,Midwest_2534,South_2534,Mountain_2534,Pacific_2534,poor_2534,mid_2534,rich_2534,nokids_all_2534,kids_all_2534,work_2534,nowork_2534)

# Define UI for application that plots features of movies
ui <- fluidPage(theme = shinytheme("cerulean"),
                tags$head(
                  tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
                ),
                headerPanel("Shiny APP"),
                
                # App title
                titlePanel("Marriage, Age: 25 - 34, Year: 1960 - 2012", windowTitle = "Marriage"),
                
                # Sidebar layout with a input and output definitions
                sidebarLayout(
                  
                  # Inputs
                  sidebarPanel(
                    
                    h3("Plotting"),      # Third level header: Plotting
                    
                    # Select variable for y-axis 
                    selectInput(inputId = "y", 
                                label = "Y-axis:",
                                choices = c("Total" = "all_2534", 
                                            "High school graduate or less" = "HS_2534", 
                                            "Some college" = "SC_2534", 
                                            "Bachelor's degree or more" = "BAp_2534", 
                                            "Bachelor's degree, no graduate degre" = "BAo_2534", 
                                            "Graduate degree" = "GD_2534", 
                                            "Non-Hispanic white" = "White_2534", 
                                            "Black or African-American" = "Black_2534", 
                                            "Hispanic of any race" = "Hisp_2534", 
                                            "New England" = "NE_2534", 
                                            "Mid-Atlantic" = "MA_2534", 
                                            "Midwest" = "Midwest_2534", 
                                            "South" = "South_2534", 
                                            "Mountain West" = "Mountain_2534", 
                                            "Pacific" = "Pacific_2534", 
                                            "Family income in lowest 25%" = "poor_2534", 
                                            "Family income in middle 50%" = "mid_2534", 
                                            "Family income in top 25%" = "rich_2534", 
                                            "No own children living at home" = "nokids_all_2534", 
                                            "At least one own child living at home" = "kids_all_2534", 
                                            "Employed 50+ weeks prior year" = "work_2534", 
                                            "Not employed at least 50 weeks prior year" = "nowork_2534"), 
                                selected = "all_2534"),
                    helpText("Select people's characteristic"),
                    
                    # Select variable for x-axis 
                    selectInput(inputId = "x", 
                                label = "X-axis:",
                                choices = "year", 
                                selected = "year"),
                    
                    # Enter text for plot title
                    textInput(inputId = "plot_title", 
                              label = "Plot title", 
                              placeholder = "Enter text to be used as plot title"),
                    
                    hr(),
                    
                    h3("Subsetting"),    # Third level header: Subsetting
                    
                    # Select which types of movies to plot
                    checkboxGroupInput(inputId = "selected_type",
                                       label = "Select sex:",
                                       choices = c("Male", "Female"),
                                       selected = "Male"),
                    
                    hr(),
                    
                    # Show data table
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table",
                                  value = FALSE)
                  ),
                  
                  
                  # Output:
                  mainPanel(
                    
                    tabsetPanel(type = "tabs",
                                id = "tabsetpanel",
                                tabPanel(title = "Plot",
                                         tags$strong("Download the selected data"),
                                         downloadButton(outputId = "marriage_download", label = "Download data"),
                                         plotOutput(outputId = "scatterplot"),
                                         br(),
                                         h5(textOutput("description"))),
                                tabPanel("Summary", 
                                         tags$strong("Summary for selected sex"),
                                         br(),
                                         verbatimTextOutput("summary")),
                                tabPanel(title = "Data", 
                                         tags$style(),
                                         br(),
                                         DT::dataTableOutput(outputId = "marriagetable"))#,
                                
                    )
                  )
                )
)

# Define server function required to create the scatterplot
server <- function(input, output, session) {
  
  # Create a subset of data filtering for selected title types
  marriage_selected <- reactive({
    req(input$selected_type) # ensure availablity of value before proceeding
    filter(marriage, sex %in% input$selected_type)
  })
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplot(data = marriage_selected(), aes_string(x = input$x, y = input$y,color="sex")) +
      geom_point() +
      labs(x = x(),
           y = y(),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = toTitleCase(input$plot_title))
  })
  
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows never married ratio changed from year when people satisfy the condition which is  ",
          y())
  })
  
  # Print data table if checked
  output$marriagetable <- DT::renderDataTable(
    DT::datatable(data = marriage_selected(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  )
  # summary table
  output$summary <- renderPrint({
    summary(marriage_selected())
  })
  
  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
    } else {
      hideTab(inputId = "tabsetpanel", target = "Data")
    }
  })
  
  # Download file
  output$marriage_download <- downloadHandler(
    filename = "marriage",
    content = function(file) {
      write_csv(marriage_selected(), path = file) 
    }
  )
  
  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)
