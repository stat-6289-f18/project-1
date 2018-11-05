library(shiny)
library(shinythemes)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
soccer <- read_csv("https://projects.fivethirtyeight.com/soccer-api/international/2018/world_cup_comparisons.csv")

#--------------------------------------------------------------------------------#
# Define UI for application that plots features of soccer
#--------------------------------------------------------------------------------#

ui <- fluidPage(theme = shinytheme("slate"),
                # App title
                titlePanel("World Cup, 1966 - 2018", windowTitle = "Soccer"),
                
#--------------------------------------------------------------------------------#
                # Sidebar layout with a input and output definitions
#--------------------------------------------------------------------------------#
                sidebarLayout(
                  #--------------------------------------------------------------------------------#
                  # Inputs
                  #--------------------------------------------------------------------------------#
                  sidebarPanel(
                    h3("Plotting"),      # Third level header: Plotting
                    #--------------------------------------------------------------------------------#
                    # Select variable for y-axis 
                    selectInput(inputId = "y", 
                                label = "Y-axis:",
                                choices = c("Goals" = "goals_z", 
                                            "Expected goals" = "xg_z", 
                                            "Crosses" = "crosses_z",
                                            "Touches in box" = "boxtouches_z",
                                            "Passes" = "passes_z", 
                                            "Progpasses" = "progpasses_z",
                                            "Take-ons" = "takeons_z",
                                            "Progruns" = "progruns_z",
                                            "Tackles" = "tackles_z",
                                            "Interceptions" = "interceptions_z",
                                            "Clearances" = "clearances_z",
                                            "Blocks" = "blocks_z",
                                            "Aerials" = "aerials_z",
                                            "Fouls" = "fouls_z",
                                            "Fouled" = "fouled_z",
                                            "Nsxg" = "nsxg_z"), 
                                selected = "fouled_z"),
                    #--------------------------------------------------------------------------------#
                    # Select variable for x-axis 
                    selectInput(inputId = "x", 
                                label = "X-axis:",
                                choices = c("Goals" = "goals_z", 
                                            "Expected goals" = "xg_z", 
                                            "Crosses" = "crosses_z",
                                            "Touches in box" = "boxtouches_z",
                                            "Passes" = "passes_z", 
                                            "Progpasses" = "progpasses_z",
                                            "Take-ons" = "takeons_z",
                                            "Progruns" = "progruns_z",
                                            "Tackles" = "tackles_z",
                                            "Interceptions" = "interceptions_z",
                                            "Clearances" = "clearances_z",
                                            "Blocks" = "blocks_z",
                                            "Aerials" = "aerials_z",
                                            "Fouls" = "fouls_z",
                                            "Fouled" = "fouled_z",
                                            "Nsxg" = "nsxg_z",
                                            "player" = "player",
                                            "team" = "team"), 
                                selected = "goals_z"),
                    #--------------------------------------------------------------------------------#  
                    # Select variable for color
                    
                    # selectInput(inputId = "z", 
                    #             label = "Color by:",
                    #             choices = c("goals_z","xg_z","crosses_z","passes_z","tackles_z",
                    #                         "interceptions_z","clearances_z","blocks_z","fouls_z","fouled_z"),
                    #             selected = "goals_z"),
                    
                    selectInput(inputId = "z", 
                                label = "Color by:",
                                choices = c("season"),
                                selected = "season"),
                    
                    #--------------------------------------------------------------------------------#  
                    # Set alpha level
                    sliderInput(inputId = "alpha", 
                                label = "Alpha:", 
                                min = 0, max = 1, 
                                value = 0.5),
                    
                    #--------------------------------------------------------------------------------#  
                    # Set point size
                    sliderInput(inputId = "size", 
                                label = "Size:", 
                                min = 0, max = 5, 
                                value = 2),
                    
                    #--------------------------------------------------------------------------------#  
                    # Enter text for plot title
                    textInput(inputId = "plot_title", 
                              label = "Plot title", 
                              placeholder = "Enter text to be used as plot title"),
                    #--------------------------------------------------------------------------------# 
                    hr(),
                    #--------------------------------------------------------------------------------#  
                    h3("Subsetting"),    # Third level header: Subsetting
                    #--------------------------------------------------------------------------------#    
                    # Select which types of movies to plot
                    checkboxGroupInput(inputId = "selected_season",
                                       label = "Select year(s):",
                                       choices = c(seq(from = 2018, to = 1966, by = -4)),
                                       selected = c("2018","2014")),
                    hr(),
                    #--------------------------------------------------------------------------------#
                    # Show data table
                    checkboxInput(inputId = "show_data",
                                  label = "Show data table",
                                  value = TRUE),
                    #--------------------------------------------------------------------------------#  
                    # Built with Shiny by RStudio
                    br(), 
                    h5("Built by Yu Xiong"),
                    
                    hr(),
                    #--------------------------------------------------------------------------------#
                    # Select filetype
                    radioButtons(inputId = "filetype",
                                 label = "Select filetype:",
                                 choices = c("csv", "tsv"),
                                 selected = "csv"),
                    #--------------------------------------------------------------------------------#
                    # Select variables to download
                    checkboxGroupInput(inputId = "selected_var",
                                       label = "Select variables:",
                                       choices = names(soccer),
                                       selected = c("title")),
                    
                    br(), br(),
                    h5("Built with",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                       "by",
                       img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                       ".")
                  ),
                  #--------------------------------------------------------------------------------#             
                  # Output:
                  #--------------------------------------------------------------------------------#
                  mainPanel(
                    tabsetPanel(type = "tabs",
                                id = "tabsetpanel",
                                #--------------------------------------------------------------------------------#  
                                tabPanel(title = "Plot", 
                                         plotOutput(outputId = "scatterplot"),
                                         br(),
                                         h5(textOutput("description"))),
                                #--------------------------------------------------------------------------------#  
                                tabPanel(title = "Data", 
                                         br(),
                                         DT::dataTableOutput(outputId = "moviestable")),
                                #--------------------------------------------------------------------------------#  
                                # New tab panel for Codebook
                                # tabPanel("Codebook", 
                                #          br(),
                                #          dataTableOutput(outputId = "codebook")),
                                #--------------------------------------------------------------------------------#  
                                # tabPanel(title = "Plot2", 
                                #          plotOutput(outputId = "scatterplot2"),
                                #          br(),
                                #          h5(textOutput("description2"))),
                                #--------------------------------------------------------------------------------#
                                tabPanel(HTML("Download data"),
                                br(), br(), # line break and some visual separation
                                downloadButton("download_data", "Download data"),
                                br(), br(),
                                DT::dataTableOutput(outputId = "soccertable1")
                    )
                  )
                )))

#--------------------------------------------------------------------------------#  
# Define server function required to create the scatterplot
#--------------------------------------------------------------------------------#  
server <- function(input, output, session) {
  #--------------------------------------------------------------------------------#  
  # Create a subset of data filtering for selected title types
  soccer_selected <- reactive({
    req(input$selected_season) # ensure availablity of value before proceeding
    filter(soccer, season %in% input$selected_season)
  })
  #--------------------------------------------------------------------------------#
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  #--------------------------------------------------------------------------------#
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplot(data = soccer_selected(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(alpha = input$alpha, size = input$size) +
      geom_smooth() +
      labs(x = x(),
           y = y(),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = toTitleCase(input$plot_title))
  })
  
  #--------------------------------------------------------------------------------#
  # Create data table
  output$soccertable1 <- DT::renderDataTable({
    DT::datatable(data = soccer_selected(), 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
  })
  
  #--------------------------------------------------------------------------------#
  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("soccer.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(soccer %>% select(input$selected_var), path = file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(soccer %>% select(input$selected_var), path = file) 
      }
    }
  )
  
  # output$scatterplot <- renderPlot({
  #   ggplot(data = soccer, aes_string(x = input$x, y = input$y, color = input$z)) +
  #     geom_point(size = 1) +
  #     geom_smooth() +
  #     labs(x = x(),
  #          y = y(),
  #          color = toTitleCase(str_replace_all(input$z, "_", " ")))
  # })
  
  #--------------------------------------------------------------------------------#
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          x(),
          "and",
          y(),
          "for",
          nrow(soccer_selected()),
          "players in selected year(s).")
  })
  
  #--------------------------------------------------------------------------------#
  # Print data table if checked
  output$moviestable <- DT::renderDataTable(
    DT::datatable(data = soccer_selected()[, 1:6],
                  options = list(pageLength = 100),
                  rownames = FALSE)
  )
  
  #--------------------------------------------------------------------------------#
  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target = "Data", select = TRUE)
    } else {
      hideTab(inputId = "tabsetpanel", target = "Data")
    }
  })
  
  #--------------------------------------------------------------------------------#
  # Render data table for codebook
  output$codebook <- renderDataTable({
    datatable(data = movies_codebook,
              options = list(pageLength = 10, lengthMenu = c(10, 25, 40)), 
              rownames = FALSE)
  })
  output$scatterplot2 <- renderPlot({
    ggplot(data = soccer_selected(), aes_string(x = input$x, y = input$y, color = input$z)) +
      geom_point(size = 1) +
      geom_smooth() +
      labs(x = x(),
           y = y(),
           color = toTitleCase(str_replace_all(input$z, "_", " ")),
           title = toTitleCase(input$plot_title))
  })
  
}
#--------------------------------------------------------------------------------#
# Create Shiny app object
#--------------------------------------------------------------------------------#
shinyApp(ui = ui, server = server)
