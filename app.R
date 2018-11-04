# Project-1 
# Shenglin Chen(linber_chen@gwu.edu)
# Loading packages
library(shiny)
library(shinythemes)
library(data.table)
library(stringr)
library(tidyverse)
library(DT)
library(tools)
library(ggvis)
# Reading data and manipulation
nba_elo <- fread("https://raw.githubusercontent.com/LinberChan/project-1/35b9bc20d8f1088d4cf0cbc02b9044b3e6e74e5c/nba_his.csv")
nba_elo$date <- as.Date(nba_elo$date,"%m/%d/%Y")
nba_elo$ID <- 1:nrow(nba_elo)

# Grouping into active or inactive
fran <- unique(nba_elo$fran)
active_team <- fran[c(2,5,11,14:16,18,23:25,28,30:33,38:43,45:53)]
inactive_team <- fran[!(fran %in% active_team)]

#Rating
ELO_Champ<- nba_elo %>% select(Franchise = fran,elo, champ) %>% group_by(Franchise) %>% summarise(AverageELO = round(mean(elo),2), TotalChamp = sum(champ)) %>% arrange(desc(AverageELO))

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   theme = shinytheme("readable"),
   
   # Application title
   titlePanel("The Complete History Of The NBA"),
   h5(textOutput("introduction")),
   
   # Side bar setting
   sidebarLayout(
     
     sidebarPanel(
       
     wellPanel(
      # Select variable for y-axis 
      radioButtons(inputId = "y", 
                  label = "Y-axis:",
                  choices = c("ELO Score" = "elo", 
                              "Match Score" = "score"), 
                  selected = "elo")
     ),
     helpText("ELO is a score that shows the relative strengh of a team. Higher score means stronger team"),
     wellPanel(
       
      # Select which team to plot
       selectizeInput(inputId = "selected_team",
                        label = "Select team:",
                        choices = list(Active = active_team, Inactive = inactive_team),
                        selected = "Warriors"
                      )
       
     ),
     wellPanel(
       
     # Sidebar with a slider input for number of season
     sliderInput("Season",
                 "Season:",
                 min = 1947,
                 max = 2018,
                 value = c(1970, 2018),
                 width = 2000 )
     
    ),
    wellPanel(
      
    # Show data table
      checkboxInput(inputId = "show_his_summary",
                  label = "Show History Summary",
                  value = FALSE),
      checkboxInput(inputId = "show_elo_rating",
                    label = "Show Ratings",
                    value = FALSE)
      
    ),
    wellPanel(
      numericInput("num", label = "How many records per page", value = 5)
      )),
    
      # main panel setting
      mainPanel(
        
        # ggvis plot
        ggvisOutput("ELO_plot"),
        h5(textOutput("description")),
        
        # tab set
        tabsetPanel(type = "tabs",
                    id = "tabsetpanel",
                    tabPanel(title = "Raw Data", 
                             br(),
                             DT::dataTableOutput(outputId = "historydata")),
                    # tab panel for team summary
                    tabPanel("Team Summary", 
                             br(),
                             verbatimTextOutput("summary")),
                    # tab panel for whole history summary
                    tabPanel("Whole Summary", 
                             br(),
                             verbatimTextOutput("his_summary")),
                    # tab panel for ELO and Championship rating
                    tabPanel(title = "ELO and Championship rating", 
                             br(),
                             DT::dataTableOutput(outputId = "ELO_Champ"))
        )

      )
   
  )
)

# Define server 
server <- function(input, output) {
  
    # Create description of plot
    output$introduction <- renderText({
      paste("Every franchise’s relative strength after every game.")
    })
    
    # Create a subset of data filtering for selected team
    Team_elos <- reactive({
      req(input$selected_team) # ensure availablity of value before proceeding
      
      # temp variables for input values
      minseason <- input$Season[1]
      maxseason <- input$Season[2]
      fran_id <- input$selected_team

      # apply filters
      x <- nba_elo %>% 
        filter(season >= minseason, 
               season <= maxseason, 
               fran %in% fran_id
               )

    x <- as.data.frame(x)
    x
    })
    
    # Create a subset of data filtering for summary
    Team_summary <- reactive({
      Team_elos() %>% select(season, elo, score)
    }) 
    
    #A reactive expression with the ggvis plot
    vis <- reactive({
      
      # Lables for axes
      xvar_name <- "date"
      yvar_name <- input$y
      # since the inputs are strings, we need to do a little more work.
      yvar <- prop("y", as.symbol(input$y))
      
      # Function for generating tooltip text
      ELO_tooltip <- function(x) {
        if (is.null(x)) return(NULL)
        
        Team_elosscore <- isolate(Team_elos())
        Team_elosscore <- Team_elosscore[Team_elosscore$ID == x$ID,]
        
        paste0("<b>", "date:", "</b>", format(Team_elosscore[,"date"],justify = "centre"), "</b><br>",
               "<b>", yvar,":", "</b>", Team_elosscore[,yvar_name], "</b><br>",
               "<b>", "fran:","</b>", Team_elosscore[,"fran"]
        )
      }
      
      # draw the ggvis
      Team_elos() %>% ggvis(x = ~date, y = yvar ) %>% 
        layer_points( size := 10, fill = ~fran, key := ~ID) %>%
        add_tooltip(ELO_tooltip, "hover") %>%
        layer_lines(stroke := "blue") %>%
        add_axis("x", title = xvar_name) %>%
        add_axis("y", title = yvar_name) %>%
        set_options(width = 1250, height = 500)
    })
    
    # show ggvis plot
      vis %>% bind_shiny("ELO_plot")
      
    # Create description of plot
    output$description <- renderText({
      paste("The plot above shows the",
            toupper(input$y), 
            "history of",
            input$selected_team,
            "and the average", 
            toupper(input$y), 
            "for this teams is",
            round(mean(Team_elos()[,input$y]),2),
            ".", 
            "The total number of champions is",
            sum(Team_elos()[,"champ"])
            )
    })
    
    # Print data table
    output$historydata <- DT::renderDataTable(
      DT::datatable(data = Team_elos(), 
                    options = list(pageLength = input$num), 
                    rownames = FALSE)
    )
    
    # Render data table for Summary
    output$summary <- renderPrint({
      summary(Team_summary())
    })
    
    # Print whole summary if checked
    output$his_summary <- renderPrint({
      data <- nba_elo %>% select(season,elo,score)
      summary(data)
    })
    
    # Print ELO rating
    output$ELO_Champ <- DT::renderDataTable(
      DT::datatable(data = ELO_Champ, 
                    options = list(pageLength = input$num), 
                    rownames = FALSE)
    )
    
    # Display whole summary tab only if show_data is checked
    observeEvent(input$show_his_summary, {
      if(input$show_his_summary){
        showTab(inputId = "tabsetpanel", target = "Whole Summary", select = TRUE)
      } else {
        hideTab(inputId = "tabsetpanel", target = "Whole Summary")
      }
    })
    
    # Display rating tab only if show_data is checked
    observeEvent(input$show_elo_rating, {
      if(input$show_elo_rating){
        showTab(inputId = "tabsetpanel", target = "ELO and Championship rating", select = TRUE)
      } else {
        hideTab(inputId = "tabsetpanel", target = "ELO and Championship rating")
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

