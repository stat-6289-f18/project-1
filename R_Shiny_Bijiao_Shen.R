library(RCurl)
#install.packages("magrittr")
library(magrittr)
library(dplyr)
fandango_score_comparison <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/fandango/fandango_score_comparison.csv")
score <- read.csv(text = fandango_score_comparison)
fandango_scrape <- getURL("https://raw.githubusercontent.com/fivethirtyeight/data/master/fandango/fandango_scrape.csv")
vote <- read.csv(text = fandango_scrape)
Rotten_Tomatoes <- score %>%
  select(FILM, RT_norm, RT_user_norm, RT_norm_round, RT_user_norm_round)
IMDB <- score %>%
  select(FILM, IMDB_norm, IMDB_norm_round, IMDB_user_vote_count)
Metacritic <- score %>%
  select(FILM, Metacritic_norm, Metacritic_user_nom, Metacritic_norm_round, Metacritic_user_norm_round, Metacritic_user_vote_count)
Fandango <- score %>%
  select(FILM, Fandango_Ratingvalue,Fandango_Stars,  Fandango_votes)
  


#score_vote = merge(score, vote, by="FILM", all.x=TRUE)
#summary(score)
```
## build shiny app

```{r}
library(shiny)
library(ggplot2)

# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Reactivity"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "caption",
                label = "Caption:",
                value = "Data Summary"),
      
      # Input: Selector for choosing dataset ----
      selectInput(inputId = "dataset",
                  label = "Choose a rating source:",
                  choices = c("Fandango", "Rotten Tomatoes", "Metacritic", "IMDB"),
                  selected = "Fandango"),
      
      # Input: Numeric entry for number of obs to view ----
      numericInput(inputId = "obs",
                   label = "Number of observations to view:",
                   value = 10),
      # Action button to download
      downloadButton("downloadData", "Download"),
      
      # Select filetype
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv"),
                   selected = "csv")
      
      
    ),
    
    
    # Main panel for displaying outputs ----
    mainPanel(tabsetPanel(
      tabPanel("Summary", tableOutput("summary")),
      tabPanel("Table",tableOutput("view")),
      tabPanel("Plot",plotOutput("plot"))
    )
    
    )
  )
)



# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {
  datasetInput <- reactive({switch(input$dataset,
                                   "Fandango" = Fandango,
                                   "Rotten Tomatoes" = Rotten_Tomatoes,
                                   "IMDB" = IMDB,
                                   "Metacritic" = Metacritic)
  })

  output$caption <- renderText({input$caption
  })
  
  output$summary <- renderTable({
    dataset <- datasetInput()
    data.frame(unclass(summary(dataset)), check.names = FALSE, stringsAsFactors = FALSE)
  })
  
  output$view <- renderTable({
    datasetInput()
  })
 
  output$plot <- renderPlot({
    plot(Fandango$Fandango_Stars[order(Fandango$Fandango_Stars)],type = "l", col="red",
         main = "normalized rating comparison", ylab = "rating", ylim = c(0,5))
    lines(datasetInput()[,3][order(datasetInput()[,3])],type = "l", col="green" )
  }) 
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}



shinyApp(ui, server)

