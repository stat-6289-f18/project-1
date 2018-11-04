##XuanyiLi G2784430 xuanyi_li26@gwu.edu
library(shiny)
setwd('~/Downloads/data-master/trump-twitter')
ui <- fluidPage(
  
  # App title ----
  titlePanel("Tabsets"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("dataset", "Choose whether to display id:", 
                  choices = c("Yes", "No")),
      
      # Input: Select the random distribution type ----
#       radioButtons("dist", "Distribution type:",
#                    c("Normal" = "norm",
#                      "Uniform" = "unif",
#                      "Log-normal" = "lnorm",
#                      "Exponential" = "exp")),
      radioButtons("dist", "want to see",
                   c("Full content" = "a",
                     "only created_at" = "b",
                     "only text" = "c")),
      
      # br() element to introduce extra vertical spacing ----
      br(),
      
      # Input: Slider for the number of observations to generate ----
      sliderInput("n",
                  "Number of observations:",
                  value = 50,
                  min = 1,
                  max = 100)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      h3("result"),
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", plotOutput("plot")),
                  tabPanel("Summary", verbatimTextOutput("summary")),
                  tabPanel("Table", tableOutput("table"))
      )
      
    )
  )
)

server <- function(input, output) {
  
  # Reactive expression to generate the requested distribution ----
  # This is called whenever the inputs change. The output functions
  # defined below then use the value computed from this expression
  
# data = read.table('realDonaldTrump_poll_tweets.csv',row.names=NULL,sep=",", header=TRUE)
  data = read.csv('realDonaldTrump_poll_tweets.csv',row.names=NULL,sep=",", header=TRUE)
  d <- reactive({
    dist <- switch(input$dist,
                   a = 1,
                   b = 2,
                   c = 3,
                   rnorm)
    xxx <- switch(input$dataset,
           "Yes" = 1,
           "No" = 2)
    if(a==1 && xxx==1){
      data[,1:3]
    }else if(a==1 && xxx==2){
      data[,c(1,2)]
    }else if(a==1 && xxx==3){
      data[,c(1,3)]
    }
    else if(a==2 && xxx==1){
      data[,2:3]
    }else if(a==2 && xxx==2){
      data[,2]
    }else if(a==2 && xxx==3){
      data[,3]
    }
     
  })
#   datasetInput <- reactive({
#     switch(input$dataset,
#            "Yes" = rock,
#            "No" = pressure)
#   })
  # Generate a plot of the data ----
  # Also uses the inputs to build the plot label. Note that the
  # dependencies on the inputs and the data reactive expression are
  # both tracked, and all expressions are called in the sequence
  # implied by the dependency graph.
  output$plot <- renderPlot({
#     dist <- input$dist
#     n <- input$n
#     
#     hist(d(),
#          main = paste("r", dist, "(", n, ")", sep = ""),
#          col = "#75AADB", border = "white")
    H <- c(40,32,12,52,41,21,67,11,20,55,16,20,1)
    M <- c("2015-8","2015-9","2015-10","2015-11","2015-12","2016-1","2016-2"
           ,"2016-3","2016-4","2016-5","2016-6","2016-7","2016-8")
    barplot(H,names.arg=M,xlab="Month",ylab="frequency",col="blue",
         main="Frequency chart",border="red")
   # plot(1:10)
  })
  
  # Generate a summary of the data ----
  output$summary <- renderPrint({
    # summary(d())
    print("Record number: 436")
    print("Statistical date: 2015.8-2016.8")
  })
  
  # Generate an HTML table view of the data ----
  output$table <- renderTable({
    #d()
    n <- input$n
    head(data,n)
  })
  
}
shinyApp(ui, server)