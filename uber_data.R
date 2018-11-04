library(shiny)
library(leaflet)
library(DT)
library(tidyverse)
library(shinythemes)
library(plyr)
library(ggplot2)

#############################################################################


jul_dat <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/uber-tlc-foil-response/master/uber-trip-data/uber-raw-data-jul14.csv", stringsAsFactors = FALSE)

jul_dat$Date.Time <- as.POSIXct(jul_dat$Date.Time, format = "%m/%d/%Y %H:%M:%S") # convert character to time

# feature extraction
print("Data preprocessing ...")

# Function to get rides each day of the week
jul_dat$Date <- as.Date(jul_dat$Date.Time)
jul_dat$hr <- format(jul_dat$Date.Time, "%H") # gives hour of ride in string
jul_dat$hr_map_r <- factor(floor(as.numeric(jul_dat$hr)/24*8))
jul_dat$hr_map <- mapvalues(jul_dat$hr_map_r,
                                     from = 0:7,
                                     to = c("0AM-3AM",
                                            "3AM-6AM",
                                            "6AM-9AM",
                                            "9AM-12AM",
                                            "12AM-15AM",
                                            "15AM-18AM",
                                            "18AM-21AM",
                                            "21AM-24AM"))
  
  
jul_dat$wd <- weekdays(jul_dat$Date) # gives weekday

tt <- table(jul_dat$Date)
tt_names <- names(tt) # already ordered
first_indices <- rep(0,length(tt_names)) # first indices contain the changepoints
ll=length(tt_names)



i=1
for (t in tt_names){
  first_indices[i] <- as.numeric(rownames(jul_dat[jul_dat$Date==t,][1,]))
  print(paste(i/ll*100, "percent of data preprocessing done ..."))
  i=i+1
}


tolerance <- 3000 # max number to plot and perform k means clustering on
min_cl_num <- 5 # minimum number of clusters
max_cl_num <- 20 # maximum number of clusters

ui <- fluidPage(
  theme = shinytheme("journal"),
  # App title
  titlePanel("Uber Analysis during July 2014 in NYC",windowTitle = "Uber in NYC"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      wellPanel(
        h3("Clustering analysis"),      
        
        
        sliderInput("slider", "Date Index:",
                    value = 1, min = 1, max = 31),
        
        sliderInput("kmc", "Number of Clusters:",
                    value = min_cl_num, min = min_cl_num, max = max_cl_num)
      ),
      
      wellPanel(
        h3("Data Summary"),# Third level header: Data Summary
        textInput("caption", "Caption:", "Subset Summary"),
        selectInput("selected_wd", "Choose a weekday:", 
                    choices = c("Monday", "Tuesday", "Wednesday","Thursday","Friday", "Saturday","Sunday"),
                    selected = "Monday"),
        numericInput("obs", "Number of observations to view:", 10)
      ),
      
      wellPanel(
        # Show data table
        checkboxInput(inputId = "show_data",
                      label = "Show data table",
                      value = TRUE),
        
        checkboxInput(inputId = "summary_data",
                      label = "Show Summary",
                      value = TRUE)
      )
    ),
    
    
    # Output:
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  # Tab 1: Map
                  tabPanel(title = "Map", 
                           h3(textOutput("description")),
                           leafletOutput("map"),
                           tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}")
                  ),
                  
                  # Tab 2: Plot
                  tabPanel(title = "Plot", 
                           h3(textOutput("plot1title")),
                           br(),
                           plotOutput("histhr")
                  ),
                  
                  # Tab 3: Subset Summary
                  tabPanel(title = "Subset Summary",
                           h3(textOutput("caption")), 
                           verbatimTextOutput("subsummary"),
                           tableOutput("view")
                  ),
                  # Tab 4: Summary
                  tabPanel("Summary", 
                           br(),
                           tags$strong("Summary of whole dataset"),
                           br(),
                           br(),
                           verbatimTextOutput("summary"),
                           h5(htmlOutput("info"))
                  ),
                  
                  # Tab 5: Data
                  tabPanel(title = "Data", 
                           br(),
                           DT::dataTableOutput(outputId = "table"))
      )
    )
  )
)


server <- function(input, output) {
  p=jul_dat
  
  
  # Create description of plot
  output$description <- renderText({
    paste("Date:", p[first_indices[input$slider],"Date"],p[first_indices[input$slider],"wd"] )
  })
  
  output$plot1title <- renderText({
    paste("Histogram of ride frequency for date:", p[first_indices[input$slider],"Date"])
  })
  
  output$info <- renderUI({
    str1 <- paste("Date/Time: The date and time of the Uber pickup")
    str2 <- paste("Lat:The latitude of the Uber pickup")      
    str3 <- paste("Lon:The longitude of the Uber pickup") 
    str4 <- paste("Base:The TLC base company code affiliated with the Uber pickup") 
    str5 <- paste("hr_map:Time zone of the Uber pickup") 
    str6 <- paste("wd:The weekday of the Uber pickup") 
    HTML(paste(str1, str2, str3, str4, str5, str6,sep = '<br/><br/>'))
  })
  
  output$caption <- renderText({
    input$caption
  })
  
  
  pal <- colorFactor( # define palette for points
    palette = "Dark2",
    domain = factor(p$hr_map))
  
  
  output$map = renderLeaflet({
    leaflet() %>%
      
      addProviderTiles(providers$CartoDB.Positron) %>% # layout
      
      addCircleMarkers(data = p[first_indices[input$slider]:(first_indices[input$slider]+
                                                               min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),], # adds start locations
                       lat = ~ Lat, lng = ~ Lon,
                       fillOpacity=1, 
                       radius=0.5,
                       color= ~pal(hr_map)) %>%
      
      addMarkers(data=data.frame(kmeans(p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                         min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),c("Lat","Lon")],
                                        input$kmc)$centers),
                 lat = ~Lat, lng = ~ Lon,
                 label= ~ paste("Latitude:", round(Lat,3), "Longitude", round(Lon,3))) %>%
      
      addLegend("bottomright", pal = pal, values = p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                                    min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),"hr_map"],
                title = paste("Date:", p[first_indices[input$slider],"Date"]),
                opacity = 1) %>%
      
      setView(lng = -73.98928, lat = 40.75042, zoom = 12) # sets zoom to be in NYC
  })
  
  
  #plot histogram
  output$histhr <- renderPlot({
    
    ggplot(data=data.frame(count(p[first_indices[input$slider]:(first_indices[input$slider]+
                                                                  min(first_indices[(input$slider+1)]-first_indices[(input$slider)], tolerance)),"hr_map"])), aes(x=x, y=freq,fill=I("black"),col=I("black"))) +  
      geom_bar(stat="identity", position="dodge", width=0.9, alpha=0.3)+
      theme(text = element_text(size = 11), 
            axis.line = element_line(colour = "black"), 
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.background = element_rect(fill = "grey92")) 
  })
  
  # Print subset summary table
  data_selected <- reactive({
    req(input$selected_wd) # ensure availablity of value before proceeding
    filter(p, p$wd %in% input$selected_wd)
  })
  subsum <- reactive({
    data_selected() %>% select(Date.Time,Lat,Lon,Base,hr_map)
  })
  output$subsummary <- renderPrint({
    summary(subsum())
  })
  
  output$view <- renderTable({
    head(subsum(), n = input$obs)
  })
  
  # Print summary table if checked
  output$summary <- renderPrint(
    if(input$summary_data){
      summary(p)
    })
  
  # Print data table if checked
  output$table <- DT::renderDataTable(
    if(input$show_data){
      DT::datatable(data = p[,c(-6,-7)], 
                    options = list(pageLength = 10), 
                    rownames = FALSE)
    }
  )
}



runApp(shinyApp(ui, server),launch.browser = TRUE) # Create app




