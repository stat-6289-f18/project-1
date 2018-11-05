library(shiny)
library(tidyverse)
library(readr)
library(ggplot2)
library(stringr)
library(dplyr)
library(DT)
library(tools)
library(leaflet)
library(tidyr)
library(shinythemes)
mydata <- read.csv("C:/Users/hou/Documents/project-1/bad-drivers.csv",header=T,sep=",")
mydata <- read.csv("C:/Users/hou/Documents/project-1/bad-drivers.csv",header=T,sep=",")

mydata$Latitude <- stringr::str_replace_all(mydata$Latitude, "[(]", "")
mydata$Longitude <- stringr::str_replace_all(mydata$Longitude, "[)]", "")


mydata$Latitude <- as.numeric(mydata$Latitude)
mydata$Longitude <- as.numeric(mydata$Longitude)
saveRDS(mydata, "C:/Users/hou/Documents/project-1/data.rds")
mydata<- readRDS("C:/Users/hou/Documents/project-1/data.rds")

ui<-shinyUI(navbarPage('What Cause The Fatal Collision In US', theme = shinytheme("united"),
                   tabPanel("Map", value = "mp",headerPanel("Collisons Map"),
                            h4("Click the location on the map"),
                            leafletOutput("mymap",height = 1000)
                            
                   ),
                   tabPanel("Histgram", value = "st",headerPanel("The number of car crashes in each state "),
                            
                            plotOutput("distPlot")
                            
                   ),
                   
                   
                   tabPanel("Linear Regression", value = "pl",headerPanel("Linear Regression Model"),
                            h4("Evaluate the relationship between 6 factors and number of fatal collisions "),
                            # h5("Insurance factors: 1.how much insurance companies pay out 2.how much insurance companies charge drivers
                            #    "),
                            # h5("Individual behavior factors: 3.Whether a driver was distracted,
                            #    4.Whether drivers were involved in previous crashes 5.Whether the driver was speeding at the time,
                            #    6.Whether a driver was alcohol-impaired"),
                            # 
                            sidebarLayout(
                              
                              sidebarPanel(
                                # radioButtons("plote", "Plot",
                                #              c("Scatter"="p", "Line"="l")
                                # ),
                                selectInput('ycol', 'Select a type of reason to visualize its relation to percentage of fatal collision', choices=names(mydata)[c(3,4,5,6,7,8)],
                                            selected=names(mydata)[[4]])
                                
                                
                                
                              ),
                              
                              mainPanel(
                                plotOutput("plot1"),
                                h4("Slope"),
                                textOutput("pred1slope"),
                                textOutput("pred2slope"),
                                textOutput("pred3slope"),
                                textOutput("pred4slope"),
                                textOutput("pred5slope"),
                                textOutput("pred6slope"),
                                h4("Intercept"),
                                textOutput("pred1intercept"),
                                textOutput("pred2intercept"),
                                textOutput("pred3intercept"),
                                textOutput("pred4intercept"),
                                textOutput("pred5intercept"),
                                textOutput("pred6intercept"),
                                h4("P Value of Regression Model"),
                                textOutput("pred1p"),
                                textOutput("pred2p"),
                                textOutput("pred3p"),
                                textOutput("pred4p"),
                                textOutput("pred5p"),
                                textOutput("pred6p")
                                
                                
                              )
                            )),
                   tabPanel("Correlation", value = "cor",headerPanel("Correlation"),
                            h4("Explore the relationship between each factor "),
                            
                            sidebarLayout(
                              sidebarPanel(
                                h3("Plotting"),      # Third level header: Plotting
                                
                                # Select variable for y-axis 
                                selectInput(inputId = "y", 
                                            label = "Y-axis:",
                                            choices = c("Speeding" = "Speeding", 
                                                        "Alcohol Impaired" = "Alcohol_Impaired", 
                                                        "Not Distracted " = "Not_Distracted", 
                                                        "Car Insurance Premiums" = "Car_Insurance_Premiums", 
                                                        "No Previous Accidents" ="No_Previous_Accidents",
                                                        "Losses incurred by insurance companies" = "Losses.incurred.by.insurance.companies"), 
                                            selected = "Speeding"),
                                
                                # Select variable for x-axis 
                                selectInput(inputId = "x", 
                                            label = "X-axis:",
                                            choices = c("Speeding" = "Speeding", 
                                                        "Alcohol Impaired" = "Alcohol_Impaired", 
                                                        "Not Distracted " = "Not_Distracted", 
                                                        "Car Insurance Premiums" = "Car_Insurance_Premiums", 
                                                        "No Previous Accidents" ="No_Previous_Accidents",
                                                        "Losses incurred by insurance companies" = "Losses.incurred.by.insurance.companies"), 
                                            selected = "Not_Distracted"),
                                
                                # Enter text for plot title
                                textInput(inputId = "plot_title", 
                                          label = "Plot title", 
                                          placeholder = "Enter text to be used as plot title"),
                                
                                hr(),
                                
                                
                                
                                
                                
                                # Show data table
                                checkboxInput(inputId = "show_data",
                                              label = "Show data table",
                                              value = TRUE)
                                
                                
                                
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
                                                     DT::dataTableOutput(outputId = "collisiontable"))
                                            
                                            
                                )
                              )
                            )
                   ),
                   navbarMenu("Others",
                              tabPanel("Data Table",value = "tb",headerPanel("DATA TABLE"),
                                       dataTableOutput("table")),
                              tabPanel("Summary",value = "sum",headerPanel("Summary"),
                                       h4("Variables Explanations:"),
                                       h5("1.Num_drivers:  Number of drivers in fatal car collisions"),
                                       h5("2.Speeding:  A driver was speeding"),
                                       h5("3.Alcohol_Impaired: A driver was alcohol-impaired"),
                                       
                                       h5("4.Not_Distracted:  Whether a driver was not distracted in collisions"),
                                       h5("5.No_Previous_Accidents: Drivers were not involved in previous crashes"),
                                       h5("6.Car_Insurance_Premiums:  How much insurance companies pay out"),
                                       
                                       verbatimTextOutput("summary") ),
                              tabPanel("About",value = "des",
                                       h3("About the collision data"),
                                       
                                       h5("This visualisation is based on data from U.S.Department of Transportation Federal Highway Administration." ),
                                       helpText(a("Click Here to get more information",href="https://www.fhwa.dot.gov/policyinformation/statistics/2011/vm2.cfm", target="_blank")),
                                       h3("About the car insurance data"),
                                       h5("This visualisation is based on data from National Association of Insurance Commissioners (NAIC)."),
                                       helpText(a("Click Here to get more information",href="https://www.naic.org/", target="_blank"))
                                       
                              )
                   ))              )
#######################################

server<-function(input, output, session) {
  
  model1 <- lm(Speeding ~ Num_drivers, data = mydata)
  model2 <- lm(Alcohol_Impaired~ Num_drivers, data = mydata)
  model3 <- lm(Not_Distracted~ Num_drivers, data = mydata)
  model4 <- lm(No_Previous_Accidents~ Num_drivers, data = mydata)
  model5 <- lm(Car_Insurance_Premiums~ Num_drivers, data = mydata)
  model6 <- lm(Losses.incurred.by.insurance.companies ~ Num_drivers, data = mydata)
  
  selectedData <- reactive({
    mydata[, c("Num_drivers", input$ycol)]
  })
  
  output$plot <- renderPlot({
    plot(selectedData(), type=input$plote)
  })  
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData())
    
    if(input$ycol=='Speeding'){abline(model1, col = "blue", lwd = 2)}
    if(input$ycol=='Alcohol_Impaired'){abline(model2, col = "green", lwd = 2)}
    if(input$ycol=='Not_Distracted'){abline(model3, col = "red", lwd = 2)}
    if(input$ycol=='No_Previous_Accidents'){abline(model4, col = "yellow", lwd = 2)}
    if(input$ycol=='Car_Insurance_Premiums'){abline(model5, col = "pink", lwd = 2)}
    if(input$ycol=='Losses.incurred.by.insurance.companies'){abline(model6, col = "orange", lwd = 2)}
    
    
  })
  
  
  output$pred1p <- renderText({if(input$ycol=='Speeding'){anova(model1)$'Pr(>F)'[1]}})
  output$pred2p <- renderText({if(input$ycol=='Alcohol_Impaired'){anova(model2)$'Pr(>F)'[1]}})
  output$pred3p <- renderText({if(input$ycol=='Not_Distracted'){anova(model3)$'Pr(>F)'[1]}})
  output$pred4p <- renderText({if(input$ycol=='No_Previous_Accidents'){anova(model4)$'Pr(>F)'[1]}})
  output$pred5p <- renderText({if(input$ycol=='Car_Insurance_Premiums'){anova(model5)$'Pr(>F)'[1]}})
  output$pred6p <- renderText({if(input$ycol=='Losses.incurred.by.insurance.companies'){anova(model6)$'Pr(>F)'[1]}})
  
  
  output$pred1slope <- renderText({if(input$ycol=='Speeding'){model1[[1]][2]}})
  output$pred2slope <- renderText({if(input$ycol=='Alcohol_Impaired'){model2[[1]][2]}})
  output$pred3slope <- renderText({if(input$ycol=='Not_Distracted'){model3[[1]][2]}})
  output$pred4slope <- renderText({if(input$ycol=='No_Previous_Accidents'){model4[[1]][2]}})
  output$pred5slope <- renderText({if(input$ycol=='Car_Insurance_Premiums'){model5[[1]][2]}})
  output$pred6slope <- renderText({if(input$ycol=='Losses.incurred.by.insurance.companies'){model6[[1]][2]}})
  
  
  output$pred1intercept <- renderText({if(input$ycol=='Speeding'){model1[[1]][1]}})
  output$pred2intercept <- renderText({if(input$ycol=='Alcohol_Impaired'){model2[[1]][1]}})
  output$pred3intercept <- renderText({if(input$ycol=='Not_Distracted'){model3[[1]][1]}})
  output$pred4intercept <- renderText({if(input$ycol=='No_Previous_Accidents'){model4[[1]][1]}})
  output$pred5intercept <- renderText({if(input$ycol=='Car_Insurance_Premiums'){model5[[1]][1]}})
  output$pred6intercept <- renderText({if(input$ycol=='Losses.incurred.by.insurance.companies'){model6[[1]][1]}})
  
  
  
  # x and y as reactive expressions
  x <- reactive({ toTitleCase(str_replace_all(input$x, "_", " ")) })
  y <- reactive({ toTitleCase(str_replace_all(input$y, "_", " ")) })
  
  # Create scatterplot object the plotOutput function is expecting 
  output$scatterplot <- renderPlot({
    ggplot(data = mydata, aes_string(x = input$x, y = input$y)) +
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
          nrow(mydata),
          "states.")
  })
  
  # Print data table if checked
  output$collisiontable <- DT::renderDataTable(
    DT::datatable(data = mydata[,1:8], 
                  options = list(pageLength = 10), 
                  rownames = FALSE)
    
  )
  
  # Display data table tab only if show_data is checked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target ="Data", select = TRUE)
    } else {
      hideTab(inputId = "tabsetpanel", target = "Data")
    }
  })
  
  
  output$table <- renderDataTable({
    (mydata)
  })
  
  output$summary <- renderPrint({
    summary(mydata)
  })
  #######MAP 
  data <- reactive({
    x <- mydata
  })
  
  output$mymap <- renderLeaflet({
    data <- data()
    
    m <- leaflet(data = mydata) %>%
      addTiles() %>%
      addMarkers(lng = ~Longitude,
                 lat = ~Latitude,
                 popup = paste("Number of drivers in fatal collisions", mydata$Num_drivers, "<br>",
                               "State:", mydata$State))
    
    m
  })
  
  output$distPlot <- renderPlot({
    p<-ggplot(data=mydata, aes(x=mydata$Abbr, y=mydata$Num_drivers)) +
      geom_bar(stat="identity") 
    
    p + xlab("State") + ylab("Number of drivers in fatal collison")  
    
    
    
    
    
  }) 
  
  
}  
shinyApp(ui = ui, server = server)




