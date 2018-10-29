library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(tools)

dat <- read_csv("https://raw.githubusercontent.com/Nikkipeng/project-1/master/approval_polllist.csv")

data <- dat[2:15]
data <- subset(data, select = -10 )
data$startdate <- as.Date(data$startdate, format = "%m/%d/%Y")
data$enddate <- as.Date(data$enddate, format = "%m/%d/%Y")
pollsters <- unique(data$pollster)
pollsters <- c("All", pollsters)
data$time <- as.numeric(data$startdate - as.Date("2017-01-20"))

loessmodel <- function(df){
  model_approve <- loess(approve~time, data = df, span = 0.01, degree = 1)
  pre_approve <- predict(model_approve, se = T)
  model_disapprove <- loess(disapprove~time, data = df, span = 0.01, degree = 1)
  pre_disapprove <- predict(model_disapprove, se = T)
  df$appfit <- pre_approve$fit
  df$appmin <- pre_approve$fit-10*pre_approve$se.fit
  df$appmax <- pre_approve$fit+10*pre_approve$se.fit
  df$disappfit <- pre_disapprove$fit
  df$dismin <- pre_disapprove$fit - 10*pre_disapprove$se.fit
  df$dismax <- pre_disapprove$fit + 10*pre_disapprove$se.fit
  return(df)
}
data <- loessmodel(data)


ui <- fluidPage(
  #themeSelector(),
  theme = shinytheme("cerulean"),
  # App title
  titlePanel("How popular is Donald Trump?", windowTitle = "Trump Approval"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      width = 3,
      wellPanel(
        
        h4("Filter"),
        
        # date range input
        dateRangeInput(inputId = "date",
                       label = "Date range", 
                       min = as.Date("2017-01-20"), max = as.Date("2018-10-21"),
                       start = as.Date("2017-05-20"), end = as.Date("2018-07-20"),
                       separator = " - "),
        
        checkboxGroupInput(inputId = "subgroup", 
                           label = "Subgroup",
                           choices = c("All" = "All polls",
                                       "Adults",
                                       "Likely or registerd" = "Voters"), 
                           selected = "All polls",
                           inline = T),
        
        selectInput(inputId = "pollster",
                    label = "Select pollster:",
                    choices = pollsters,
                    multiple = T,
                    selected = "All")
      ),      
      
      wellPanel(
        
        checkboxGroupInput(inputId = "variables", 
                           label = "Variables",
                           choices = c("Subgroup" = "subgroup",
                                       "Start dates" = "startdate",
                                       "End dates" = "enddate",
                                       "Poster" = "pollster",
                                       "Grade" = "grade",
                                       "Sample Size" = "samplesize",
                                       "Population" = "population",
                                       "Weight" = "weight",
                                       "Approve" = "approve",
                                       "Disapprove" = "disapprove",
                                       "Adjusted Approve" = "adjusted_approve",
                                       "Adjusted Disapprove" = "adjusted_disapprove"), 
                           selected = c("subgroup", 
                                        "startdate",
                                        "pollster",
                                        "samplesize",
                                        "weight",
                                        "adjusted_approve",
                                        "adjusted_disapprove"),
                           inline = F)
      ),      
      wellPanel(
        textInput(inputId = "plot_title", 
                  label = "Ttitle", 
                  placeholder = "Enter text to be used as plot title"),
        
        checkboxInput(inputId = "show_data",
                      label = "Show plot and table",
                      value = F)
        
      ),
      
      wellPanel(
        h4("Download the full data"),
        radioButtons(inputId = "filetype1",
                     label = "Select filetype:",
                     choices =  c("pdf", "png"),
                     selected = "png",
                     inline = T),
        downloadButton(outputId = "download_plot1", label = "Download full plot"),
        br(), br(),
        radioButtons(inputId = "filetype2",
                     label = "Select filetype:",
                     choices = c("csv", "tsv"),
                     selected = "csv",
                     inline = T),
        downloadButton(outputId = "download_table1", label = "Download full data"),
        
        # Built with Shiny by RStudio
        br(),br(),
        h5("Built by Xiaopeng Peng from",
           img(src = "https://www.gwu.edu/sites/g/files/zaxdzs2226/f/image/gw_monogram_2c_process.gif", height = "30px"))
      )
    ),
    
    # Output:
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel("Full result", icon=icon("home"),
                           h4("Predicted by", a("local polynomial regression",     href="http://www.dfcm.utoronto.ca/Assets/DFCM2+Digital+Assets/Family+and+Community+Medicine/DFCM+Digital+Assets/Faculty+$!26+Staff/DFCM+Faculty+Work+$!26+Leadership+Survey+Poster.pdf"), "."),
                           plotOutput(outputId = "plot1"),
                           br(),br(),
                           DT::dataTableOutput(outputId = "table1", width = '1200px')),
                  
                  tabPanel(title = "Selected table", 
                           br(),
                           DT::dataTableOutput(outputId = "table2"),
                           radioButtons(inputId = "filetype3",
                                        label = "Download this table:",
                                        choices = c("csv", "tsv"),
                                        selected = "csv",
                                        inline = T),
                           downloadButton(outputId = "download_table2", label = "Download table")),
                  
                  tabPanel(title = "Selected plot", 
                           plotOutput(outputId = "plot2"),
                           h5(textOutput("description")),
                           radioButtons(inputId = "filetype4",
                                        label = "Download this plot:",
                                        choices = c("pdf", "png"),
                                        selected = "png",
                                        inline = T),
                           downloadButton(outputId = "download_plot2", label = "Download plot"),
                           br())
                  
      )
    )
  )
)