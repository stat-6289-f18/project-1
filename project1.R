library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(tools)

## prepare the data
flight <- read_csv("flying-etiquette.csv")

## only keep the useful data
flying_etiquette <- flight %>%
  select(-c(6, 7, 8)) %>%
  mutate_if(is.character, as.factor) %>%
  filter(`How often do you travel by plane?` != "Never") %>%
  mutate_at(vars(matches("rude")), function(x) 
  {ifelse(!is.na(x), 
          (ifelse(x %in% 
                    c("No, not rude at all", "No, not at all rude"), 0, 1)),
          x)}) %>%
  mutate(violation = if_else(
    `Have you ever used personal electronics during take off or landing in violation of a flight attendant's direction?`
    == "Yes" | 
      `Have you ever smoked a cigarette in an airplane bathroom when it was against the rules?`
    == "Yes", "Yes", "No")) %>%
  select(-c(18, 19)) %>%
  mutate_at(23, as.factor)

## rename the data.frame and save the name list
##names(flying_etiquette)
rename1 <- c("ID", "Travel_frequency", "Recline_seat", "Tall", "Children_under_18", 
             "rude_Move_to_an_unsold_seat", "rude_Be_chatty_with_seatmate",
             "Acceptable_times_to_get_up", "Obligation_for_recline", 
             "rude_Recline_your_seat", "Eliminate_recline", 
             "rude_Ask_to_switch_seats_for_friends", "rude_Ask_to_switch_seats_for_family",
             "rude_Wake_someone_up_to_use_bathroom", "rude_Wake_someone_up_to_go_for_a_walk",
             "rude_Bring_a_baby_on_a_plane", "rude_Knowingly_bring_unruly_children",
             "Gender", "Age", "Household_Income", "Education", "Location", "Rule_violation")

dataused1 <- flying_etiquette
names(dataused1) <- rename1

## now use dataused1

## modify the order
dataused2 <- dataused1 %>%
  mutate(Tall = fct_collapse(Tall,
                             "under 5'3\"" = c("Under 5 ft.", "5'0\"", "5'1\"", "5'2\""),
                             "5'3\"-5'7\"" = c("5'3\"", "5'4\"", "5'5\"", "5'6\"","5'7\""),
                             "5'8\"-6'0\"" = c("5'8\"", "5'9\"", "5'10\"", "5'11\"", "6'0\""),
                             "over 6'1\"" = c("6'1\"", "6'2\"", "6'3\"", "6'4\"", "6'5\"",
                                              "6'6\" and above")))  %>%
  mutate(Obligation_for_recline = fct_recode(Obligation_for_recline,
                                             "Yes" = "Yes, they should not recline their chair if the person behind them asks them not to",
                                             "No" = "No, the person on the flight has no obligation to the person behind them"))

v2 <- rename1 %>% str_replace("rude", "") %>% str_replace_all("_", " ")
v3 <- names(flight %>%
  select(-c(6, 7, 8)))[-c(18, 19)]

v4 <- paste(names(flight %>% select(-c(6, 7, 8)))[18], "or", 
        names(flight %>% select(-c(6, 7, 8)))[19])

Survey_codebook <- data.frame("Column" = c(1:23), "Variable name" = v2, "Description" = c(v3, v4))


# Define UI for application that plots features of movies
ui <- fluidPage(
  
  theme = shinytheme("journal"),
  
  # App title
  titlePanel("Flying Etiquette Survey", windowTitle = "Flying Etiquette"),
  
  # Sidebar layout with a input and output definitions
  sidebarLayout(
    
    # Inputs
    sidebarPanel(
      
      width = 3,
      
      wellPanel(
        h3("Subsetting Plot"),
        
        selectInput(inputId = "Sub_Var", 
                    label = "Variable:",
                    choices = c("Travel frequency" = "Travel_frequency", 
                                "Recline seat or not" = "Recline_seat", 
                                "Tall", 
                                "Have children under 18" = "Children_under_18", 
                                "Acceptable times to get up" = "Acceptable_times_to_get_up", 
                                "Obligation for the people behand about recline" = "Obligation_for_recline", 
                                "Support eliminate recline" = "Eliminate_recline", 
                                "Gender",
                                "Age", 
                                "Household Income" = "Household_Income", 
                                "Education", 
                                "Location", 
                                "Violate the rule" = "Rule_violation"), 
                    selected = "Age"),
        textInput(inputId = "plot_title2", 
                  label = "Plot title", 
                  placeholder = "Enter text to be used as plot title")
      ),
      
      wellPanel(
      
        h3("View for Different Group"),
      
        # Select variable for rude behavior 
        selectInput(inputId = "Rude", 
                    label = "Rude behavior:",
                    choices = c("Move to an unsold seat" = "rude_Move_to_an_unsold_seat", 
                              "Be chatty with seatmate" = "rude_Be_chatty_with_seatmate", 
                              "Recline your seat" = "rude_Recline_your_seat", 
                              "Ask to switch seats for friends" = "rude_Ask_to_switch_seats_for_friends", 
                              "Ask to switch seats for family" = "rude_Ask_to_switch_seats_for_family",
                              "Wake someone up to use bathroom" = "rude_Wake_someone_up_to_use_bathroom", 
                              "Wake someone up to go for a walk" = "rude_Wake_someone_up_to_go_for_a_walk",
                              "Bring a baby on a plane" = "rude_Bring_a_baby_on_a_plane", 
                              "Knowingly bring unruly children" = "rude_Knowingly_bring_unruly_children"), 
                    selected = "rude_Move_to_an_unsold_seat"),
      
        # Select variable for variable 
        selectInput(inputId = "Var", 
                    label = "Variable:",
                    choices = c("Travel frequency" = "Travel_frequency", 
                              "Recline seat or not" = "Recline_seat", 
                              "Tall", 
                              "Have children under 18" = "Children_under_18", 
                              "Acceptable times to get up" = "Acceptable_times_to_get_up", 
                              "Obligation for the people behand about recline" = "Obligation_for_recline", 
                              "Support eliminate recline" = "Eliminate_recline", 
                              "Gender",
                              "Age", 
                              "Household Income" = "Household_Income", 
                              "Education", 
                              "Location", 
                              "Violate the rule" = "Rule_violation"), 
                    selected = "Age"),
      
        # Enter text for plot title
        textInput(inputId = "plot_title", 
                label = "Plot title", 
                placeholder = "Enter text to be used as plot title"),
      
        hr(),
        # Show group detail
        checkboxInput(inputId = "show_detail",
                    label = "Show data detail",
                    value = TRUE),
        checkboxInput(inputId = "show_detail_compare",
                    label = "Show table and compare",
                    value = TRUE)
      ),
      
      wellPanel(
        h3("Download"),
        radioButtons(inputId = "filetype",
                     label = "Download the raw data in:",
                     choices = c("csv", "tsv"),
                     selected = "csv", inline = TRUE),
        downloadButton(outputId = "download_data", label = "Download"),
        hr(),
        radioButtons(inputId = "filetype2",
                     label = "Download the full plot in:",
                     choices = c("pdf", "png"),
                     selected = "pdf", inline = TRUE),
        downloadButton(outputId = "download_plot", label = "Download")
      ),
      
      
      # 
      # h3("Subsetting"),    # Third level header: Subsetting
      # 
      # # Select which types of movies to plot
      # checkboxGroupInput(inputId = "selected_type",
      #                    label = "Select movie type(s):",
      #                    choices = c("Documentary", "Feature Film", "TV Movie"),
      #                    selected = "Feature Film"),
      # 
      hr(),
      
      br(),
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
      
    ),
    
    # Output:
    mainPanel(
      
      width = 9,
      
      tabsetPanel(type = "tabs",
                  id = "tabsetpanel",
                  tabPanel(title = "Full plot", icon=icon("home"),
                           
                           plotOutput(outputId = "fullplot")
                           
                  ),
                  tabPanel(title = "Subsetting",
                           
                           uiOutput("Subsetting_var"),
                           
                           uiOutput("Subsetting_plot")
                           
                  ),
                  tabPanel(title = "Behavior vs Group", 
                           plotOutput(outputId = "groupplot"),
                           br(),
                           h5(textOutput("description")),
                           br(),
                           navlistPanel(id = "group_detail",
                             tabPanel(title = "Summary",
                                      br(),
                                      tableOutput(outputId = "sample_tt_info"),
                                      h5(textOutput("description2"))),
                             tabPanel(title = "Compare",
                                      br(),
                                      tableOutput(outputId = "sample_tt_info2"),
                                      verbatimTextOutput(outputId = "testoutput"))                             )
                           ),
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
  

  output$Subsetting_var <- renderUI({
    sub_var <- dataused2 %>%
      pull(input$Sub_Var) %>%
      levels
    fluidPage(
      
      sidebarLayout(
        
        # Inputs
        sidebarPanel(
          h4("Subgroup"),
          checkboxGroupInput("Sub_Var2", "Choose group", sub_var)
        ),
        
        # Output:
        mainPanel(
          renderPlot({
            req(input$Sub_Var2)
            dataused2 %>%
              filter(eval(as.name(input$Sub_Var)) %in% input$Sub_Var2) %>%
              select(contains("rude")) %>%
              gather(response_var, rude) %>%
              mutate(response_var = str_replace(response_var, '^rude_', '')) %>%
              mutate(response_var = str_replace_all(response_var, '_', ' ')) %>%
              filter(!is.na(rude)) %>%
              group_by(response_var) %>%
              summarise(perc_rude = mean(rude))%>%
              ggplot(aes(x = fct_reorder(response_var, perc_rude), y = perc_rude)) +
              geom_col(width = 0.8, fill = "darkorange", alpha = 0.8) +
              labs(title = toTitleCase(input$plot_title2),
                   subtitle = "Percentage of selected respondents who said action is very or somewhat rude",
                   caption = "Source: SurveyMonkey Audience",
                   # Remove the x- and y-axis labels
                   x = NULL,
                   y = NULL)  +
              coord_flip() +
              # Remove the x-axis ticks and labels
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank()) +
              geom_text(aes(label = scales::percent(perc_rude),
                            y = perc_rude + .05),
                        position = position_dodge(0.9),
                        vjust = 1)
          })
        )
      )
    )
  })
  ########test
  output$Subsetting_plot <- renderUI({
    req(input$Sub_Var2)
    sub_var <- dataused2 %>%
      pull(input$Sub_Var) %>%
      levels
    sub_plot <- reactive({
      req(input$Sub_Var2)
      dataused2 %>%
        filter(eval(as.name(input$Sub_Var)) %in% input$Sub_Var2) %>%
        select(contains("rude")) %>%
        gather(response_var, rude) %>%
        mutate(response_var = str_replace(response_var, '^rude_', '')) %>%
        mutate(response_var = str_replace_all(response_var, '_', ' ')) %>%
        filter(!is.na(rude)) %>%
        group_by(response_var) %>%
        summarise(perc_rude = mean(rude))%>%
        ggplot(aes(x = fct_reorder(response_var, perc_rude), y = perc_rude)) +
        geom_col(width = 0.8, fill = "darkorange", alpha = 0.8) +
        labs(title = toTitleCase(input$plot_title2),
             subtitle = "Percentage of selected respondents who said action is very or somewhat rude",
             caption = "Source: SurveyMonkey Audience",
             x = NULL, y = NULL)  +
        coord_flip() +
        # Remove the x-axis ticks and labels
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        geom_text(aes(label = scales::percent(perc_rude),
                      y = perc_rude + .05),
                  position = position_dodge(0.9), vjust = 1)
    })
    
    output$download_plot2 <- downloadHandler(
      filename = function() {
        paste0("sub_plot.", input$filetype3)
      },
      content = function(file) {
        ggsave(file, plot = sub_plot(), device = input$filetype3, width = 15, height = 8)
      }
    )
    
    fluidPage(
      radioButtons(inputId = "filetype3",
                   label = "Download the plot in:",
                   choices = c("pdf", "png"),
                   selected = "pdf", inline = TRUE),
      downloadButton(outputId = "download_plot2", label = "Download")
    )

  })
  
  

  ## create var vs. rude part
  data_1 <- reactive({ 
    dataused2 %>%
      filter(!is.na(eval(as.name(input$Var)))) %>%  ## maybe can use pull?
      filter(!is.na(eval(as.name(input$Rude)))) %>%
      group_by(eval(as.name(input$Var))) %>%
      summarise(rude = mean(eval(as.name(input$Rude)))) %>%
      rename(Group = `eval(as.name(input$Var))`, Percentage = rude) %>%
      mutate(Group = fct_reorder(Group, Percentage))
    })
  
  data_2 <- reactive({ 
    dataused2 %>%
      filter(!is.na(eval(as.name(input$Var)))) %>%
      filter(!is.na(eval(as.name(input$Rude)))) %>%
      group_by(eval(as.name(input$Var))) %>%
      summarise(rude = sum(eval(as.name(input$Rude))), no_rude = n() - rude) %>%
      rename(Group = `eval(as.name(input$Var))`, Rude = rude, "Not rude" = no_rude)
  })
  
  output$sample_tt_info2 <- renderTable({
    data_2()
  })

  output$sample_tt_info <- renderTable({
    data_1()
  })
  
  output$testoutput <- renderPrint({
    data <- data_2()[,2:3]
    print(chisq.test(data))
  })
  ## chisq.test(data)$p.value can use to give some conclusion, use or not?
  
  # Create plot object the plotOutput function is expecting 
  output$groupplot <- renderPlot({
      ggplot(data = data_1(), aes(x = Group, y = Percentage)) +
      geom_col(width = 0.5, fill = "darkorange", alpha = 0.8) +
      coord_flip() +
      labs(title = toTitleCase(input$plot_title),
           subtitle = "Percentage of air-passenger respondents who said action is very or somewhat rude",
           caption = "Source: SurveyMonkey Audience", 
           # Remove the x- and y-axis labels
           x = NULL, 
           y = NULL)  +
      # Remove the x-axis ticks and labels
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      geom_text(aes(label = scales::percent(round(Percentage,3), accuracy = .01),
                    y = Percentage + ifelse(max(Percentage > 0.5), 0.03, 0.02)), 
                position = position_dodge(0.9),
                vjust = 1)
  })
  
 
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the relationship between",
          toTitleCase(str_replace_all(input$Var, "_", " ")),
          "and",
          toTitleCase(str_replace_all(str_replace(input$Rude, "rude", ""), "_", " ")))
  })
  output$description2 <- renderText({
    paste("There are", sum(colSums(data_2()[,2:3])), "passengers divided into", 
          nrow(data_1()),"groups")
  })
  
  # Display group detail tab only if show_detail is checked
  observeEvent(input$show_detail_compare, {
    if(input$show_detail_compare){
      showTab(inputId = "group_detail", target = "Compare", select = TRUE)
    } else {
      hideTab(inputId = "group_detail", target = "Compare")
    }
  })
  observeEvent(input$show_detail, {
    if(input$show_detail){
      showTab(inputId = "group_detail", target = "Summary", select = TRUE)
    } else {
      hideTab(inputId = "group_detail", target = "Summary")
    }
  })
  
  #output$fullplot
  fullplot <- reactive({
    dataused2 %>%
      select(contains("rude")) %>%
      gather(response_var, rude) %>%
      mutate(response_var = str_replace(response_var, '^rude_', '')) %>%
      mutate(response_var = str_replace_all(response_var, '_', ' ')) %>%
      filter(!is.na(rude)) %>%
      group_by(response_var) %>%
      summarise(perc_rude = mean(rude))%>%
      ggplot(aes(x = fct_reorder(response_var, perc_rude), y = perc_rude)) + 
      geom_col(width = 0.9, fill = "cornflowerblue", alpha = 0.8) +
      labs(title = "Hell Is Other People In A Pressurized Metal Tube",
           subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
           caption = "Source: SurveyMonkey Audience", 
           # Remove the x- and y-axis labels
           x = NULL, 
           y = NULL)  +
      coord_flip() + 
      # Remove the x-axis ticks and labels
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank()) +
      geom_text(aes(label = scales::percent(perc_rude), 
                    y = perc_rude + .05), 
                position = position_dodge(0.9),
                vjust = 1)
  })
  output$fullplot <- renderPlot({
    fullplot()
    # dataused2 %>%
    #   select(contains("rude")) %>%
    #   gather(response_var, rude) %>%
    #   mutate(response_var = str_replace(response_var, '^rude_', '')) %>%
    #   mutate(response_var = str_replace_all(response_var, '_', ' ')) %>%
    #   filter(!is.na(rude)) %>%
    #   group_by(response_var) %>%
    #   summarise(perc_rude = mean(rude))%>%
    #   ggplot(aes(x = fct_reorder(response_var, perc_rude), y = perc_rude)) + 
    #   geom_col(width = 0.9, fill = "cornflowerblue", alpha = 0.8) +
    #   labs(title = "Hell Is Other People In A Pressurized Metal Tube",
    #        subtitle = "Percentage of 874 air-passenger respondents who said action is very or somewhat rude",
    #        caption = "Source: SurveyMonkey Audience", 
    #        # Remove the x- and y-axis labels
    #        x = NULL, 
    #        y = NULL)  +
    #   coord_flip() + 
    #   # Remove the x-axis ticks and labels
    #   theme(axis.text.x = element_blank(),
    #         axis.ticks.x = element_blank()) +
    #   geom_text(aes(label = scales::percent(perc_rude), 
    #                 y = perc_rude + .05), 
    #             position = position_dodge(0.9),
    #             vjust = 1)
  })
  
  # Render data table for codebook
  output$codebook <- renderDataTable({
    datatable(data = Survey_codebook,
              options = list(pageLength = 10, lengthMenu = c(10, 20, 30)),
              rownames = FALSE)
  })
  
  
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("full_flying_survey.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(flight, path = file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(flight, path = file) 
      }
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("full_plot.", input$filetype2)
    },
    content = function(file) {
      ggsave(file, plot = fullplot(), device = input$filetype2, width = 15, height = 8)
    }
  )

  
}

# Create Shiny app object
shinyApp(ui = ui, server = server)