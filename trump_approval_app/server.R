library(tidyverse)
library(shiny)
library(shinythemes)
library(DT)
library(tools)

function(input, output, session) {
  
  summary_plot <- reactive({
    ggplot(data = data, aes(x = startdate)) +
      geom_point(aes(y = approve), color = 'forestgreen', alpha = 0.2, shape = 16) +
      geom_line(aes(y = appfit, color = 'forestgreen'), size = 0.8) +
      geom_point(aes(y = disapprove), color = 'darkorange1', alpha = 0.2, shape = 16) +
      geom_line(aes(y = disappfit, color = 'darkorange1'), size = 0.8) +
      geom_ribbon(aes(ymin=appmin,ymax=appmax), fill = 'forestgreen', alpha=0.2) +
      geom_ribbon(aes(ymin=dismin, ymax=dismax), fill = 'darkorange1', alpha=0.2)+
      geom_hline(yintercept = 50) +
      labs(x = "Poll date", y = "", title = toTitleCase(input$plot_title)) +
      scale_x_date(date_labels = "%b %Y") +
      scale_colour_manual(name = "",
                          values = c("darkorange1" = "darkorange1", "forestgreen" = "forestgreen"),
                          breaks = c("darkorange1", "forestgreen"),
                          labels = c("Disapprove", "Approve"))
  })
  
  output$plot1 <- renderPlot({
    summary_plot()
  })
  
  full_table <- reactive({
    data %>% 
      mutate("DATES" = paste(as.character(format(startdate, "%b.%d")), "-", as.character(format(enddate, "%d")))) %>%
      mutate("SAMPLE" = paste(samplesize, " ", toupper(population))) %>%
      mutate("APPROVE" = paste(round(approve), "%")) %>%
      mutate("DIS-APPROVE" = paste(round(disapprove), "%")) %>%
      mutate("ADJUSTED approve" = paste(round(adjusted_approve), "%")) %>%
      mutate("ADJUSTED disapprove" = paste(round(adjusted_disapprove), "%")) %>%
      mutate("WEIGHT" = format(round(weight, 3), nsmall = 3)) %>%
      rename("POLLSTER"=pollster) %>%
      rename("GRADE"=grade) %>%
      select("DATES", "POLLSTER", "GRADE", "SAMPLE", "WEIGHT", "APPROVE", "DIS-APPROVE", "ADJUSTED approve", "ADJUSTED disapprove")
  })
  
  output$table1 <- DT::renderDataTable(
    DT::datatable(data = full_table(), 
                  rownames = FALSE, options = list(pageLength = 15, autoWidth = TRUE,
                                                   columnDefs=list(list(targets = c(0), width = '200'),
                                                                   list(targets = c(1), width = '400'),
                                                                   list(targets = c(2), width = '50'),
                                                                   list(targets = c(3), width = '100'),
                                                                   list(targets = c(4), width = '90'),
                                                                   list(targets = c(5), width = '90'),
                                                                   list(targets = c(6), width = '90'),
                                                                   list(targets = c(7), width = '90'),
                                                                   list(targets = c(8), width = '90'))
                  )) %>% 
      formatStyle(c("DIS-APPROVE", "ADJUSTED disapprove"),backgroundColor="#ffae4c") %>%
      formatStyle(c("APPROVE", "ADJUSTED approve"),backgroundColor="#84c184")
  )
  
  data_selected <- reactive({
    req(input$pollster)
    if(input$pollster == "All") {
      filter(data,
             startdate >= input$date[1] & startdate <= input$date[2],
             subgroup %in% input$subgroup)
    } else {
      filter(data,
             startdate >= input$date[1] & startdate <= input$date[2],
             pollster %in% input$pollster,
             subgroup %in% input$subgroup)
    }
  })
  
  output$table2 <- DT::renderDataTable(
    DT::datatable(data = data_selected() %>% select(input$variables),
                  options = list(pageLength = 15), 
                  rownames = FALSE)
  )
  
  selected_plot <- reactive({
    ggplot(data = loessmodel(data_selected()), aes(x = startdate)) +
      geom_point(aes(y = approve), color = 'forestgreen', alpha = 0.2, shape = 16) +
      geom_line(aes(y = appfit, color = 'forestgreen'), size = 0.8) +
      geom_point(aes(y = disapprove), color = 'darkorange1', alpha = 0.2, shape = 16) +
      geom_line(aes(y = disappfit, color = 'darkorange1'), size = 0.8) +
      geom_ribbon(aes(ymin=appmin,ymax=appmax), fill = 'forestgreen', alpha=0.2) +
      geom_ribbon(aes(ymin=dismin, ymax=dismax), fill = 'darkorange1', alpha=0.2) +
      geom_hline(yintercept = 50) +
      labs(x = "Poll date", y = "", title = toTitleCase(input$plot_title)) +
      scale_x_date(date_labels = "%b %Y") +
      scale_colour_manual(name = "",
                          values = c("darkorange1" = "darkorange1", "forestgreen" = "forestgreen"),
                          breaks = c("darkorange1", "forestgreen"),
                          labels = c("Disapprove", "Approve"))
  })  
  
  output$plot2 <- renderPlot({
    selected_plot()
  })
  
  output$download_plot1 <- downloadHandler(
    filename = function() {
      paste('full_plot.', input$filetype1, sep='')
    },
    content = function(file) { 
      if(input$filetype1 == "pdf"){ 
        ggsave(file, plot = summary_plot(), device = "pdf", width = 15, height = 8)
      }
      if(input$filetype1 == "png"){ 
        ggsave(file, plot = summary_plot(), device = "png", width = 15, height = 8) 
      }
    }
  )
  
  output$download_table1 <- downloadHandler(
    filename = function() {
      paste0("full_table.", input$filetype2)
    },
    content = function(file) { 
      if(input$filetype2 == "csv"){ 
        write_csv(data, path = file) 
      }
      if(input$filetype2 == "tsv"){ 
        write_tsv(data, path = file) 
      }
    }
  )
  
  output$download_plot2 <- downloadHandler(
    filename = function() {
      paste0("selected_plot.", input$filetype4)
    },
    content = function(file) { 
      if(input$filetype4 == "pdf"){ 
        ggsave(file, plot = selected_plot(), device = "pdf", width = 15, height = 8)
      }
      if(input$filetype4 == "png"){ 
        ggsave(file, plot = selected_plot(), device = "png", width = 15, height = 8) 
      }
    }
  )
  
  output$download_table2 <- downloadHandler(
    filename = function() {
      paste0("select_table.", input$filetype3)
    },
    content = function(file) { 
      if(input$filetype3 == "csv"){ 
        write_csv(data_selected(), path = file) 
      }
      if(input$filetype3 == "tsv"){ 
        write_tsv(data_selected(), path = file) 
      }
    }
  )
  
  voters <- reactive({
    if(input$subgroup == "All polls"){
      "all"
    }else if(input$subgroup == "Voters"){
      "likely or registerd"
    }
    else{
      "adult"
    }
  })
  
  pollster_number <- reactive({
    req(input$pollster)
    if(input$pollster == "All"){
      "all"
    } else {
      length(input$pollster)
    }
  })
  
  # Create description of plot
  output$description <- renderText({
    paste("The plot above shows the Trump approval in the time period between ",
          input$date[1],
          " to ",
          input$date[2],
          " of ",
          voters(),
          " voters ",
          " by ",
          pollster_number(),
          " pollsters",
          ", total ",
          nrow(data_selected()),
          " polls.")
  })
  
  # Display data table tab only if show_data is clicked
  observeEvent(input$show_data, {
    if(input$show_data){
      showTab(inputId = "tabsetpanel", target = "Selected table")
      showTab(inputId = "tabsetpanel", target = "Selected plot")
    } else {
      hideTab(inputId = "tabsetpanel", target = "Selected table")
      hideTab(inputId = "tabsetpanel", target = "Selected plot")
    }
  })
}