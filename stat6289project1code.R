data=read.csv("https://raw.githubusercontent.com/yukefu/stat6289-project1/master/sleeping-alone-data-new.csv")
summary(data)
library(tidyr)
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)

table1=round(prop.table(table(data$RelationshipStatus , data$HowOften)),4)
m1=matrix(nrow=36, ncol=5)
m1[,5]=rep("RelationshipStatus",36)
m1[,1]=rep(t(colnames(table1)),each=6)
m1[,3]=rep(rownames(table1),6)
m1[,4]=as.vector(table1*100)
question=c('Which of the following best describes your current relationship status?')
m1[,2]=rep(question,36)

table2=round(prop.table(table(data$HowLongRelationship , data$HowOften)),4)
m2=matrix(nrow=36, ncol=5)
m2[,5]=rep("HowLongRelationship",36)
m2[,1]=rep(t(colnames(table2)),each=6)
m2[,3]=rep(rownames(table2),6)
m2[,4]=as.vector(table2*100)
question=c('How long have you been in your current relationship? If you are not currently in a relationship, please answer according to your last relationship.')
m2[,2]=rep(question,36)


table15=round(prop.table(table(data$Gender , data$HowOften)),4)
m15=matrix(nrow=12, ncol=5)
m15[,5]=rep("Gender",12)
m15[,1]=rep(t(colnames(table15)),each=2)
m15[,3]=rep(rownames(table15),6)
m15[,4]=as.vector(table15*100)
question=c('Gender')
m15[,2]=rep(question,12)
m15=m15[which(m15[,3]!=""),]

table16=round(prop.table(table(data$Age , data$HowOften)),4)
m16=matrix(nrow=24, ncol=5)
m16[,5]=rep("Age",24)
m16[,1]=rep(t(colnames(table16)),each=4)
m16[,3]=rep(rownames(table16),6)
m16[,4]=as.vector(table16*100)
question=c('Age')
m16[,2]=rep(question,24)


data2=data[which(data$Income !=""),] 
#there are 195 respondents do not answer income, we have 810 response left.
table17=round(prop.table(table(data2$Income , data2$HowOften)),4)
m17=matrix(nrow=36, ncol=5)
m17[,5]=rep("Income",36)
m17[,1]=rep(t(colnames(table17)),each=6)
m17[,3]=rep(rownames(table17),6)
m17[,4]=as.vector(table17*100)
question=c('Income')
m17[,2]=rep(question,36)
m17=m17[which(m17[,3]!=""),]

data2=data[which(data$Education !=""),] 
table18=round(prop.table(table(data2$Education , data2$HowOften)),4)
m18=matrix(nrow=36, ncol=5)
m18[,5]=rep("Education",36)
m18[,1]=rep(t(colnames(table18)),each=6)
m18[,3]=rep(rownames(table18),6)
m18[,4]=as.vector(table18*100)
question=c('Education')
m18[,2]=rep(question,36)
m18=m18[which(m18[,3]!=""),]

data2=data[which(data$Location !=""),] 
table19=round(prop.table(table(data2$Location , data2$HowOften)),4)
m19=matrix(nrow=60, ncol=5)
m19[,5]=rep("Location",60)
m19[,1]=rep(t(colnames(table19)),each=10)
m19[,3]=rep(rownames(table19),6)
#m19[,4]=paste(as.vector(table19*100),"%")
m19[,4]=as.vector(table19*100)
question=c('Location')
m19[,2]=rep(question,60)
m19=m19[which(m19[,3]!=""),]

table20=round(prop.table(table(data$HowOften)),4)
m20=matrix(nrow=6, ncol=5)
m20[,5]=rep("All",6)
m20[,3]=rep("All",6)
m20[,1]=t(rownames(table20))
#m20[,4]=paste(as.vector(t(table20)*100),"%")
m20[,4]=as.vector(t(table20)*100)
question=c('All')
m20[,2]=rep(question,6)

m21=rbind(m1,m2,m15,m16,m17,m18,m19,m20)
colnames(m21)<-c("Howoften","Questions","Options","Proportion (%)","Variable Name")

VariableName=matrix(c("Which of the following best describes your current relationship status?",
                      "How long have you been in your current or last relationship?", "Gender" ,
                      "Age" ,"Income" , "Education" , "Location" ,  
                      "RelationshipStatus","HowLongRelationship",
                      "Gender","Age","Income","Education","Location"),ncol=2)





ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset==="BasicQuestion"',
      wellPanel(
      selectInput(inputId = "Howoften", 
                  label = "When both you and your partner are at home, how often do you sleep in separate beds?",
                  choices = c("All","A few times per month", "A few times per week", "Every night", "Once a month or less","Once a year or less"), 
                  selected = "All"))
      ),
     
      # Select variable 
      conditionalPanel(
      'input.dataset==="BasicQuestion"',
      wellPanel(
      selectInput(inputId = "BasicInformation", 
                  label = "Basic Questions:",
                  choices = c("All",
                              "Which of the following best describes your current relationship status?" = "RelationshipStatus", 
                              "How long have you been in your current or last relationship?" = "HowLongRelationship",
                              "Gender" = "Gender",
                              "Age" = "Age",
                              "Income" = "Income",
                              "Education" = "Education",
                              "Location" = "Location"), 
                  selected = "All"))
      ),
      
      conditionalPanel(
        'input.dataset==="BasicQuestion"',
      wellPanel(
      sliderInput(inputId ="Range", label ="Range:",
                  min = 0, max = 100,
                  value = 100))
      ),
      
      conditionalPanel(
        'input.dataset==="BasicQuestion"',
        wellPanel(
          actionButton("apply","Who did this amazing data visualization?"),
          hr(),
          textOutput(outputId = "amazing")
        )
      )
      
      
    ),
    
    mainPanel = (
      tabsetPanel(id= 'dataset' ,type="tabs",
        tabPanel("BasicQuestion",
                  mainPanel(h2("How Many Couples Sleep in Separate Beds?"),
                  h3("Frequency Table"),
                  #      tableOutput(outputId = "feqtable"),
                  hr(),
                  DT::dataTableOutput(outputId = "table21"),
                  hr(),
                  h3("Corresponding Bar Chart"),
                  hr(),
                  plotOutput(outputId = "graph"),
                  hr(),
                  htmlOutput(outputId = "sample_info")
                 )),
        tabPanel("OriginalNews",
                tags$iframe(style="height:800px; width:100%; scrolling=yes",
                            src="https://fivethirtyeight.com/features/dear-mona-how-many-couples-sleep-in-separate-beds/"))
      ) 
      ),
    position = "right"
    )
)

server <-function(input, output){
  selected_data=reactive(
    {data %>% select(c("HowOften",input$BasicInformation)) %>% filter(data[,input$BasicInformation]!= "")})
  
  n_sample=reactive(nrow(selected_data()))
  
  VarName=reactive(
    VariableName[which(VariableName[,2]==input$BasicInformation),1])
  
  output$sample_info <-renderUI({
    if (input$BasicInformation == "All"){
      paste("Note:",nrow(data), "respondents took participate in this survey and provided valid answers.")
    }
    else if (input$BasicInformation != "All" && input$Howoften == "All"){
      paste("Note:",n_sample(),
            "respondents give valid answers. The table below shows the frequencies of between 'How often do you sleep in separate beds' and '", VarName(),"'.")
    }
    else if (input$BasicInformation != "All" && input$Howoften != "All"){
      paste("Note:",n_sample(),
            "respondents give valid answers. The table below shows the frequencies of between 'How often do you sleep in separate beds' and '", 
            VarName(),"'.",
            nrow(selected_data()[which(selected_data()$HowOften==input$Howoften),]),
            "respondents choose '", input$Howoften, "' The table below shows the frequencies of between", input$Howoften, "and '", VarName(),"'.")
    }
  })
  
  
  output$table21 <- DT::renderDataTable({
    
    if (input$BasicInformation == "All" && input$Howoften == "All")
    {
      DT::datatable(data = m21[which(m21[,3]=="All" &
                                       as.numeric(m21[,4])< input$Range),1:4])
    }
    
    else{
      data1 <- m21
      if (input$BasicInformation != "All"){
        
        data1<-data1[which(data1[,5]==input$BasicInformation &
                             as.numeric(data1[,4])< input$Range),1:4]
        
      }
      if (input$Howoften != "All"){
        data1<-data1[which(data1[,1]==input$Howoften &
                             as.numeric(data1[,4])< input$Range),1:4]  
      }
      DT::datatable(data = data1)
    }
    
  })
  
  output$graph<-renderPlot(
    {
      if (input$BasicInformation == "All"){
        g=ggplot(data, aes(HowOften))+geom_bar(aes(fill=HowOften))+labs(fill = " ")
      }
      else if (input$BasicInformation != "All" && input$Howoften == "All"){
        g=ggplot(selected_data(), aes(HowOften))+geom_bar(aes(fill=selected_data()[,input$BasicInformation]))+labs(fill = " ") 
      }
      else if (input$BasicInformation != "All" && input$Howoften != "All"){
        da=selected_data()[which(selected_data()$HowOften==input$Howoften),]
        g=ggplot(da, aes(da[,input$BasicInformation]))+geom_bar(aes(fill=da[,input$BasicInformation]))+labs(fill = " ")+labs(x=VarName()) 
      }
      g 
    }
  )
  
  observeEvent(input$apply,
               {output$amazing<-renderText({"Her name is Yuke Fu."})})
  
}

shinyApp(ui = ui, server = server)


