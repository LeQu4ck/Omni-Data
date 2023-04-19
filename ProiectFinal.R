library(shiny)
library(readxl)
library(lubridate)
library(tidyr)
library(tidyverse)
library(DT)
library(ggplot2)
library(hrbrthemes)
library(viridis)

myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))

ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("selectYear", "Choose year :",
                  choices = c(unique(year(omniData$Date))),
                  selected = "2021"
      ),      
      
      selectInput("selectMonth", "Choose month :",
                  choices = c("All","January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                  selected = "All"
      ),
      
      selectInput("selectAcc", "Choose account :",
                  choices = c("All", unique(omniData$Account)),
                  selected = "All"
      )
    ),
    mainPanel(
      fluidRow(
        column(12,
               p("Tabel pentru EMEA"),
               DT::dataTableOutput("tabel_EMEA")
        )
      ),
      fluidRow(
        column(12,
               p("Tabel pentru NA"),
               DT::dataTableOutput("tabel_NA")
        )
      ), 
      
      fluidRow(
        column(6,
               p("History plot for EMEA"),
               plotOutput("graphEMEA")
        ),
        column(6,
               p("History plot for NA"),
               plotOutput("graphNA")
        )
      ),
    )
  )
  
)
 
server <- function(input, output) {
  
  reactiveData <- reactive({
    
    #& input$selectMonth == "All" & input$selectAcc == "All" 
    
    if (input$selectMonth == "All" & input$selectAcc == "All") {

      omniFiltered <- omniData %>% filter(year(Date) == input$selectYear & 
                                          Value != 0
                                          )
      
    } else if (input$selectMonth == "All" & input$selectAcc != "All") {

      omniFiltered <- omniData %>% filter(year(Date) == input$selectYear &
                                          any(Account %in% input$selectAcc) &
                                          Value != 0
        )
      
    } else if (input$selectMonth != "All" & input$selectAcc == "All") {

      omniFiltered <- omniData %>% filter(year(Date) == input$selectYear &
                                          month(Date, label = TRUE, abbr = FALSE, locale = "English") == input$selectMonth &
                                          Value != 0
                                          )
      
    } else {

      omniFiltered <- omniData %>% filter(year(Date) == input$selectYear &
                                          month(Date, label = TRUE, abbr = FALSE, locale = "English") == input$selectMonth &
                                          any(Account %in% input$selectAcc) &
                                          Value != 0
                                          )
    }
    

    tabelEMEA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    tabelNA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    tabelEMEA <- tabelEMEA %>% filter(tabelEMEA$Cluster == "EMEA")
    
    tabelNA <- tabelNA %>% filter(tabelNA$Cluster == "NA")
    
    graphDfEMEA <- omniFiltered %>% filter(Cluster == "EMEA") %>% 
      select(Date, Account, Value)
    
    graphDfNA <- omniFiltered %>% filter(Cluster == "NA") %>% 
      select(Date, Account, Value)
    
    historyEMEA <- graphDfEMEA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line() +
      scale_color_viridis(discrete = TRUE) +
      ylab("Sales")+ 
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE))+
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10))  
    
    historyNA <- graphDfNA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line() +
      scale_color_viridis(discrete = TRUE) +
      ylab("Sales")+ 
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE))+ 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10))  
    
    list(tabel_EMEA = tabelEMEA, tabel_NA = tabelNA, graphEMEA = historyEMEA, graphNA = historyNA)
  
  })
  
  
  output$tabel_EMEA <- renderDataTable({
    reactiveData()$tabel_EMEA
  })
  
  output$tabel_NA <- renderDataTable({
    reactiveData()$tabel_NA
  })
  
  output$graphEMEA <- renderPlot({
    reactiveData()$graphEMEA
  })
  
  output$graphNA <- renderPlot({
    reactiveData()$graphNA
  })
  
}

shinyApp(ui, server)
