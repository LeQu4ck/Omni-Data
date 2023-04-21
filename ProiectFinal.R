library(shiny)
library(readxl)
library(lubridate)
library(tidyr)
library(tidyverse)
library(DT)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(shinydashboard)

myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

options(width=120)

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                menuItem("Istoric", tabName = "istoric",  icon = icon("database")),
                menuItem("Predictie", tabName = "predictie", icon = icon("clock")),
                menuItem("Al 3 lea tab", tabName = "3rdtab", icon = icon("dice"))
    ),
    uiOutput("sidebar_input1"),
    uiOutput("sidebar_input2"),
    uiOutput("sidebar_input3")
  ),
  dashboardBody(
    tabItems( 
      # First tab content
      tabItem(tabName = "istoric",
              fluidRow( 
                
                box(DT::dataTableOutput("tabel_EMEA"),  height = 460,width = 12)
                
                
                
              ),
              fluidRow(
                box(plotOutput("graphEMEA"),  height = 420, width = 6),
                
                box(plotOutput("graphNA"),  height = 420, width = 6),
                
                
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "predictie",
              h2("Predictie date")
      ),
      # Thrid tab content
      tabItem(tabName = "3rdtab",
              h2("Al 3 lea tab")
      )
    )
  )
)

server <- function(input, output, session) {
  
  #primul input generat dinamic
  output$sidebar_input1 <- renderUI({
    if (input$sidebar == "istoric") {
      selectInput("selectYear", "Choose year :",
                  choices = c(unique(year(omniData$Date))),
                  selected = "2021"
      )
    } else if (input$sidebar == "predictie") {
      textInput("input1", "input 2 gen")
    } else if (input$sidebar == "3rdtab") {
      textInput("input1", "input 3 gen")
    }
  }) 
  
  #al doilea input generat dinamic
  output$sidebar_input2 <- renderUI({
    if (input$sidebar == "istoric") {
      selectInput("selectMonth", "Choose month :",
                  choices = c("All","January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                  selected = "All"
      )
    } else if (input$sidebar == "predictie") {
      textInput("input2", "input 2 tab2")
    } else if (input$sidebar == "3rdtab") {
      textInput("input2", "input 2 tab3")
    }
  }) 
  
  #al treilea input generat dinamic
  output$sidebar_input3 <- renderUI({
    if (input$sidebar == "istoric") {
      selectInput("selectAcc", "Choose account :",
                  choices = c("All", unique(omniData$Account)),
                  selected = "All"
      )
    } else if (input$sidebar == "predictie") {
      textInput("input3", "input 3 tab2")
    } else if (input$sidebar == "3rdtab") {
      textInput("input3", "input 3 tab3")
    }
  }) 
  
  omniData$Value <- round(omniData$Value, 2)
  
  reactiveData <- reactive({
    
    if (input$selectMonth == "All" & input$selectAcc == "All") {
      
      omniFiltered <- omniData %>% filter(year(Date) == input$selectYear & 
                                            Value != 0
      )
      
    } else if (input$selectMonth == "All" & input$selectAcc != "All") {
      
      omniFiltered <- omniData %>% filter(year(Date) == input$selectYear &
                                            Account == input$selectAcc &
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
                                            Account == input$selectAcc &
                                            Value != 0
      )
    }
    
    
    tabelEMEA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    tabelNA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    tabelEMEA <- tabelEMEA %>% 
      filter(Cluster == "EMEA")
    
    tabelNA <- tabelNA %>% 
      filter(Cluster == "NA") 
    
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
        legend.margin = margin(t = 10),
      )  
    
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
