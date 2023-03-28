library(shiny)
library(readxl)
library(lubridate)
library(tidyr)
library(tidyverse)
library(DT)

myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      helpText("Alege An"),
      selectInput("selectYear", "Years :",
                  unique(year(omniData$Date))
      ),      
      helpText("Alege Luna"),
      selectInput("selectMonth", "Month :",
                  choices = c("All","January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                  selected = "All"
      )
    ),
    mainPanel(
      fluidRow(
        column(12,
               p("Tabel pentru EMEA"),
               tableOutput("tabel_EMEA")
        ),
        column(12,
               p("Tabel pentru NA"),
               tableOutput("tabel_NA")
        )
      )
    )
  )
)
 
server <- function(input, output) {
  
  myReactiveFunction <- reactive({
    
  
    
    if(input$selectMonth == "All"){
      
    omniFiltered <- data.frame(omniData %>% filter(year(Date) == input$selectYear & Value != 0))
    
    }else{
      
    omniFiltered <- data.frame(omniData %>% filter(year(Date) == input$selectYear & lubridate::month(omniData$Date, label = TRUE, abbr = FALSE, locale = "English") == input$selectMonth & Value != 0))
    
    }
    

    tabelEMEA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    tabelNA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    tabelEMEA <- tabelEMEA %>% filter(tabelEMEA$Cluster == "EMEA")
    
    tabelNA <- tabelNA %>% filter(tabelNA$Cluster == "NA")
    
    list(tabel_EMEA = tabelEMEA, tabel_NA = tabelNA)
  })
  
  output$tabel_EMEA <- renderTable({
    myReactiveFunction()$tabel_EMEA
  })
  
  output$tabel_NA <- renderTable({
    myReactiveFunction()$tabel_NA
  })
  
}

shinyApp(ui, server)
