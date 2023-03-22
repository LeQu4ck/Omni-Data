library(shiny)
library(readxl)
library(lubridate)
library(tidyr)

myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      helpText("Alege An"),
      selectInput("selectYear", "Years :",
                  unique(year(omniData$Date))
      )
    ),
    mainPanel(
      fluidRow(
        column(12,
               helpText("Tabel pentru EMEA"),
               tableOutput("tabel_EMEA")
        ),
        column(12,
               helpText("Tabel pentru NA"),
               tableOutput("tabel_NA")
        )
      )
    )
  )
)

server <- function(input, output) {
  
  observeEvent(input$selectYear, {
    
    omniFiltered <- data.frame(omniData %>% filter(year(Date) == input$selectYear & Value != 0))
    
    tabelEMEA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    
    tabelNA <- data.frame(pivot_wider(omniFiltered, names_from = Date, values_from = Value, names_prefix = "")) 
    
    
    output$tabel_EMEA <- renderTable({
      
      tabelEMEA <- tabelEMEA %>% filter(tabelEMEA$Cluster == "EMEA")
      
    })
    
    output$tabel_NA <- renderTable({
      
      tabelNA <- tabelNA %>% filter(tabelNA$Cluster == "NA")
      
    })
    
  })  
  
}

shinyApp(ui, server)