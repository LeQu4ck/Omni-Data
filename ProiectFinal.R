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
library(plotly)
library(shinycssloaders)
library(stringr)
library(forecast)
library(modeltime)
library(timetk)
library(rsample)
library(tidymodels) 

options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "OmniBI"
  ),
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
                
                box(title = "EMEA History", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput("tabel_EMEA")), width = 12),
                
                box(title = "NA History", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput("tabel_NA")), width = 12)
                
              ),
              fluidRow(
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet1EMEA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet1NA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet2EMEA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet2NA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet3EMEA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet3NA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet4EMEA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphSet4NA")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("graphAllYears")),  height = 450, width = 12),
                
              )
      ),
      
      # Second tab content
      tabItem(tabName = "predictie",
              fluidRow(
                box(title = "Prediction table EMEA", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput("tabel_predictie"))),
                box(title = "Prediction table NA", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput(""))),
                box(title = "Prediction table NA", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput(""))),
                box(title = "Prediction table NA", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput(""))),
                
              )
              
      ),
      # Thrid tab content
      tabItem(tabName = "3rdtab",
              h2("Al 3 lea tab")
      )
    )
  )
)

server <- function(input, output) {
  
  #myDataLocation <-"C:/Users/ocris/Desktop/omni.xlsx"
  myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"
  omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))
  
  acc <- unique(omniData$Account)
  
  #primul input generat dinamic
  output$sidebar_input1 <- renderUI({
    if (input$sidebar == "istoric") {
      selectInput("selectYear", "Choose year :",
                  choices = c(unique(year(omniData$Date))),
                  selected = "2021"
      )
    } else if (input$sidebar == "predictie") {
      selectInput("predictAccount", "Choose an account: ",
                  choices = c(unique(omniData$Account)),
                  selected = "Gross Trade Sales")
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
      actionButton("predict", "Predict")
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
      #textInput("input3", "input 3 tab2")
    } else if (input$sidebar == "3rdtab") {
      textInput("input3", "input 3 tab3")
    }
  }) 
  
  omniData$Value <- round(omniData$Value, 2)
  
  reactiveData <- reactive({
    
    req(input$selectYear)
    req(input$selectMonth)
    req(input$selectAcc)
    
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
    
    dfGraphSet1EMEA <- omniData %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & Account %in% c("Gross Trade Sales", "Net Trade Sales", "SGM")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet1NA <- omniData %>% filter(Cluster == "NA" & year(Date) == input$selectYear & Account %in% c("Gross Trade Sales", "Net Trade Sales", "SGM")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet2EMEA <- omniData %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & Account %in% c("OCOS")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet2NA <- omniData %>% filter(Cluster == "NA" & year(Date) == input$selectYear & Account %in% c("OCOS")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet3EMEA <- omniData %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & grepl("(SG&A|FX Other)", Account)) %>% 
      select(Date, Account, Value)
    
    dfGraphSet3NA <- omniData %>% filter(Cluster == "NA" & year(Date) == input$selectYear & grepl("(SG&A|FX Other)", Account)) %>% 
      select(Date, Account, Value)
    
    dfGraphSet4EMEA <- omniData %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & Account %in% c("Trade OM")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet4NA <- omniData %>% filter(Cluster == "NA" & year(Date) == input$selectYear & Account %in% c("Trade OM")) %>% 
      select(Date, Account, Value)
    
    graphAllYears <- omniData %>% filter(Account == input$selectAcc & Date < "2021-12-01")%>% 
      select(Date, Account, Cluster, Value)
    
    historyGraphSet1EMEA <- dfGraphSet1EMEA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      scale_color_viridis(discrete = TRUE)+
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "EMEA Historic Data for Gross Trade Sales, Net Trade Sales and SGM"))+
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10),
      )
    
    historyGraphSet1NA <- dfGraphSet1NA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "NA Historic Data for Gross Trade Sales, Net Trade Sales and SGM")) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) + 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10))
    
    historyGraphSet2EMEA <- dfGraphSet2EMEA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      scale_color_viridis(discrete = TRUE)+
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "EMEA Historic Data for OCOS")) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10),
      )
    
    historyGraphSet2NA <- dfGraphSet2NA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "NA Historic Data for OCOS")) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) + 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10))
    
    historyGraphSet3EMEA <- dfGraphSet3EMEA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      scale_color_viridis(discrete = TRUE)+
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "EMEA Historic Data for SG&A and FX Other")) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10),
      )
    
    historyGraphSet3NA <- dfGraphSet3NA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "NA Historic Data for SG&A and FX Other")) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) + 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10))
    
    historyGraphSet4EMEA <- dfGraphSet4EMEA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      scale_color_viridis(discrete = TRUE)+
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "EMEA Historic Data for Trade OM")) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) +
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10),
      )
    
    historyGraphSet4NA <- dfGraphSet4NA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "NA Historic Data for SG&A and Trade OM")) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) + 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10))
    
    historyAllYears <- graphAllYears %>%
      ggplot(aes(x=Date, y=Value, group = Cluster, color = Cluster)) +
      geom_line(size = 1.2) +
      ylab("Sales") + 
      ggtitle(paste("All Years Historic Data for", input$selectAcc)) +
      scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE))+ 
      theme(
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box = "horizontal",   
        legend.margin = margin(t = 10),
        plot.title = element_text(size = 24))  
    
    list(tabel_EMEA = tabelEMEA, tabel_NA = tabelNA, 
         graphEMEASet1 = historyGraphSet1EMEA, graphNASet1 = historyGraphSet1NA,
         graphEMEASet2 = historyGraphSet2EMEA, graphNASet2 = historyGraphSet2NA, 
         graphEMEASet3 = historyGraphSet3EMEA, graphNASet3 = historyGraphSet3NA, 
         graphEMEASet4 = historyGraphSet4EMEA, graphNASet4 = historyGraphSet4NA, 
         graphAllYears = historyAllYears
    )
  })
  
  
  observeEvent(input$predict, {
    omniDataEMEA <- omniData %>% filter(Cluster == "EMEA" & Account == input$predictAccount & Date < '2021-12-01')
    omniDataEMEA <- data.frame(pivot_wider(omniDataEMEA, names_from = Account, values_from = Value))
    omniDataEMEA <- omniDataEMEA %>% select(-c("Cluster"))
    
    for(i in 2:ncol(omniDataEMEA)){
      omniDataEMEA[i] <- as.numeric(tsclean(omniDataEMEA[[i]], replace.missing = TRUE, lambda = "auto"))
    }
    
    forecastPeriod <- 13
    dummyDF <- data.frame(Timeseries = character(), Model = character(), Date = character(), Forecast = character())
    
    for(i in 2:ncol(omniDataEMEA)){
      
      #Prepare data
      mainDataForecast <- cbind(omniDataEMEA[1], omniDataEMEA[i])
      colnames(mainDataForecast)[2] <- "Values"
      
      splits <- initial_time_split(mainDataForecast, prop=0.75)
      
      #create and fit models
      #Nnetar
      model_fit_NNETAR <- nnetar_reg() %>%
        set_engine("nnetar") %>%
        fit(Values ~ Date, data = training(splits))
      
      
      models_tbl <- modeltime_table(
        model_fit_NNETAR
      )
      
      #Calibrate model to testing set
      calibration_tbl <- models_tbl %>% 
        modeltime_calibrate(
          new_data = testing(splits)
        )
      
      #visual 
      calibration_tbl %>% 
        modeltime_forecast(
          new_data = testing(splits),
          actual_data =  mainDataForecast
        ) %>% 
        plot_modeltime_forecast(
          .legend_max_width = 25
        )
      
      #metrics
      calibration_tbl %>% 
        modeltime_accuracy() %>% 
        table_modeltime_accuracy()
      
      #Forecast
      refit_tbl <- calibration_tbl %>% 
        modeltime_refit( data = mainDataForecast )
      
      refit_tbl %>% 
        modeltime_forecast(h = forecastPeriod, actual_data = mainDataForecast) %>% 
        plot_modeltime_forecast((
          .legend_ma_width = 25
        ))
      
      final_forecast <- refit_tbl %>% 
        modeltime_forecast( h = forecastPeriod, actual_data = mainDataForecast) %>% 
        filter(.key=="prediction")
      
      final_forecast <- subset(final_forecast, select = c(.model_desc, .index, .value))
      
      final_forecast[1:forecastPeriod, 1] <- "NNETAR"
      
      final_forecast <- cbind(as.data.frame(colnames(omniDataEMEA[i])), final_forecast)
      
      colnames(final_forecast)[1] <- "Timeseries"
      colnames(final_forecast)[2] <- "Model"
      colnames(final_forecast)[3] <- "Date"
      colnames(final_forecast)[4] <- "Forecast"
      
      #final data
      dummyDF <- data.frame(rbind(as.matrix(dummyDF), as.matrix(final_forecast)))
      
    }
    
    #FINAL FORECAST
    finalDf <- dummyDF[which(dummyDF$Model == "NNETAR"),]
    outputFinalDF <- data.frame(pivot_wider(finalDf, names_from = Date, values_from = Forecast)) 
    output$tabel_predictie <- DT::renderDT({
      req(outputFinalDF)
      DT::datatable(outputFinalDF, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), caption = input$predictAccount )
    })
  })
  
  
  output$tabel_EMEA <- DT::renderDT({
    req(reactiveData()$tabel_EMEA, cancelOutput = TRUE)
    DT::datatable(reactiveData()$tabel_EMEA, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE))
  })
  
  output$tabel_NA <- DT::renderDT({
    req(reactiveData()$tabel_NA, cancelOutput = TRUE)
    DT::datatable(reactiveData()$tabel_NA, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE))
  })
  
  output$graphSet1EMEA <- renderPlot({
    reactiveData()$graphEMEASet1
  })
  
  output$graphSet1NA <- renderPlot({
    reactiveData()$graphNASet1
  })
  
  output$graphSet2EMEA <- renderPlot({
    reactiveData()$graphEMEASet2
  })
  
  output$graphSet2NA <- renderPlot({
    reactiveData()$graphNASet2
  })
  
  output$graphSet3EMEA <- renderPlot({
    reactiveData()$graphEMEASet3
  })
  
  output$graphSet3NA <- renderPlot({
    reactiveData()$graphNASet3
  })
  
  output$graphSet4EMEA <- renderPlot({
    reactiveData()$graphEMEASet4
  })
  
  output$graphSet4NA <- renderPlot({
    reactiveData()$graphNASet4
  })
  
  output$graphAllYears <- renderPlot({
    reactiveData()$graphAllYears
  })
  
}

shinyApp(ui, server)