library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(readxl)
library(lubridate)
library(tidyr)
library(tidyverse)
library(DT)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(plotly)
library(stringr)
library(forecast)
library(modeltime)
library(timetk)
library(rsample)
library(tidymodels) 


options(spinner.color="#2596be", spinner.color.background="#ffffff", spinner.size=1.4)

ui <- dashboardPage(
  
  skin = "blue",
  
  dashboardHeader(title = "OmniBI"
  ),
  dashboardSidebar(
    
    sidebarMenu(id = "sidebar",
                menuItem("Actual data", tabName = "actual",  icon = icon("database")),
                menuItem("Prediction", tabName = "prediction", icon = icon("clock"))
    ),
    
    uiOutput("sidebar_input1"),
    uiOutput("sidebar_input2"),
    uiOutput("sidebar_input3"),
    actionBttn("selectData", "Select data set")
    
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .modal-header {
          background-color: #2596be;
          color: #ffffff;
        }
        .modal-title {
          font-weight: bold;
          font-size: 2rem;
        }
        .modal-footer {
          background-color: #222d32;
        }
        .modal-body {
          background-color: #222d32;
        }
        .modal-content {
          border-radius: 1rem;
          box-shadow: 0 0.5rem 1rem rgba(0, 0, 0, 0.15);
        }
        #ok {
          background-color: #2596be;
          color: #ffffff;
        }
        #ok:hover {
          background-color: green;
        }
        .close-modal-btn {
          background-color: red;
          color: #ffffff;
        }
        .box-header {
          background-color: #222d32;
          color: #ffffff;
        }
        #selectData {
          background-color: #2596be;
          color: #ffffff;
          transition: all 0.5s ease-in-out;
        }
        #selectData:hover {
          color: black;
          box-shadow: 0 0 2px #fff, 0 0 10px #fff, 0 0 20px #0ba9ca, 0 0 30px #0ba9ca;
        }
        #predict {
          background-color: #2596be;
          color: #ffffff;
          transition: all 0.5s ease-in-out;
          margin-bottom: 15px;
        }
        #predict:hover {
          color: black;
          box-shadow: 0 0 2px #fff, 0 0 10px #fff, 0 0 20px #0ba9ca, 0 0 30px #0ba9ca;
        }
        .navbar navbar-static-top{
        position: fixed;
        }
        .fa-check:hover {
          color: black;
        }
        
      "))
    ),
    tabItems( 
      # First tab content
      tabItem(tabName = "actual",
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
      tabItem(tabName = "prediction",
              
              fluidRow(
                
                box(title = "Prediction table EMEA", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput("predictie_EMEA")),  width = 12),
                
                box(title = "Prediction table NA", collapsible = TRUE, solidHeader = TRUE, withSpinner(DT::dataTableOutput("predictie_NA")),  width = 12)
                
              ),
              
              fluidRow(
                
                box(shinycssloaders::withSpinner(plotOutput("predictieEMEAgraph")),  height = 450, width = 6),
                
                box(shinycssloaders::withSpinner(plotOutput("predictieNAgraph")),  height = 450, width = 6)
                
              )
      )
    )
  )
)

server <- function(input, output) {

  #myDataLocation <-"C:/Users/ocris/Desktop/omni.xlsx" 
  myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx" 
  #myDataLocation <-"C:/Users/flori/OneDrive/Desktop/Omni-Data-main/Omni_Data.xlsx"
  
  omniData <- reactiveVal()  
  
  observeEvent(input$selectData, {
    showModal(modalDialog(
      
      title = "Select data set",
      
      footer = tagList(
        
        tags$button(id = "ok", tags$i(class = "fa fa-check"), type = "button", class = "btn action-button btn-secondary"),
        
        tags$button(tags$i(class = "fa fa-x"), type = "button", class = "btn btn-secondary close-modal-btn", `data-dismiss` = "modal")
        
      ),
      
      size = "l",
      
      id = "dataModal",
      
      radioGroupButtons(
        
        "dataSelection",
        
        choices = c("Default data", "Upload new data"),
        
        justified = TRUE
      ),
      conditionalPanel(
        
        "input.dataSelection == 'Upload new data'",
        
        fileInput("dataFile", "Choose data file", accept = c(".xlsx"))
      )
    ))
  })
  
  observeEvent(input$ok, {
    req(input$dataSelection) 
    
    if (input$dataSelection == "Default data") {

      data <- read_excel(myDataLocation) %>%
        mutate(Date = as.Date(Date, tz = "UTC")) 
      
      data$Value <- round(data$Value, 2)
      
    } else if (input$dataSelection == "Upload new data") {
      
      req(input$dataFile) 
      
      tempLocation <- input$dataFile
      
      data <- read_excel(tempLocation$datapath, 1) %>%
        mutate(Date = as.Date(Date, tz = "UTC")) 
      
      data$Value <- round(data$Value, 2)
    }
    
    if (exists("data")) {
      omniData(data)  
    }
    removeModal()
  })
  
  #primul input generat dinamic
  output$sidebar_input1 <- renderUI({
    
    req(omniData())
    
    if (input$sidebar == "actual") {
      
      selectInput("selectYear", "Choose year :",
                  
                  choices = c(unique(year(omniData()$Date))),
                  
                  selected = "2021"
      )
      
    } else if (input$sidebar == "prediction") {
      
      selectInput("predictAccount", "Choose an account: ",
                  
                  choices = c(unique(omniData()$Account)),
                  
                  selected = "Gross Trade Sales")
      
    }
  }) 
  
  #al doilea input generat dinamic
  output$sidebar_input2 <- renderUI({
    
     req(omniData()) 
    
     if (input$sidebar == "actual") {
      
      selectInput("selectMonth", "Choose month :",
                  
                  choices = c("All","January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"),
                  
                  selected = "All"
      )
      
    } else if (input$sidebar == "prediction") {
      
      actionBttn("predict", "Predict")
      
    } else if (input$sidebar == "3rdtab") {
      
      textInput("input2", "input 2 tab3")
      
    }
  })
  
  #al treilea input generat dinamic
  output$sidebar_input3 <- renderUI({
    
    req(omniData())
    
    if (input$sidebar == "actual") {
      
      req(omniData())
      
      selectInput("selectAcc", "Choose account :",
                  choices = c("All", 
                              unique(omniData()$Account)),
                  selected = "All"
      )
    } 
  }) 

  reactiveData <- reactive({

    req(input$selectYear)
    
    req(input$selectMonth)
    
    req(input$selectAcc)
    
    if (input$selectMonth == "All" & input$selectAcc == "All") {
      
      omniFiltered <- omniData() %>% filter(year(Date) == input$selectYear & 
                                            Value != 0
      )
      
    } else if (input$selectMonth == "All" & input$selectAcc != "All") {
      
      omniFiltered <- omniData() %>% filter(year(Date) == input$selectYear &
                                            Account == input$selectAcc &
                                            Value != 0
      )
      
    } else if (input$selectMonth != "All" & input$selectAcc == "All") {
      
      omniFiltered <- omniData() %>% filter(year(Date) == input$selectYear &
                                            month(Date, label = TRUE, abbr = FALSE, locale = "English") == input$selectMonth &
                                            Value != 0
      )
      
    } else {
      
      omniFiltered <- omniData() %>% filter(year(Date) == input$selectYear &
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
    
    dfGraphSet1EMEA <- omniData() %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & Account %in% c("Gross Trade Sales", "Net Trade Sales", "SGM")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet1NA <- omniData() %>% filter(Cluster == "NA" & year(Date) == input$selectYear & Account %in% c("Gross Trade Sales", "Net Trade Sales", "SGM")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet2EMEA <- omniData() %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & Account %in% c("OCOS")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet2NA <- omniData() %>% filter(Cluster == "NA" & year(Date) == input$selectYear & Account %in% c("OCOS")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet3EMEA <- omniData() %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & grepl("(SG&A|FX Other)", Account)) %>% 
      select(Date, Account, Value)
    
    dfGraphSet3NA <- omniData() %>% filter(Cluster == "NA" & year(Date) == input$selectYear & grepl("(SG&A|FX Other)", Account)) %>% 
      select(Date, Account, Value)
    
    dfGraphSet4EMEA <- omniData() %>% filter(Cluster == "EMEA" & year(Date) == input$selectYear & Account %in% c("Trade OM")) %>% 
      select(Date, Account, Value)
    
    dfGraphSet4NA <- omniData() %>% filter(Cluster == "NA" & year(Date) == input$selectYear & Account %in% c("Trade OM")) %>% 
      select(Date, Account, Value)
    
    graphAllYears <- omniData() %>% filter(Account == input$selectAcc)%>% 
      select(Date, Account, Cluster, Value)
    
    historyGraphSet1EMEA <- dfGraphSet1EMEA %>%
      ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
      geom_line(size = 1.2) +
      geom_point(size = 4) +
      scale_color_viridis(discrete = TRUE)+
      ylab("Sales") + 
      ggtitle(paste(input$selectYear, "EMEA Historical Data for Gross Trade Sales, Net Trade Sales and SGM"))+
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
      ggtitle(paste(input$selectYear, "NA Historical Data for Gross Trade Sales, Net Trade Sales and SGM")) +
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
      ggtitle(paste(input$selectYear, "EMEA Historical Data for OCOS")) +
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
      ggtitle(paste(input$selectYear, "NA Historical Data for OCOS")) +
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
      ggtitle(paste(input$selectYear, "EMEA Historical Data for SG&A and FX Other")) +
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
      ggtitle(paste(input$selectYear, "NA Historical Data for SG&A and FX Other")) +
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
      ggtitle(paste(input$selectYear, "EMEA Historical Data for Trade OM")) +
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
      ggtitle(paste(input$selectYear, "NA Historical Data for SG&A and Trade OM")) +
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
      ggtitle(paste("All Years Historical Data for", input$selectAcc)) +
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
  
  #####################################INCEPUT FORECAST########################################
  observeEvent(input$predict, {
  
    account <- input$predictAccount
    
    ###FORECAST EMEA

    omniDataEMEA <- omniData() %>% filter(Cluster == "EMEA" & Account == account & Date < '2021-12-01')
    omniDataEMEA <- data.frame(pivot_wider(omniDataEMEA, names_from = Account, values_from = Value))
    omniDataEMEA <- omniDataEMEA %>% select(-c("Cluster"))
    
    for(i in 2:ncol(omniDataEMEA)){
      omniDataEMEA[i] <- as.numeric(tsclean(omniDataEMEA[[i]], replace.missing = TRUE, lambda = "auto"))
    }
    
    forecastPeriod <- 13
    dummyDFEmea <- data.frame(Timeseries = character(), Model = character(), Date = character(), Forecast = character())
    
    for(i in 2:ncol(omniDataEMEA)){
      
      #Prepare data
      mainDataForecast <- cbind(omniDataEMEA[1], omniDataEMEA[i])
      colnames(mainDataForecast)[2] <- "Values"
      
      splits <- initial_time_split(mainDataForecast, prop=0.75)
      
      #create and fit models
      model_fit_prophet <- prophet_reg() %>% 
        set_engine(engine = "prophet") %>% 
        fit(Values ~ Date, data = training(splits)) 
      
      models_tbl <- modeltime_table(
        model_fit_prophet
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
      
      final_forecast[1:forecastPeriod, 1] <- "PROPHET"
      
      final_forecast <- cbind(as.data.frame(colnames(omniDataEMEA[i])), final_forecast)
      
      colnames(final_forecast)[1] <- "Timeseries"
      colnames(final_forecast)[2] <- "Model"
      colnames(final_forecast)[3] <- "Date"
      colnames(final_forecast)[4] <- "Forecast"
      
      #final data
      dummyDFEmea <- data.frame(rbind(as.matrix(dummyDFEmea), as.matrix(final_forecast)))
      
    }
    
    #FINAL FORECAST EMEA
    finalDfEmea <- dummyDFEmea[which(dummyDFEmea$Model == "PROPHET"),]
    outputFinalDFEmea <- data.frame(pivot_wider(finalDfEmea, names_from = Date, values_from = Forecast)) 
    outputFinalDFEmea <- outputFinalDFEmea %>% select(-c("Timeseries", "Model"))
    
    ################################FORECAST NA#####################################################3
    omniDataNA <- omniData() %>% filter(Cluster == "NA" & Account == account & Date < '2021-12-01')
    omniDataNA <- data.frame(pivot_wider(omniDataNA, names_from = Account, values_from = Value))
    omniDataNA <- omniDataNA %>% select(-c("Cluster"))
    
    for(i in 2:ncol(omniDataNA)){
      omniDataNA[i] <- as.numeric(tsclean(omniDataNA[[i]], replace.missing = TRUE, lambda = "auto"))
    }
    
    forecastPeriod <- 13
    dummyDFNa <- data.frame(Timeseries = character(), Model = character(), Date = character(), Forecast = character())
    
    for(i in 2:ncol(omniDataNA)){
      
      #Prepare data
      mainDataForecast <- cbind(omniDataNA[1], omniDataNA[i])
      colnames(mainDataForecast)[2] <- "Values"
      
      splits <- initial_time_split(mainDataForecast, prop=0.75)
      
      #create and fit models
      model_fit_snaive <- naive_reg() %>%
        set_engine("snaive") %>%
        fit(Values ~ Date, data = training(splits))
      
      models_tbl <- modeltime_table(
        model_fit_snaive
      )
      
      #Calibrate model to testing set
      calibration_tbl <- models_tbl %>% 
        modeltime_calibrate(
          new_data = testing(splits)
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
      
      final_forecast[1:forecastPeriod, 1] <- "SNAIVE"
      
      final_forecast <- cbind(as.data.frame(colnames(omniDataNA[i])), final_forecast)
      
      colnames(final_forecast)[1] <- "Timeseries"
      colnames(final_forecast)[2] <- "Model"
      colnames(final_forecast)[3] <- "Date"
      colnames(final_forecast)[4] <- "Forecast"
      
      #final data
      dummyDFNa <- data.frame(rbind(as.matrix(dummyDFNa), as.matrix(final_forecast)))
      
    }
    
    #FINAL FORECAST NA
    finalDFNa <- dummyDFNa[which(dummyDFNa$Model == "SNAIVE"),]
    outputFinalDFNa <- data.frame(pivot_wider(finalDFNa, names_from = Date, values_from = Forecast)) 
    outputFinalDFNa <- outputFinalDFNa %>% select(-c("Timeseries", "Model"))
    
    ##############3#OUTPUT NA AND EMEA FOREASTS##########################################################################################
    ##TABLE OUTPUT####
    output$predictie_EMEA <- DT::renderDT({
      req(outputFinalDFEmea)
      DT::datatable(outputFinalDFEmea, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), caption = account )
    })
    
    output$predictie_NA <- DT::renderDT({
      req(outputFinalDFNa)
      DT::datatable(outputFinalDFNa, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE), caption = account )
    })
    
    #################OUTPUT GRAFICE PREZICERE######################################
    output$predictieEMEAgraph <- renderPlot({
      dfGraphPredictEMEA <- finalDfEmea %>% 
        mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% # convert to date format
        select(Date, Forecast)
      ggplot(dfGraphPredictEMEA, aes(x = Date, y = Forecast, group = 1, color = "EMEA")) +
        geom_line(size = 1.2) +
        geom_point(size = 4) +
        ggtitle(paste(account, "prediction for EMEA")) +
        scale_color_manual(values = c("EMEA" = "red")) + 
        labs(color = "Region")
    })
    
    output$predictieNAgraph <- renderPlot({
      dfGraphPredictNA <- finalDFNa %>% 
        mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>% # convert to date format
        select(Date, Forecast)
      ggplot(dfGraphPredictNA, aes(x = Date, y = Forecast, group = 1, color = "NA")) +
        geom_line(size = 1.2) +
        geom_point(size = 4) +
        ggtitle(paste(account, "prediction for NA")) +
        scale_color_manual(values = c("NA" = "blue")) + 
        labs(color = "Region")
    })
    
  })
  
  #REACTIVE DATA
  output$tabel_EMEA <- DT::renderDT({
    req(reactiveData()$tabel_EMEA, cancelOutput = TRUE)
    DT::datatable(reactiveData()$tabel_EMEA, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:4))))
  })
  
  output$tabel_NA <- DT::renderDT({
    req(reactiveData()$tabel_NA, cancelOutput = TRUE)
    DT::datatable(reactiveData()$tabel_NA, options = list(pageLength = 5, lengthChange = FALSE, searching = FALSE, columnDefs = list(list(className = 'dt-center', targets = 0:4))))
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