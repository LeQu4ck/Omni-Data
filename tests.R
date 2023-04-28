library(lubridate)
library(tidyr)
library(tidyverse)
library(readxl)
library(forecast)
library(tidyr)
library(modeltime)
library(lubridate)
library(tidyverse)
library(timetk)
library(rsample)
library(tidymodels)      


myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))
tabelEMEA <- data.frame(pivot_wider(omniData, names_from = Date, values_from = Value, names_prefix = "")) 


omniFiltered <- omniData %>% filter(year(Date) == 2020 & grepl("(SG&A|FX Other)", Account))

View(omniFiltered)

 loopDF <- omniData %>% filter(year(Date) == "2021" & Cluster == "EMEA")
dfTemp <- loopDF %>% filter(Account == "Gross Trade Sales")
plotList <- list()

acc<- unique(omniData$Account)
for(account in acc){
  dfTemp <- loopDF %>% filter(Account == account)
  
  historyTEST <- dfTemp %>%
    ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
    geom_line(size = 1.2) +
    scale_color_viridis(discrete = TRUE)+
    ylab("Sales") + 
    ggtitle(paste0(account, " Historic Data")) +
    scale_y_continuous(labels = function(Value)format(Value, scientific = FALSE)) +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",   
      legend.margin = margin(t = 10),
    )
  
  plotList[[account]] <- historyTEST
}


View(plotList)


graphDf <- omniFiltered %>%
  filter( Cluster == "EMEA") %>%
  select(Date, Account, Value)

graphAllYears <- omniData %>% filter(Account == "Gross Trade Sales")%>% 
  select(Date, Account, Cluster, Value)

View(graphAllYears)

View(dfTemp)

historyPlot <- graphAllYears %>%
  ggplot(aes(x=Date, y=Value, group = Cluster, color = Cluster)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ylab("Sales")+ 
  theme(
  legend.position = "bottom",  
  legend.box = "horizontal",   
  legend.margin = margin(t = 10)) 

plot(historyPlot)

head(omniData)

omniDataEMEA <- data.frame(pivot_wider(omniData, names_from = Account, values_from = Value)) %>% filter(Cluster == "EMEA")
omniDataEMEA <- omniDataEMEA %>% select(-c("Cluster"))
View(omniDataEMEA)

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
    
    #Linear regression
    
    model_fit_lm <- linear_reg() %>% 
      set_engine(("lm")) %>% 
      fit(Values ~ as.numeric(Date), ordered = FALSE, data = training(splits))
    
    #Prophet 
    
    model_fit_prophet <- prophet_reg() %>% 
      set_engine(engine = "prophet") %>% 
      fit(Values ~ Date, data = training(splits))
    
    # Naive
    model_fit_naive <- naive_reg() %>%
      set_engine("naive") %>%
      fit(Values ~ Date, data = training(splits))
    
    #SNaive
    model_fit_snaive <- naive_reg() %>%
      set_engine("snaive") %>%
      fit(Values ~ Date, data = training(splits))
    
    #Median forecast
    model_fit_medf <- window_reg(
      window_size = 7
    ) %>%
      set_engine(
        engine = "window_function",
        window_function = median,
        na.rm = TRUE
      ) %>%
      fit(Values~ Date, data = training(splits))
    
    #Mean forecast
    model_fit_meanf <- window_reg(
      window_size = 7
    ) %>%
      set_engine(
        engine = "window_function",
        window_function = mean,
        na.rm = TRUE
      ) %>%
      fit(Values~ Date, data = training(splits))
    
    #Weighted forecast
    model_fit_wf <- window_reg(
      window_size = 7
    ) %>%
      set_engine(
        engine = "window_function",
        window_function = ~ sum(tail(.x,3) * c(0.1, 0.3, 0.6)),
        na.rm = TRUE
      ) %>%
      fit(Values~ Date, data = training(splits))
    
    #ARIMA
    model_fit_ARIMA <- arima_reg() %>%
      set_engine("auto_arima") %>%
      fit(Values ~ Date, data = training(splits))
    
    #TBATS
    model_fit_TBATS <- seasonal_reg() %>%
      set_engine("tbats") %>%
      fit(Values ~ Date, data = training(splits))
    
    #Nnetar
    model_fit_NNETAR <- nnetar_reg() %>%
      set_engine("nnetar") %>%
      fit(Values ~ Date, data = training(splits))
    
    
    models_tbl <- modeltime_table(
      model_fit_prophet,
      model_fit_lm,
      model_fit_naive,
      model_fit_snaive,
      model_fit_medf,
      model_fit_meanf,
      model_fit_wf,
      model_fit_ARIMA,
      model_fit_TBATS,
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
    
    #Logic
    i2_beg <- forecastPeriod + 1
    i2_end <- 2*forecastPeriod
    
    i3_beg <- i2_beg + 1
    i3_end <- 3*forecastPeriod
    
    i4_beg <- i3_end + 1
    i4_end <- 4*forecastPeriod
    
    i5_beg <- i4_end + 1
    i5_end <- 5*forecastPeriod
    
    i6_beg <- i5_end + 1
    i6_end <- 6*forecastPeriod
    
    i7_beg <- i6_end + 1
    i7_end <- 7*forecastPeriod
    
    i8_beg <- i7_end + 1
    i8_end <- 8*forecastPeriod
    
    i9_beg <- i8_end + 1
    i9_end <- 9*forecastPeriod
    
    i10_beg <- i9_end + 1
    i10_end <- 10*forecastPeriod
    
    final_forecast[1:forecastPeriod, 1] <- "Naive"
    final_forecast[i2_beg:i2_end, 1] <- "SNaive"
    final_forecast[i3_beg:i3_end, 1] <- "MF"
    final_forecast[i4_beg:i4_end, 1] <- "MEF"
    final_forecast[i5_beg:i5_end, 1] <- "WAF"
    final_forecast[i6_beg:i6_end, 1] <- "ARIMA"
    final_forecast[i7_beg:i7_end, 1] <- "TBATS"
    final_forecast[i8_beg:i8_end, 1] <- "NNETAR"
    final_forecast[i9_beg:i9_end, 1] <- "PROPHET"
    final_forecast[i10_beg:i10_end, 1] <- "LM"
    
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

  View(finalDf)  
  