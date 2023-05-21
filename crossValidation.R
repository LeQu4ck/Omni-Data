library(dplyr)
library(forecast)
library(tidymodels)
library(modeltime)
library(prophet)
library(reshape2)
library(data.table)
library(readxl)

forecastPeriod <- 13
forecastCrossValidation <- 6

dfCrossValidationEMEA <-
  data.frame(
    Item = character(),
    For_Method = character(),
    Date = character(),
    Forecast = numeric()
  )
dfCrossValidationNA <-
  data.frame(
    Item = character(),
    For_Method = character(),
    Date = character(),
    Forecast = numeric()
  )

myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))

omniData <- omniData %>% filter(Date < "2021-12-01")

omniData$Date <- as.Date(omniData$Date, origin = "1970-01-01")

omniData$Value <- tsclean(omniData$Value)

dfCrossValidationEMEA <- data.frame()

for (cluster in unique(omniData$Cluster)) {
  for (account in unique(omniData$Account)) {
    
    cat("\nProcessing Cluster:", cluster, "Account:", account)
    
    omniDataFiltered <- subset(omniData, Cluster == cluster & Account == account)
    
    startCrossValidation <- nrow(omniDataFiltered) - forecastCrossValidation
    
    for (j in 1:forecastCrossValidation) {
      
      crossValidationPeriod <- omniDataFiltered[1:(startCrossValidation + j), ]
      
      model_fit_naive <- naive_reg() %>%
        set_engine("naive") %>%
        fit(Value ~ Date, data = crossValidationPeriod)
      
      #Linear regression
      model_fit_lm <- linear_reg() %>% 
        set_engine(("lm")) %>% 
        fit(Value ~ as.numeric(Date), ordered = FALSE, data = crossValidationPeriod) 
      
      #Prophet 
      model_fit_prophet <- prophet_reg() %>% 
        set_engine(engine = "prophet") %>% 
        fit(Value ~ Date, data = crossValidationPeriod)
      
      #SNaive
      model_fit_snaive <- naive_reg() %>%
        set_engine("snaive") %>%
        fit(Value ~ Date, data = crossValidationPeriod)
      
      #ARIMA
      model_fit_ARIMA <- arima_reg() %>%
        set_engine("auto_arima") %>%
        fit(Value ~ Date, data = crossValidationPeriod)
      
      #TBATS
      model_fit_TBATS <- seasonal_reg() %>%
        set_engine("tbats") %>%
        fit(Value ~ Date, data = crossValidationPeriod)
      
      #Nnetar
      model_fit_NNETAR <- nnetar_reg() %>%
        set_engine("nnetar") %>%
        fit(Value ~ Date, data = crossValidationPeriod)
      
      models_tbl <- modeltime_table(
        model_fit_naive,
        model_fit_snaive,
        model_fit_ARIMA,
        model_fit_TBATS,
        model_fit_NNETAR,
        model_fit_prophet,
        model_fit_lm
      )
      
      calibration_tbl <- models_tbl %>%
        modeltime_calibrate(new_data = crossValidationPeriod)
      
      calibration_tbl %>%
        modeltime_accuracy() %>%
        table_modeltime_accuracy()
      
      refit_tbl <- calibration_tbl %>%
        modeltime_refit(data = crossValidationPeriod)
      
      refit_tbl %>%
        modeltime_forecast(h = forecastPeriod, actual_data = crossValidationPeriod)
      
      finalForecast <- refit_tbl %>%
        modeltime_forecast(h = forecastPeriod, actual_data = crossValidationPeriod) %>%
        filter(.key == "prediction")
      
      finalResult <- subset(finalForecast, select = c(.model_desc, .index, .value))

      colnames(finalResult) <- c("For_Method", "Date", "Forecast")
      
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
      
      # i8_beg <- i7_end + 1
      # i8_end <- 8*forecastPeriod
      # 
      # i9_beg <- i8_end + 1
      # i9_end <- 9*forecastPeriod
      # 
      # i10_beg <- i9_end + 1
      # i10_end <- 10*forecastPeriod
      
      finalResult[1:forecastPeriod, 1] <- "Naive"
      finalResult[i2_beg:i2_end, 1] <- "SNaive"
      # finalResult[i3_beg:i3_end, 1] <- "MedF"
      # finalResult[i4_beg:i4_end, 1] <- "MeanF"
      # finalResult[i5_beg:i5_end, 1] <- "WAF"
      finalResult[i3_beg:i3_end, 1] <- "ARIMA"
      finalResult[i4_beg:i4_end, 1] <- "TBATS"
      finalResult[i5_beg:i5_end, 1] <- "NNETAR"
      finalResult[i6_beg:i6_end, 1] <- "PROPHET"
      finalResult[i7_beg:i7_end, 1] <- "LM"
    
      dfAux <- data.frame(Item = colnames(omniDataFiltered)[4], finalResult)
      colnames(dfAux) <- c("Item", "For_Method", "Date", "Forecast")
      
      if(cluster=="EMEA"){
        dfCrossValidationEMEA <- rbind(dfCrossValidationEMEA, dfAux)
      }else{
        dfCrossValidationNA <- rbind(dfCrossValidationNA, dfAux)
      }
    }
  }
}

View(finalResult)

Actuals1 <- omniData %>% select(Date, Cluster, Account, Value)
Actuals1 <- Actuals1 %>% filter(Cluster == 'EMEA')

Actuals1 <- reshape2::melt(Actuals1, id = c("Date", "Cluster", "Account"))
colnames(Actuals1)[4] <- "Item"
colnames(Actuals1)[5] <- "Actuals"

View(Actuals1)

dfCrossValidationEMEA$Date <- as.Date(dfCrossValidationEMEA$Date, origin = "1970-01-01")

View(dfCrossValidationEMEA)

resultAccuracy1 <-
  merge(dfCrossValidationEMEA,
        Actuals1,
        by = c("Date", "Item"),
        all.x = TRUE)


resultAccuracy1 <- resultAccuracy1 %>% drop_na(Actuals)
View(resultAccuracy1)

str(resultAccuracy1)
resultAccuracy1$Forecast <- as.numeric(resultAccuracy1$Forecast)

multiMetric1 <- metric_set(mae)

mdAcc1 <- resultAccuracy1 %>%
  group_by(Item, For_Method) %>%
  summarize_accuracy_metrics(Actuals, Forecast, metric_set = multiMetric1)

mdAcc1 <- mdAcc1 %>%
  tidyr::pivot_longer(cols = c(mae),
                      names_to = "variable",
                      values_to = "MAE")

DT1 <- data.table(mdAcc1)
View(DT1)

bm_final_results1 <-
  as.data.frame(DT1[, .SD[which.min(MAE)], by = c("Item")])
View(bm_final_results1)

bm_final_results1 <- merge(Actuals1, bm_final_results1, by = c("Item"))
View(bm_final_results1)

bm_final_results1 <- bm_final_results1[c(2, 1, 5, 6)]
View(bm_final_results1)

bestMethod1 <- bm_final_results1[1, 4]
View(bestMethod1)

bestMethodForecast1 <- dfCrossValidationEMEA %>%
  filter(For_Method == bestMethod1)

View(bestMethodForecast1)
View(dfCrossValidationEMEA)

Actuals2 <- omniData %>% select(Date, Cluster, Account, Value)
Actuals2 <- Actuals2 %>% filter(Cluster == 'NA')

Actuals2 <- reshape2::melt(Actuals2, id = c("Date", "Cluster", "Account"))
colnames(Actuals2)[4] <- "Item"
colnames(Actuals2)[5] <- "Actuals"

View(Actuals2)

dfCrossValidationNA$Date <- as.Date(dfCrossValidationNA$Date, origin = "1970-01-01")

View(dfCrossValidationNA)

resultAccuracy2 <-
  merge(dfCrossValidationNA,
        Actuals1,
        by = c("Date", "Item"),
        all.x = TRUE)


resultAccuracy2 <- resultAccuracy2 %>% drop_na(Actuals)
View(resultAccuracy2)

str(resultAccuracy2)
resultAccuracy2$Forecast <- as.numeric(resultAccuracy2$Forecast)

multiMetric2 <- metric_set(mae)

mdAcc2 <- resultAccuracy2 %>%
  group_by(Item, For_Method) %>%
  summarize_accuracy_metrics(Actuals, Forecast, metric_set = multiMetric1)

mdAcc2 <- mdAcc2 %>%
  tidyr::pivot_longer(cols = c(mae),
                      names_to = "variable",
                      values_to = "MAE")

DT2 <- data.table(mdAcc2)
View(DT2)

bm_final_results2 <-
  as.data.frame(DT2[, .SD[which.min(MAE)], by = c("Item")])
View(bm_final_results2)

bm_final_results2 <- merge(Actuals2, bm_final_results2, by = c("Item"))
View(bm_final_results2)

bm_final_results2 <- bm_final_results2[c(2, 1, 5, 6)]
View(bm_final_results1)

bestMethod2 <- bm_final_results2[1, 4]
View(bestMethod2)

bestMethodForecast2 <- dfCrossValidationNA %>%
  filter(For_Method == bestMethod2)

View(bestMethodForecast2)
View(dfCrossValidationNA)
