library(dplyr)
library(forecast)
library(tidymodels)
library(modeltime)
library(prophet)
library(reshape2)
library(data.table)
library(readxl)

# Assume your data is named 'myData' and has columns 'Date', 'Cluster', 'Account', and 'Value'
# You should also set 'forecastCrossValidation' and 'forecastPeriod' values before running the script

forecastPeriod <- 12
forecastCrossValidation <- 5

dfCrossValidation1 <-
  data.frame(
    Item = character(),
    For_Method = character(),
    Date = character(),
    Forecast = numeric()
  )

myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))

omniData$Date <- as.Date(omniData$Date, origin = "1970-01-01")

# Create an empty data frame to store cross-validation results
dfCrossValidation1 <- data.frame()

for (cluster in c("NA")) {
  for (account in c("OCOS", "SG&A")) {
    
    cat("\nProcessing Cluster:", cluster, "Account:", account)
    
    omniDataFiltered <- subset(omniData, Cluster == cluster & Account == account)
    
    startCrossValidation <- nrow(omniDataFiltered) - forecastCrossValidation
    
    for (j in 1:forecastCrossValidation) {
      
      crossValidationPeriod <- omniDataFiltered[1:(startCrossValidation + j), ]
      
      model_fit_naive <- naive_reg() %>%
        set_engine("naive") %>%
        fit(Value ~ Date, data = crossValidationPeriod)
      
      models_tbl <- modeltime_table(
        model_fit_naive
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
      

      dfAux <- data.frame(Item = colnames(omniDataFiltered)[4], finalResult)
      colnames(dfAux) <- c("Item", "For_Method", "Date", "Forecast")
      
      dfCrossValidation1 <- rbind(dfCrossValidation1, dfAux)
    }
  }
}

Actuals1 <- omniData %>% select(Date, Cluster, Account, Value)

Actuals1 <- reshape2::melt(Actuals1, id = c("Date", "Cluster", "Account"))
colnames(Actuals1)[4] <- "Item"
colnames(Actuals1)[5] <- "Actuals"

dfCrossValidation1$Date <- as.Date(dfCrossValidation1$Date, origin = "1970-01-01")

resultAccuracy1 <-
  merge(dfCrossValidation1,
        Actuals1,
        by = c("Date", "Item"),
        all.x = TRUE)


resultAccuracy1 <- resultAccuracy1 %>% drop_na(Actuals)

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

bestMethodForecast1 <- dfCrossValidation1 %>%
  filter(For_Method == bestMethod1)
View(bestMethodForecast1)
View(dfCrossValidation1)
