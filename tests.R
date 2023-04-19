library(lubridate)
library(tidyr)
library(tidyverse)
library(readxl)
myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))
tabelEMEA <- data.frame(pivot_wider(omniData, names_from = Date, values_from = Value, names_prefix = "")) 

omniFiltered <- omniData %>% filter(year(Date) == 2020
)

graphDf <- omniFiltered %>%
  filter( Cluster == "EMEA") %>%
  select(Date, Account, Value)
View(omniFiltered)

View(graphDf)

# Create the plot
historyPlot <- graphDf %>%
  ggplot(aes(x=Date, y=Value, group=Account, color=Account)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ylab("Sales")+ 
  theme(
  legend.position = "bottom",  
  legend.box = "horizontal",   
  legend.margin = margin(t = 10))  

plot(historyPlot)

