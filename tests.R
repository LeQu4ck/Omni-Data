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

graphAllYears <- omniData %>% filter(Account == "Gross Trade Sales")%>% 
  select(Date, Account, Cluster, Value)

View(graphAllYears)

View(graphDf)

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

