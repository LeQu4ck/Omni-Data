library(lubridate)
myDataLocation <-"C:/Users/Userr/Downloads/Omni_Data.xlsx"

omniData <- read_excel(myDataLocation) %>% mutate(Date = as.Date(Date))

monthCol <- lubridate::month(omniData$Date, label = TRUE, abbr = FALSE, locale = "English")

print(monthCol)
