data = read.csv("DHS_Daily_Report.csv")

data$Date = strptime(data$Date, "%m/%d/%Y")
data$Date = format(data$Date, "%Y-%m-%d")
data$Date = as.Date(data$Date)

library(dplyr)
data = data %>% select(Date, Total.Individuals.in.Shelter, Easter, 
                       Thanksgiving, Christmas)

colnames(data)[2] <- "y"

future = subset(data, data$Date > "2020-11-11")
dataset = subset(data, data$Date <= "2020-11-11")



library(lubridate)
dataset$y <- ts(dataset$y, 
                frequency = 365, 
                start=c(2013,yday(head(dataset$Date,1))))

plot(dataset$y, ylab="Demand", xlab="Time")

# Seasonal Decomposition
decomposition = decompose(dataset$y, type="multiplicative")
plot(decomposition)


# other plots
# install.packages("forecast")
library(forecast)
ggseasonplot(dataset$y)












