data = read.csv("DHS_Daily_Report.csv")

data$Date = strptime(data$Date, "%m/%d/%Y")
data$Date = format(data$Date, "%Y-%m-%d")
data$Date = as.Date(data$Date)

library(dplyr)
data = data %>% select(Date, Total.Individuals.in.Shelter, Easter, 
                       Thanksgiving, Christmas)

colnames(data)[2] <- 'y'

future = subset(data, data$Date > "2020-11-11")
dataset = subset(data, data$Date <= "2020-11-11")

library(lubridate)
dataset$y <- ts(dataset$y, frequency = 365,
                start = c(2013, yday(head(dataset$Date,1))))

test_set = subset(dataset, dataset$Date > "2020-09-30")
training_set = subset(dataset, dataset$Date <= "2020-09-30")

training_set$y <- ts(training_set$y, frequency = 365,
                     start=c(2013, yday(head(training_set$Date,1))))

####stationarity
#ndiffs gives the value of how many steps require make data stationary, 0 means data is stationary
library(forecast)
ndiffs(training_set$y,
      alpha = 0.05,
      test=c("adf")) #adf:augmented dickey fuller test

#extracting regressor (to feed into model)
training_reg = as.matrix(training_set[,3:5]) #regressor must be matrix form
test_reg = as.matrix(test_set[,3:5])

#SARIMAX model
sarimax_model = auto.arima(training_set$y, xreg=training_reg)
