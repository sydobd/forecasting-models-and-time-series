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

#getting values of non sesoanal and seasonal p,d,q
summary(sarimax_model)

#comparing model with test set
#forecasting
library(forecast)
predictions_sarimax = forecast(sarimax_model, xreg = test_reg)

#visualization
plot(predictions_sarimax, ylab="Demand", xlab="Time", main="SARIMAX")

#accuracy
accuracy(predictions_sarimax$mean, test_set$y)

##########################applying to whole data

#extracting regressor 
dataset_reg = as.matrix(dataset[,3:5])
future_red = as.matrix(future[,3:5])

#SARIMAX model
sarimax_model2 = auto.arima(dataset$y, xreg=dataset_reg)


#getting values of non seasonal and seasonal p,d,q
summary(sarimax_model2)

#forecasting
future_sarimax = forecast(sarimax_model2, xreg=future_reg)

#visualization
plot(future_sarimax, ylab="Demand", xlab="Time", main="SARIMAX")

##writing csvs of our models
write.csv(predictions_sarimax$mean, 
          file='8.MyEnsemble/Forecast/hw_prediction.csv',
          row.names = FALSE)
write.csv(future_sarimax$mean,
          file="8.MyEnsemble/Future/hw_future.csv",
          row.names = FALSE)

