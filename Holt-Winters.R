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

#####################################################
# Holt-Winters
# breaking into test set and training set
test_set = subset(dataset, dataset$Date > "2020-09-30")
training_set = subset(dataset, dataset$Date <= "2020-09-30")

# transform y series of training set into timeseries
training_set$y <-  ts(training_set$y,
                    frequency = 365,
                    start=c(2013,yday(head(training_set$Date,1))))

# holt-winters model
hw_model = HoltWinters(training_set$y, 
                       seasonal = "multiplicative")

#holt-winter prediction
library(forecast)
hw_prediction = forecast(hw_model,
                         h=nrow(test_set)) # h is length of forecast i.e. rows of test set

#only predictions of forecast, previous gives values at different level of confidence
hw_prediction$mean

#visualization
plot(hw_prediction,  ylab="Demand", xlab="Time", main="Holt-Winters")

#measuring accuracy of the model
accuracy(hw_prediction$mean, test_set$y)

####applying model to whole data
hw_model2 = HoltWinters(dataset$y, seasonal = "multiplicative")
hw_future = forecast(hw_model2, h=nrow(future))
plot(hw_future,  ylab="Demand", xlab="Time", main="Holt-Winters")


##writing csvs of our models
write.csv(hw_prediction$mean, 
          file='8.MyEnsemble/Forecast/hw_prediction.csv',
          row.names = FALSE)
write.csv(hw_future$mean,
          file="8.MyEnsemble/Future/hw_future.csv",
          row.names = FALSE)
