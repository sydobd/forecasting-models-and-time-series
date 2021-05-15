data = read.csv("Churrasco.csv")


colnames(data)[2] <- "y"

training_set = data[1:251,]
test_set = data[252:261,]

training_reg = as.matrix(training_set[,3:5])
test_reg = as.matrix(test_set[,3:5])

train_y = ts(data=training_set$y,
             start=c(2016,4),
             frequency = 365/7)#for weekly data frequency is 365/7, for start our data start year in 2016 and our first observation lies in fourth week
test_y = ts(data=test_set$y,
             start=c(2020,47),
             frequency = 365/7) #our value lies in 47th week
plot.ts(train_y)

library(forecast)
sarimax_model = auto.arima(train_y, xreg=training_reg)

summary(sarimax_model)

prediction_sarimax = forecast(sarimax_model, xreg=test_reg)

plot(prediction_sarimax, ylab="Demand", xlab="Time", main="SARIMAX")

accuracy(prediction_sarimax$mean, test_y)
