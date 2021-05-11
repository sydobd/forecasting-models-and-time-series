#install.packages("TSA")
library(TSA)
data("airmiles")
airmiles
plot.ts(airmiles)

training_set = window(airmiles, end=c(2004,5))
test_set = window(airmiles, start=c(2004,6))

hw_model = HoltWinters(training_set, seasonal="multiplicative")

library(forecast)
prediction = forecast(hw_model, h = nrow(test_set))
plot(prediction, xlab="Time", ylab="airmiles", main="Holt Winters")

accuracy(prediction$mean, test_set)