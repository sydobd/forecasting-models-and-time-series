#getting data
data = read.csv("DHS_Daily_Report.csv")

#transforming data variable format to date
data$Date = strptime(data$Date, "%m/%d/%Y") #convert char to date
data$Date = format(data$Date, "%Y-%m-%d")   #convert into to date format
data$Date = as.Date(data$Date)              #convert

#forming dataset, extracting useful variables
#install.packages("dplyr") install this package before running below code
library(dplyr)
data = data %>% select(Date, Total.Individuals.in.Shelter, Easter, 
                       Thanksgiving, Christmas)

#renaming forecasting variable to y
colnames(data)[2] <- "y"  #converting second col var name to y, <- is equal to

#create two subset from our dataset
future = subset(data, data$Date > '2020-11-11')
dataset = subset(data, data$Date <= '2020-11-11')

#transform y to time series
install.packages("lubridate")
library(lubridate)
dataset$y <- ts(dataset$y,
                frequency = 365,
                start=c(2013, yday(head(dataset$Date,1))))

#Visualization
plot(dataset$y, ylab="Demand", xlab="Time")
