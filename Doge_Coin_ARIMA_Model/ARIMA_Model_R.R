# Load necessary libraries
library(forecast)

# Load the dataset
data <- read.csv("C:\\Users\\loges\\OneDrive\\Desktop\\ECON 5336\\Project\\DOGE-USD.csv")

# Convert date column to Date format
data$date <- as.Date(data$Date, format="%m/%d/%Y")

# Create a time series object
ts_data <- ts(data[, "Close"], start=c(2017, 11), end=c(2023, 5), frequency=365)

# Perform ARIMA modeling and forecasting
fit <- auto.arima(ts_data)
forecast <- forecast(fit, h=7)

# Print the forecasted values
print(forecast)
plot(forecast,main="Closing Price for next 7 days")
predict(forecast)
plot(predict(forecast))
print(forecast)
plot(forecast, main = "ARIMA Forecast for Dogecoin", xlim = c(max(time(forecast$mean))-2, max(time(forecast$mean))))


#Forecast for volume
# Check for stationarity using Augmented Dickey-Fuller Test
adf.test(df$Volume)
# If p-value > 0.05, perform transformations to make the series stationary

# Create time series object
# Create a time series object for the 'Volume' column
ts_volume <- ts(df$Volume, start = c(2017, 311), end = c(2023, 93), frequency = 365)
ts_cls <- ts(df$Close, start = c(2017, 311), end = c(2023, 93), frequency = 365)
autoplot(ts_cls)+ggtitle("Daily Adjusted Closing Price of DOGE Coin From 2017-2023")
adf.test(ts_cls)
cls_diff=diff(ts_cls)
adf.test(cls_diff)
acf(cls_diff,main="ACF Plot")
pacf(cls_diff,main="PACF Plot")
autoplot(cls_diff)+ggtitle("DOGE Coin Close Price Differenced to Order 1")
autoplot(ts_volume)+ggtitle("DOGE Coin Market Volume From 2017 to 2023")
# Split data into train and test sets
train_data <- window(ts_cls, start = c(2017, 311), end = c(2022, 365))
test_data <- window(ts_cls, start = c(2023, 1), end = c(2023, 93))
#ARima for cls
arima_cls=auto.arima(cls_diff)
arima_cls
m1=arima(cls_diff,order=c(4,1,5))
m2=arima(cls_diff,order=c(5,1,5))
m3=arima(cls_diff,order=c(5,1,4))
m1
accuracy(m1)
accuracy(m2)
accuracy(m3)
m2
m3
# Build ARIMA model
arima_volume <- auto.arima(train_data)

# Forecast for next 30 days
forecast_vol <- forecast(arima_volume, h = 7)
forecast_vol
# Print the forecasted values
print(forecast_vol)

# Plot the forecast
plot(forecast_vol, main = "ARIMA Forecast for Volume of Dogecoin for Next 7 Days")

#accuracy
accuracy(forecast_vol, test_data)

# Plot the forecast and actual values
plot(forecast_vol, main = "Actual Vs. Predicted Dogecoin Price for Next 7 Days")
lines(test_data, col = "red")
legend("topright", legend = c("Forecast", "Actual"), col = c("black", "red"), lty = 1)

