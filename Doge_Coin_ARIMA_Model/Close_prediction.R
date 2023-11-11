library(readxl)
library(fpp2)
library(lubridate)
library(xts)
library(forecast)
library(urca)
library(tseries)
#Importing the CSV file
doge_coin=read.csv("C:\\Users\\loges\\OneDrive\\Desktop\\ECON 5336\\Project\\DOGE-USD.csv")


#Extracting variables
doge_open=doge_coin$Open
doge_close=doge_coin$Close
doge_high=doge_coin$High
doge_low=doge_coin$Low
doge_adj=doge_coin$Adj.Close
volume=doge_coin$Volume
doge_coin$Date=as.Date(doge_coin$Date)

close_ts=ts(doge_close,frequency=1)
close_ts
autoplot(close_ts)
#df_close=data.frame(x=doge_coin$Date, y=close_ts)         
adf.test(close_ts)
close_d1=diff(close_ts)
adf.test(close_d1)
autoplot(close_d1)+ggtitle("DOGE Coin Close Price Differenced to Order 1")
acf(close_d1)
pacf(close_d1)
######
ts_cls <- ts(data[, "Close"], start=c(2017, 11), end=c(2023, 5), frequency=365)
adf.test(ts_cls)
autoplot(ts_cls)
#order of difference is 1 we have to try different models
model1<-arima(close_d1,order=c(1,1,2))
model2<-arima(close_d1,order=c(2,1,1))
model3<-arima(close_d1,order=c(2,1,2))
print(model1);print(model2);print(model3)

#Based on AIC ARIMA(2,1,2) is the best model
model3
forecast(model3,h=7)
close_pred=forecast(model3,h=7)
checkresiduals(close_d1)
#Fit
fit<-predict(model3, n.ahead = 7)
y<-fit$pred
fit1<-data.frame(y)
fit1         

#Predict
h=0.075525

for (i in 2:nrow(fit1))
{
  dy1=fit1$y[1]+0.075525

  dy2=fit1$y[2]+dy1
  dy3=fit1$y[3]+dy2
  dy4=fit1$y[4]+dy3
  dy5=fit1$y[5]+dy4
  dy6=fit1$y[6]+dy5
  dy7=fit1$y[7]+dy6
}
Day_1=dy1
Day_2<-dy2
Day_3=dy3
Day_4<-dy4
Day_5=dy5
Day_6<-dy6
Day_7=dy7
Predict<-rbind(Day_1,Day_2,Day_3,Day_4,Day_5,Day_6,Day_7)
colnames(Predict)=c("USD")
Predict
plot(Predict)
y=c(6,7,8,9,10,11,12)
plot(y, Predict, type = "l", main = "Predicted Price From Mar 6 to 12 (in USD)", xlab = "X-axis label", ylab = "Y-axis label")

#Prediction comparision
actual=read.csv("C:\\Users\\loges\\OneDrive\\Desktop\\ECON 5336\\Project\\Doge_Actual.csv")
actual_close=actual$Close
accuracy_df <- data.frame(Date = time(actual), Actual = actual, Forecast = forecast$mean)
accuracy_df
accuracy(close_pred)
plot(y,actual_close,type='l')

auto_close1=auto.arima(close_ts)
auto_close1
auto_fore=forecast(auto_close1,h=7)
plot(auto_fore)
accuracy(Predict,actual_close)
