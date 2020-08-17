#Calling all the needed packages
library(readxl)
library(ggplot2)
library(forecast)

#importing the COVID 19 Excel data
#covid = read_excel("C:\\Users\\User\\Desktop\\COVID 19 Data.xlsx")
covid_data = c(27,0,0,17,0,15,0,0,0,0,0,0,0,1,0,1,0,5,17,136,20,153,142,97,266,453,673,797,1767,
                  1480,1756,2003,2120,2608,2818,3243,3907,3751,3218,3442,2619,2988,2565,2072,15151,
                  4215,2560,2162,2067,1995,1864,532,996,1085,1008,527,793,863,1106,1264,1838,1821,
                  2044,1596,2413,2239,2856,3961,3691,3871,4537,4378,6887,8355,9375,8140,16051,12678,
                  14740,18314,29214,28742,34011,33016,39785,38927,51204,59718,63937,65162,58487,62444,
                  75403,77128,71813,81805,92598)
length_data = length(covid_data)
length_data

#Making the data into a timeseries
timeseries = ts(covid_data,start = c(1), end = c(length_data), frequency = 1)
plot(timeseries)

#Simple Exponential Smoothing Method
se_model = ses(timeseries, h = 6)
plot(se_model)
summary(se_model)
#The forecast show the same value becuase the alpha value is close to 1.

#Holt's Trend Method
holt_trend = holt(timeseries, h = 6)
plot(holt_trend)
summary(holt_trend)

#Holt-Winters' Exponetial Smoothing Forecasting
hw = HoltWinters(timeseries)
plot(hw)

hw_forecast = predict(hw,n.ahead = 6, prediction.interval = T, level = 0.95)
plot(hw,hw_forecast)
summary(hw_forecast)

#ARIMA Forcasting
Arima = auto.arima(timeseries)
plot(forecast(Arima, h = 6))
summary(Arima)

#TBATS Forcasting
tbats_model = tbats(timeseries)
plot(tbats_model)

tbats_forecast = forecast::forecast(tbats_model, h = 6)
plot(tbats_forecast)
summary(tbats_forecast)

#I ended up using the Holt-Winters' forcasting Method to forcast the COVID 19 cases.
#This is becuase it takes into account the trend, seasonality.
#Further more the other mthod stated that their MAPE is Infinite which is too high for data forcasting.
#I tried log the data and cutting the first partof the data but it didn't make a difference to the forecast.
#I chose the fit forcasting as I believe it is the best fit prediction for the COVID 19 cases.
forecast_covid = summary(hw_forecast)
forecast_covid
